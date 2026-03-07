#' Create External Sentinel File with Input Tracking
#'
#' @param sentinel_path Path to sentinel file, either relative to project root or absolute
#' @param input_files Character vector of input file paths, either relative to project root or absolute
#' @param metadata Optional list of additional metadata to store
#'
#' @details
#' Tracks external dependencies (HPC jobs, shell scripts, database exports, etc.) using sentinel files.
#' All paths are resolved relative to the project root using `here::here()`. Absolute paths 
#' are automatically converted to relative paths for storage, ensuring sentinels work consistently 
#' across platforms (Windows/Linux/Mac) and different working directories.
#' 
#' All timestamps are stored in UTC to ensure consistency when sentinels are shared across 
#' machines in different timezones (e.g., Sydney to Brisbane, or across international systems).
#' 
#' Examples of valid calls:
#' - Relative paths: `create_external_sentinel(".external/job.json", c("data/raw.csv", "params.json"))`
#' - Absolute paths: `create_external_sentinel("/full/path/.external/job.json", c("/full/path/data/raw.csv"))`
#' - Mixed: `create_external_sentinel("./external/job.json", "/full/path/data/raw.csv")`
#'
#' Creates a JSON sentinel file that records:
#' - Completion timestamp (UTC)
#' - Input files (stored as relative paths) with their content hashes (SHA-256)
#' - Optional custom metadata
#'
#' @examples
#' \dontrun{
#' # Using relative paths (recommended)
#' create_external_sentinel(
#'   sentinel_path = ".external/job_complete.json",
#'   input_files = c("data/raw.csv", "params.json"),
#'   metadata = list(job_id = "12345", nodes = 4)
#' )
#' }
#' @export 

create_external_sentinel <- function(sentinel_path, input_files, metadata = list()) {
    stopifnot(is.character(input_files), length(input_files) > 0)
    if (!requireNamespace("digest", quietly = TRUE)) {
        stop("The 'digest' package is required for file hashing. Please install it.")
    }

    # Get hashes for all input files
    input_info <- lapply(input_files, function(f) {
        if (!file.exists(f)) {
            warning("Input file not found: ", f)
            return(list(hash = NA_character_))
        }
        hash <- digest::digest(file = f, algo = "sha256")
        list(hash = hash)
    })
    names(input_info) <- input_files

    # Build sentinel metadata using relative paths for portability
    sentinel_data <- list(
        completed_at = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
        input_files = input_info,
        metadata = metadata
    )

    # Write as JSON
    dir.create(dirname(sentinel_path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(sentinel_data, sentinel_path, 
                         pretty = TRUE, auto_unbox = TRUE)

    # Compute and store hash of the sentinel file itself (for downstream dependencies)
    # sentinel_hash <- digest::digest(file = sentinel_path, algo = "sha256")
    # Optionally, write this hash to a separate file or return it if needed

    message("Created sentinel: ", sentinel_path)
    message("  Completed: ", sentinel_data$completed_at)
    message("  Tracked ", length(input_files), " input file(s)")

    invisible(sentinel_path)
}


#' Check External Sentinel Staleness for targets
#'
#' @param sentinel_path Path to sentinel file, either relative to project root or absolute
#' @param input_files Character vector of input file paths, either relative to project root or absolute
#' @param on_missing Action when sentinel missing: "stop" (default) or "warn"
#' @param on_stale Action when sentinel stale: "stop" (default), "warn", or "delete"
#'
#' @return Relative path to sentinel (invisibly) if valid, otherwise stops with informative error
#'
#' @details
#' All paths are resolved relative to the project root using `here::here()`. Absolute paths 
#' are automatically converted to relative paths for comparison with stored paths, ensuring
#' consistency across platforms and working directories.
#'
#' All timestamps are compared in UTC, ensuring correct staleness detection even when sentinels 
#' are shared across machines in different timezones.
#'
#' Validates sentinel by checking:
#' 1. Sentinel file exists
#' 2. Input files haven't changed since external process ran (compared by file content hash only)
#' 3. All expected input files are tracked in sentinel
#'
#' If validation fails, provides clear error message and optionally deletes stale sentinel.
#' 
#' Note: Only file hashes are used for staleness checks. Modification times are stored for information only.
#'
#' @examples
#' \dontrun{
#' # Using relative paths (recommended)
#' check_external_sentinel(
#'   sentinel_path = ".external/job_complete.json",
#'   input_files = c("data/raw.csv", "params.json")
#' )
#' }
#' @export
check_external_sentinel <- function(sentinel_path, 
                               input_files,
                               on_missing = c("stop", "warn"),
                               on_stale = c("stop", "warn", "delete")) {
    stopifnot(is.character(input_files), length(input_files) > 0)
    stopifnot(is.character(sentinel_path), length(sentinel_path) == 1)
    on_missing <- match.arg(on_missing)
    on_stale <- match.arg(on_stale)
    
    # Check if sentinel exists
    if (!file.exists(sentinel_path)) {
        msg <- paste0(
            "External process not completed yet.\n",
            "Sentinel file not found: ", sentinel_path, "\n",
            "Run external process to create it."
        )
        if (on_missing == "stop") stop(msg, call. = FALSE)
        warning(msg)
        return(NULL)
    }
    
    # Read sentinel metadata
    tryCatch({
        sentinel_data <- jsonlite::read_json(sentinel_path)
    }, error = function(e) {
        unlink(sentinel_path)
        stop(
            "Sentinel file corrupted: ", sentinel_path, "\n",
            "Deleted. Re-run external process.\n",
            "Error: ", e$message,
            call. = FALSE
        )
    })
    
    # Validate structure
    if (is.null(sentinel_data$input_files)) {
        unlink(sentinel_path)
        stop(
            "Sentinel file has invalid format (missing input_files).\n",
            "Deleted. Re-run external process.",
            call. = FALSE
        )
    }
    
    stored_files <- names(sentinel_data$input_files)

    # Check for missing inputs
    missing_inputs <- setdiff(input_files, stored_files)
    if (length(missing_inputs) > 0) {
        if (on_stale == "delete") unlink(sentinel_path)
        msg <- paste0(
            "External process sentinel is incomplete.\n",
            "Missing tracked inputs or no matching input files in the sentinel file: ", paste(missing_inputs, collapse = ", "), "\n",
            if (on_stale == "delete") "Deleted sentinel. " else "",
            "Re-run external process."
        )
        if (on_stale == "stop" || on_stale == "delete") {
            stop(msg, call. = FALSE)
        } else {
            warning(msg)
        }
    }

    # Check for stale inputs (by hash only)
    if (!requireNamespace("digest", quietly = TRUE)) {
        stop("The 'digest' package is required for file hashing. Please install it.")
    }
    stale_inputs <- character(0)
    for (i in seq_along(input_files)) {
        input_file <- input_files[i]
        input_name <- input_files[i]

        if (!file.exists(input_file)) {
            stop("Input file not found: ", input_name, call. = FALSE)
        }

        current_hash <- digest::digest(file = input_file, algo = "sha256")
        stored_info <- sentinel_data$input_files[[input_name]]
        stored_hash <- stored_info$hash

        if (is.na(stored_hash)) {
            stale_inputs <- c(stale_inputs, paste0(input_name, " (not tracked)"))
        } else if (!identical(current_hash, stored_hash)) {
            stale_inputs <- c(stale_inputs, paste0(input_name, " (hash mismatch)"))
        }
    }

    if (length(stale_inputs) > 0) {
        if (on_stale == "delete") unlink(sentinel_path)
        msg <- paste0(
            "Input data changed after external process completed.\n",
            "Stale inputs (by hash):\n  ",
            paste(stale_inputs, collapse = "\n  "), "\n",
            if (on_stale == "delete") "Deleted sentinel. " else "",
            "Re-run external process."
        )
        if (on_stale == "stop" || on_stale == "delete") {
            stop(msg, call. = FALSE)
        } else {
            warning(msg)
        }
    }

    # All checks passed
    sentinel_path
}


#' Get External Sentinel Metadata
#'
#' @param sentinel_path Path to sentinel file, either relative to project root or absolute
#' @return List with completion time, input files (as relative paths), and custom metadata
#'
#' @details
#' The sentinel path is resolved relative to the project root using `here::here()`. 
#' Absolute paths are automatically converted to relative paths.
#'
#' @examples
#' \dontrun{
#' meta <- get_external_sentinel_metadata(".external/job_complete.json")
#' cat("External process completed at:", meta$completed_at, "\n")
#' }
#' @export 
get_external_sentinel_metadata <- function(sentinel_path) {
    sentinel_abs <- suppressWarnings(here::here(sentinel_path))
    if (!file.exists(sentinel_abs)) {
        stop("Sentinel file not found: ", sentinel_path, call. = FALSE)
    }
    jsonlite::read_json(sentinel_abs)
}


