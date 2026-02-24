#' Get a Quarto parameter value
#'
#' Returns a value from `params` when available, otherwise evaluates an
#' optional default function.
#'
#' @param name Character scalar. Name of the parameter to retrieve.
#' @param default_fun Optional function used to compute a default value when
#'   the parameter is missing or `NULL`.
#' @param ... Additional arguments passed to `default_fun`.
#'
#' @return The parameter value if found and not `NULL`; otherwise the return
#'   value from `default_fun(...)` when provided; otherwise `NULL`.
#' @export
#' @examples
#' get_param("missing")
#' get_param("x", function() 42)
get_param <- function(name, default_fun = NULL, ...) {
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.null(default_fun) || is.function(default_fun))
    if (exists("params", inherits = TRUE)) {
        params_obj <- get("params", inherits = TRUE)
        if (!is.null(params_obj[[name]])) {
            return(params_obj[[name]])
        }
    }
    if (!is.null(default_fun)) {
        return(default_fun(...))
    }

    NULL
}


#' Read data from a file path
#'
#' Reads `.rds` or `.csv` files using a path relative to the project root.
#' Optionally converts the result to a tibble when the `tibble` package is
#' available.
#'
#' @param path Character scalar. Relative path to the data file.
#' @param as_tibble Logical scalar. Whether to coerce output to tibble when
#'   possible. Defaults to `TRUE`.
#' @param ... Additional arguments passed to [base::readRDS()] or
#'   [utils::read.csv()].
#'
#' @return A data object read from disk, optionally converted to a tibble.
#' @export
#' @examples
#' \dontrun{
#' read_data("data/example.rds")
#' read_data("data/example.csv", stringsAsFactors = FALSE)
#' }
read_data <- function(path, as_tibble = TRUE, ...) {
    stopifnot(is.character(path), length(path) == 1)
    path <- here::here(path)
    stopifnot(file.exists(path))
    stopifnot(is.logical(as_tibble), length(as_tibble) == 1)

    ext <- tolower(tools::file_ext(path))

    data <- switch(ext,
        "rds" = readRDS(path, ...),
        "csv" = utils::read.csv(path, ...),
        stop("Unsupported file type: ", ext)
    )

    if (as_tibble && requireNamespace("tibble", quietly = TRUE)) {
        data <- tibble::as_tibble(data)
    }

    data
}


# copy_quarto_site <- function(
#     source_dir = "_site",
#     target_dir,
#     overwrite = TRUE,
#     create_parent = TRUE,
#     verbose = TRUE
# ) {
#     # ---- checks ----
#     if (missing(target_dir) || is.null(target_dir)) {
#         stop("`target_dir` must be provided.")
#     }

#     if (!dir.exists(source_dir)) {
#         stop(sprintf("Source directory does not exist: %s", source_dir))
#     }

#     # normalize paths
#     source_dir <- fs::path_norm(source_dir)
#     target_dir <- fs::path_norm(target_dir)

#     # prevent dangerous copy (copying into itself)
#     if (fs::path_has_parent(target_dir, source_dir)) {
#         stop("Target directory cannot be inside source directory.")
#     }

#     # create parent directory if needed
#     parent_dir <- fs::path_dir(target_dir)
#     if (!dir.exists(parent_dir)) {
#         if (create_parent) {
#             if (verbose) message("Creating parent directory: ", parent_dir)
#             fs::dir_create(parent_dir, recurse = TRUE)
#         } else {
#             stop("Parent directory does not exist: ", parent_dir)
#         }
#     }

#     # remove existing target
#     if (dir.exists(target_dir)) {
#         if (overwrite) {
#             if (verbose) message("Removing existing target: ", target_dir)
#             fs::dir_delete(target_dir)
#         } else {
#             stop("Target already exists. Set `overwrite = TRUE` to replace it.")
#         }
#     }

#     # ---- copy ----
#     if (verbose) {
#         message("Copying Quarto site:")
#         message("  from: ", source_dir)
#         message("  to:   ", target_dir)
#     }

#     fs::dir_copy(source_dir, target_dir)

#     if (verbose) message("Done ✔")

#     return(invisible(target_dir))
# }
