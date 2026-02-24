#' Load and Combine targets Objects
#'
#' Recursively finds and sources target definition scripts, then collects all
#' objects in scope with names beginning with `_targets_` and combines them into
#' a single vector or list using `c()`.
#'
#' @param path Character vector of directories to search for target scripts.
#'   Defaults to `c("script", "source", "story")`.
#' @param pattern Character scalar regular expression used to match target
#'   script file names. Defaults to `"^_targets_.*\\.R$"`.
#'
#' @return A combined object created from all objects whose names match
#'   `"^targets_"` after sourcing matched files.
#' @details
#' All matched `targets_*` objects must inherit from class `"tar_target"`.
#' The function validates this after each sourced file and errors with file
#' context if any matched object does not. It also errors on duplicate
#' internal target names. If sourcing a matched file fails, the function
#' rethrows with the source file path.
#' @export
#' @examples
#' \dontrun{
#' dir.create("script")
#' writeLines("targets_example <- list(a = 1)", "script/_targets_example.R")
#' get_targets(path = "script")
#' }
get_targets <- function(
    path = c("script", "source", "story"),
    pattern = "^_targets_.*\\.R$") {
    stopifnot(is.character(path), length(path) > 0)
    stopifnot(is.character(pattern), length(pattern) == 1)

    target_env <- new.env(parent = baseenv())

    target_files <- sort(
        list.files(
            path = path,
            pattern = pattern,
            recursive = TRUE,
            full.names = TRUE
        )
    )
    if (length(target_files) == 0) {
        message("No target files found in specified paths.")
        return(NULL)
    }
    for (target_file in target_files) {
        file_env <- new.env(parent = target_env)
        source_error <- tryCatch(
            {
                source(target_file, local = file_env)
                NULL
            },
            error = function(error_condition) {
                error_condition
            }
        )

        if (!is.null(source_error)) {
            stop(
                sprintf(
                    "Failed to source target file %s: %s",
                    target_file,
                    conditionMessage(source_error)
                ),
                call. = FALSE
            )
        }

        file_object_names <- ls(envir = file_env)
        file_target_names <- sort(ls(envir = file_env, pattern = "^targets_"))

        if (length(file_target_names) == 0) {
            list2env(mget(file_object_names, envir = file_env, inherits = FALSE), envir = target_env)
            next
        }

        existing_target_names <- ls(envir = target_env, pattern = "^targets_")

        file_target_objects <- mget(
            file_target_names,
            envir = file_env,
            inherits = FALSE
        )
        is_tar_target <- vapply(
            file_target_objects,
            inherits,
            logical(1),
            what = "tar_target"
        )

        if (any(!is_tar_target)) {
            bad_names <- names(file_target_objects)[!is_tar_target]
            stop(
                sprintf(
                    paste0(
                        "All matched objects must inherit from 'tar_target'. ",
                        "Invalid objects after sourcing %s: %s"
                    ),
                    target_file,
                    paste(bad_names, collapse = ", ")
                ),
                call. = FALSE
            )
        }

        existing_target_objects <- mget(
            existing_target_names,
            envir = target_env,
            inherits = FALSE,
            ifnotfound = list(NULL)
        )
        existing_target_objects <- existing_target_objects[!vapply(existing_target_objects, is.null, logical(1))]

        existing_internal_names <- vapply(existing_target_objects, function(target_object) target_object$name, character(1))
        file_internal_names <- vapply(file_target_objects, function(target_object) target_object$name, character(1))

        duplicated_internal_names <- intersect(file_internal_names, existing_internal_names)
        if (length(duplicated_internal_names) > 0) {
            stop(
                sprintf(
                    paste0(
                        "Duplicate internal target names after sourcing %s: %s"
                    ),
                    target_file,
                    paste(duplicated_internal_names, collapse = ", ")
                ),
                call. = FALSE
            )
        }

        list2env(mget(file_object_names, envir = file_env, inherits = FALSE), envir = target_env)
    }

    target_object_names <- sort(ls(envir = target_env, pattern = "^targets_"))

    if (length(target_object_names) == 0) {
        return(NULL)
    }

    target_objects <- mget(target_object_names, envir = target_env, inherits = FALSE)

    do.call(c, target_objects)
}