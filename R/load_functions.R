# About project managements


#' Source R Functions from Directory
#'
#' Sources R files listed in `_project.yml` and from fallback folders.
#'
#' @return Invisible NULL. The function sources the R files for their side effects.
#' @export
#' @examples
#' \dontrun{
#' # Source all R files from default directory
#' load_functions()
#' }
load_functions <- function() {
    config <- tryCatch(
        read_prj_config(),
        error = function(e) {
            warning("Failed to read project configuration: ", conditionMessage(e))
            NULL
        }
    )
    if (is.null(config)) {
        warning("No project configuration found. Please create a _project.yml file.")
        return(invisible())
    }
    if (is.null(config$functions) || length(config$functions) == 0) {
        warning("No function files specified in _project.yml. Please add a 'functions' section.")
        return(invisible())
    }
    functions_files <- config$functions
    for (i in seq_along(functions_files)) {
        file_path <- path_prj(functions_files[i])
        if (!file.exists(file_path)) {
            warning("Specified function file does not exist: ", file_path)
            next
        }
        source(file_path, local = FALSE)
    }
    return(invisible())
}
