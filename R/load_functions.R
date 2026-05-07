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
    functions_files <- character(0)
    tryCatch({
        config <- read_prj_config()
        
        if (!is.null(config) && !is.null(config$functions) && length(config$functions) > 0) {
            functions_files <- config$functions
        }
        if (is.null(functions_files) || length(functions_files) == 0) {
            warning("No function files specified in _project.yml. Using default directories.")
        }
        for (i in seq_along(functions_files)) {
            file_path <- path_prj(functions_files[i])
            if (!file.exists(file_path)) {
                warning("Specified function file does not exist: ", file_path)
                next
            }
            source(file_path, local = FALSE)
        }
    }, error = function(e) {
        warning(e)
    }, warning = function(w) {
        warning(w)
    })
    return(invisible())
}
