# About project managements


#' Source R Functions from Directory
#'
#' Sources R files listed in `_project.yml` and from fallback folders.
#'
#' @param folders Character vector. Paths to the directories containing R files relative to project root. 
#'   Default is c("Rcode/function",  "Rcode/00_function", "script/function", "script/00_function").
#' @return Invisible NULL. The function sources the R files for their side effects.
#' @export
#' @examples
#' \dontrun{
#' # Source all R files from default directory
#' load_functions()
#' }
load_functions <- function(folders = c(
            "Rcode/function",
            "Rcode/00_function", 
            "script/function",
            "script/00_function")) {
    config <- read_prj_config()
    functions_files <- character(0)
    if (!is.null(config) && !is.null(config$functions) && length(config$functions) > 0) {
        functions_files <- config$functions
    }
    if (is.null(functions_files) || length(functions_files) == 0) {
        warning("No function files specified in _project.yml. Using default directories.")
    }
    folder_path <- here::here(folders)
    files2 <- list.files(folder_path, pattern = "\\.(R|r)$", 
        full.names = TRUE, 
        recursive = TRUE)
    functions_files <- c(functions_files, files2)
    for (i in seq_along(functions_files)) {
        file_path <- here::here(functions_files[i])
        if (!file.exists(file_path)) {
            warning("Specified function file does not exist: ", file_path)
            next
        }
        source(file_path, local = FALSE)
    }
    return(invisible())
}
