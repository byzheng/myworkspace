# About project managements


#' Source R Functions from Directory
#'
#' This function sources all R files from a specified directory and its subdirectories into global environment.
#'
#' @param folders Character vector. Paths to the directories containing R files relative to project root. 
#'   Default is c("Rcode/function",  "Rcode/00_function", "script/function", "script/00_function",  "../000-CommonFiles/script/00_function").
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
            "script/00_function",
            "../000-CommonFiles/script/00_function")) {
    folder_path <- here::here(folders)
    a <- list.files(folder_path, pattern = "\\.(R|r)$", 
        full.names = TRUE, 
        recursive = TRUE) |>
        sapply(source)
    return(invisible())
}
