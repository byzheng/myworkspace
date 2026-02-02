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
#' project_fun()
#' }
project_fun <- function(folders = c(
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

#' Initialize Project Structure
#'
#' Creates a standardized project folder structure with index.qmd files.
#' The structure includes:
#' - script/: code
#' - source/: raw input data
#' - derived/: intermediate/computed results
#' - output/: final outputs
#' - story/: analysis narratives & reports
#'   - story/source/: source documentation
#'   - story/output/: output documentation
#'
#' Index.qmd files are created in the root, script/, story/source/, and story/output/ folders.
#' The root index.qmd includes links to these three main sections.
#'
#' @param name Character. Name of the project, used in titles and links.
#' @param root Character. Path to the project root directory. Default is the current project root (here::here()).
#' @return Invisible NULL. Creates directories and index.qmd files for their side effects.
#' @export
#' @examples
#' \dontrun{
#' # Initialize project structure in current directory
#' init_project("MyAnalysis")
#' 
#' # Initialize in a specific directory
#' init_project("MyAnalysis", "path/to/project")
#' }
init_project <- function(name, root = here::here()) {
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.character(root), length(root) == 1)
    stopifnot(dir.exists(root))
    # Define folder structure
    folders <- c("script", "source", "derived", "output", "story", 
                file.path("story", "source"),
                file.path("story", "output"))
    
    # Create directories
    for (folder in folders) {
        folder_path <- file.path(root, folder)
        if (!dir.exists(folder_path)) {
            dir.create(folder_path, recursive = TRUE)
            message("Created directory: ", folder_path)
        } else {
            message("Directory already exists: ", folder_path)
        }
    }
    
    # Create index.qmd in root
    root_index <- file.path(root, "index.qmd")
    if (!file.exists(root_index)) {
        writeLines(c(
            "---",
            sprintf("title: \"%s Project Overview\"", name),
            "---",
            "",
            "## Overview",
            "",
            "",
            "## Sources",
            "",
            sprintf("[Sources](story/source/index.qmd) for %s", name),
            "",
            "## Scripts",
            "",
            sprintf("[Scripts](script/index.qmd) for %s", name),
            "",
            "## Outputs",
            "",
            sprintf("[Outputs](story/output/index.qmd) for %s", name),
            ""
        ), root_index)
        message("Created: index.qmd in root")
    } else {
        message("File already exists: index.qmd in root")
    }
    # Customize content based on folder purpose
    folders_index <- c("script", "story/source", "story/output")
    folder_descriptions <- list(
        script = c("---", "title: \"Scripts\"", "---", ""),
        story_source = c("---", "title: \"Sources\"", "---", ""),
        story_output = c("---", "title: \"Outputs\"", "---", "")
    )
    
    # Create index.qmd in each folder
    for (i in seq(along = folders_index)) {
        index_path <- file.path(root, folders_index[i], "index.qmd")
        if (!file.exists(index_path)) {
            writeLines(folder_descriptions[[i]], index_path)
            message("Created: ", index_path)
        } else {
            message("File already exists: ", index_path)
        }
    }
    
    message("\nProject structure initialized successfully!")
    return(invisible())
}