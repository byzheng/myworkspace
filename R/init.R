
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
#' init("MyAnalysis")
#' 
#' # Initialize in a specific directory
#' init("MyAnalysis", "path/to/project")
#' }
init <- function(name, root = here::here()) {
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