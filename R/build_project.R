
#' Build and deploy project pipeline
#'
#' This function builds the targets pipeline using `_targets.R`, renders the pipeline Quarto document,
#' and deploys the resulting HTML to the workspace folder. It provides error handling and informative messages
#' for each step.
#' @param pipeline_only Logical. If `TRUE`, skip `targets::tar_make()` and only render/deploy `pipeline_targets.qmd`.
#'
#' @return Invisible NULL. Side effects: builds pipeline, renders Quarto, copies HTML to workspace.
#' @examples
#' \dontrun{
#' build_project()
#' }
#' @export
build_project <- function(pipeline_only = FALSE) {
    stopifnot(is.logical(pipeline_only), length(pipeline_only) == 1)
    message("Starting project build process...")
    prj_root <- find_prj()
    message("Project root found at: ", prj_root)
    withr::with_dir(prj_root, {
        target_file <- file.path("_targets.R")
        if (!file.exists(target_file)) {
            stop("No _targets.R file found in the project root directory.")
        }

        if (!pipeline_only) {
            # Build targets pipeline with error handling
            message("Building targets pipeline...")
            error_tar_make <- try({
                targets::tar_make(script = target_file)
            }, silent = TRUE)
            if (inherits(error_tar_make, "try-error")) {
                message("Error during targets::tar_make(). Please check the error messages above for details.")
                message("Error message: \n", attr(error_tar_make, "condition")$message)
            }
        }
        # deploy pipeline
        message("Rendering pipeline_targets.qmd with Quarto...")
        pip_file <- "pipeline_targets.qmd"
        if (!file.exists(pip_file)) {
            message("No pipeline_targets.qmd file found in the project root directory.")
            return(invisible())
        }
        error_quarto_pip <- try({
            quarto::quarto_render("pipeline_targets.qmd", quiet = TRUE)
        }, silent = TRUE)

        if (inherits(error_quarto_pip, "try-error")) {
            message("Error during quarto::quarto_render(). Please check the error messages above for details.")
            message("Error message: \n", attr(error_quarto_pip, "condition")$message)
        }
        # deploy to workspace folder
        message("Deploying rendered pipeline_targets.html to workspace folder...")
        error_prj_name <- try({
            prj_name <- get_prj_name()
        }, silent = TRUE)
        if (inherits(error_prj_name, "try-error")) {
            message("Could not determine project name. Please ensure the project is properly set up.")
            return(invisible())
        }
        source_file <- "_site/pipeline_targets.html"
        if (!file.exists(source_file)) {
            message("Rendered pipeline_targets.html not found. Please check if the quarto render step was successful.")
            return(invisible())
        }
        ws_root <- find_ws()
        html_pip_file <- file.path(ws_root, "_site", prj_name, "pipeline_targets.html")
        if (!dir.exists(dirname(html_pip_file))) {
            dir.create(dirname(html_pip_file), recursive = TRUE)
        }
        file.copy(
            source_file, 
            html_pip_file,
            overwrite = TRUE)
        message("Pipeline deployed successfully to: ", html_pip_file)
    }) 
    message("Project build process completed.")
    return(invisible())
}
