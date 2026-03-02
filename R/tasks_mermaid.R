#' Read an asset file from the package inst/assets folder
#'
#' This helper function loads the contents of a file from the package's inst/assets directory.
#' It is used to include external CSS or JS in knitr output.
#'
#' @param filename The name of the asset file (e.g., "mermaid-zoom.css").
#' @return A character string containing the file contents.
#' @examples
#' # Read CSS asset
#' read_asset_file("mermaid-zoom.css")
read_asset_file <- function(filename) {
    asset_path <- system.file("assets", filename, package = "myworkspace")
    if (asset_path == "") stop(paste("Asset not found:", filename))
    paste(readLines(asset_path, warn = FALSE), collapse = "\n")
}
#' Render targets pipeline as a vertical mermaid diagram for knitr
#'
#' This function generates a mermaid diagram from a targets pipeline, modifies the direction from left-to-right (LR) to top-to-bottom (TB), and returns it as knitr asis output for embedding in R Markdown or Quarto documents.
#'
#' @param ... Arguments passed to `targets::tar_mermaid()`.
#' @return A character string formatted for knitr asis output containing the mermaid diagram.
#' @examples
#' # In an R Markdown code chunk:
#' \dontrun{
#' knit_targets_mermaid(targets_only = TRUE)
#' }
#'
#' @export
knit_targets_mermaid <- function(...) {
    mermaid_code <- targets::tar_mermaid(...)
    pos <- grepl("direction LR", mermaid_code)
    mermaid_code[pos] <- gsub("direction LR", "direction TB", mermaid_code[pos])
    mermaid_code <- paste(mermaid_code, collapse = "\n")
    css <- paste0("<style>\n", read_asset_file("mermaid-zoom.css"), "\n</style>")
    js  <- paste0("<script>\n", read_asset_file("mermaid-zoom.js"), "\n</script>")
    knitr::asis_output(
        paste0(
            css, "\n",
            js, "\n",
            "```{mermaid}\n",
            mermaid_code,
            "\n```")
    )
}

