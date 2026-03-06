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
    config <- c(
        '%%{init: {',
        '  "flowchart": {',
        '    "nodeSpacing": 15,',
        '    "rankSpacing": 20',
        '  }',
        '}}%%',
        ''
    )
    manifest <- targets::tar_manifest()
    mermaid_code <- targets::tar_mermaid(...)
    pos <- grepl("direction LR", mermaid_code)
    mermaid_code[pos] <- gsub("direction LR", "direction TB", mermaid_code[pos])
    
    graph_start <- seq_len(length(mermaid_code))[pos]
    graph_end <- grep('^\\s*end\\s*$', mermaid_code)
    graph_end <- graph_end[graph_end > graph_start]

    # Add description as click callbacks if available
    if (!is.null(manifest[["description"]])) {
        # Only process lines after the legend
        process_idx <- seq(graph_start + 1, graph_end - 1)
        node_lines <- grep("\\([\"']?([^)]+)[\"']?\\)", mermaid_code[process_idx], value = TRUE)
        node_matches <- regmatches(node_lines, gregexpr("([a-zA-Z0-9_]+)\\([\"']?([^)]+)[\"']?\\)", node_lines))
        node_matches <- unique(unlist(node_matches))
        click_rows <- character()
        i <- 1
        for (i in seq_along(node_matches)) {
            node_id <- sub("\\(.*", "", node_matches[i])
            node_label <- sub(".*\\([\"']?([^)]+)[\"']?\\)", "\\1", node_matches[i])
            node_label <- gsub('\\[|\\]|"', "", node_label)
            
            idx <- which(manifest$name == node_label)
            if (length(idx) == 1 && !is.na(manifest$description[idx])) {
                desc <- gsub('"', '\\"', manifest$description[idx])
                click_rows <- c(click_rows, sprintf('    click %s callback "%s"', node_id, desc))
            }
        }
        mermaid_code <- append(mermaid_code, click_rows, after = graph_end - 1)
    }
    mermaid_code <- paste(c(config, mermaid_code), collapse = "\n")
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

