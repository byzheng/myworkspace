##' Render targets pipeline as a vertical mermaid diagram for knitr
##'
##' This function generates a mermaid diagram from a targets pipeline, modifies the direction from left-to-right (LR) to top-to-bottom (TB), and returns it as knitr asis output for embedding in R Markdown or Quarto documents.
##'
##' The diagram includes a config block (see the top of the output) that sets node and rank spacing for improved layout. The function also injects custom CSS and JS for zooming, panning, and interactive tooltips.
##'
##' If a target in the pipeline has a `description` field in the manifest, it will be shown as a tooltip when hovering over the node. Descriptions are parsed as markdown (if the `markdown` package is installed) and rendered as HTML, so you can use formatting, links, and other markdown features in your tooltips.
##'
##' @param ... Arguments passed to `targets::tar_mermaid()` and `targets::tar_manifest()`. Shared arguments (e.g., `script`) are passed to both; unique arguments (e.g., `targets_only`) are passed only to the appropriate function.
##' @return A character string formatted for knitr asis output containing the mermaid diagram with interactive tooltips.
##' @examples
##' # In an R Markdown code chunk:
##' \dontrun{
##' knit_targets_mermaid(targets_only = TRUE)
##' # You can use markdown in target descriptions:
##' # tar_target(x, 1, description = "**Bold** [link](https://r-project.org)")
##' }
##'
##' @export
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
    # Split ... into args for tar_manifest and tar_mermaid

    dots <- list(...)
    manifest_formals <- names(formals(targets::tar_manifest))
    manifest_args <- dots[names(dots) %in% manifest_formals]
    mermaid_formals <- names(formals(targets::tar_mermaid))
    mermaid_args <- dots[names(dots) %in% mermaid_formals]
    
        tmp_env <- new.env()
    manifest_msg <- capture.output({
        manifest_val <- do.call(targets::tar_manifest, manifest_args)
        assign("manifest_val", manifest_val, envir = tmp_env)
    })
    manifest <- get("manifest_val", envir = tmp_env)
    mermaid_msg <- capture.output({
        mermaid_val <- do.call(targets::tar_mermaid, mermaid_args)
        assign("mermaid_val", mermaid_val, envir = tmp_env)
    })
    mermaid_code <- get("mermaid_val", envir = tmp_env)

    if (length(manifest_msg) > 0) {
        message("tar_manifest messages:\n", paste(manifest_msg, collapse = "\n"))
    }
    if (length(mermaid_msg) > 0) {
        message("tar_mermaid messages:\n", paste(mermaid_msg, collapse = "\n"))
    }


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
                                desc_md <- manifest$description[idx]
                                # Parse markdown to HTML (remove <p> wrappers for inline)
                                if (requireNamespace("markdown", quietly = TRUE)) {
                                    desc_html <- markdown::markdownToHTML(text = desc_md, fragment.only = TRUE)
                                    desc_html <- gsub('^<p>(.*)</p>\r?\n?$', '\\1', desc_html)
                                } else {
                                    desc_html <- desc_md
                                }
                                desc_html <- gsub('"', '\\"', desc_html)
                                click_rows <- c(click_rows, sprintf('    click %s callback "%s"', node_id, desc_html))
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

