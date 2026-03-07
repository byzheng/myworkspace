# Render targets pipeline as a vertical mermaid diagram for knitr

This function generates a mermaid diagram from a targets pipeline,
modifies the direction from left-to-right (LR) to top-to-bottom (TB),
and returns it as knitr asis output for embedding in R Markdown or
Quarto documents.

## Usage

``` r
knit_targets_mermaid(...)
```

## Arguments

- ...:

  Arguments passed to \`targets::tar_mermaid()\` and
  \`targets::tar_manifest()\`. Shared arguments (e.g., \`script\`) are
  passed to both; unique arguments (e.g., \`targets_only\`) are passed
  only to the appropriate function.

## Value

A character string formatted for knitr asis output containing the
mermaid diagram with interactive tooltips.

## Details

The diagram includes a config block (see the top of the output) that
sets node and rank spacing for improved layout. The function also
injects custom CSS and JS for zooming, panning, and interactive
tooltips.

If a target in the pipeline has a \`description\` field in the manifest,
it will be shown as a tooltip when hovering over the node. Descriptions
are parsed as markdown (if the \`markdown\` package is installed) and
rendered as HTML, so you can use formatting, links, and other markdown
features in your tooltips.

## Examples

``` r
# In an R Markdown code chunk:
if (FALSE) { # \dontrun{
knit_targets_mermaid(targets_only = TRUE)
# You can use markdown in target descriptions:
# tar_target(x, 1, description = "**Bold** [link](https://r-project.org)")
} # }
```
