# Build and deploy project pipeline

This function builds the targets pipeline using \`\_targets.R\`, renders
the pipeline Quarto document, and deploys the resulting HTML to the
workspace folder. It provides error handling and informative messages
for each step.

## Usage

``` r
build_project(pipeline_only = FALSE)
```

## Arguments

- pipeline_only:

  Logical. If \`TRUE\`, skip \`targets::tar_make()\` and only
  render/deploy \`pipeline_targets.qmd\`.

## Value

Invisible NULL. Side effects: builds pipeline, renders Quarto, copies
HTML to workspace.

## Examples

``` r
if (FALSE) { # \dontrun{
build_project()
} # }
```
