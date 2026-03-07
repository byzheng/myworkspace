# Visualizing Target Pipelines with Mermaid

## Introduction

The
[`knit_targets_mermaid()`](https://myworkspace.bangyou.me/reference/knit_targets_mermaid.md)
function provides a powerful way to visualize your
[`targets`](https://docs.ropensci.org/targets/) pipeline as an
interactive [Mermaid](https://mermaid-js.github.io/mermaid/#/) diagram.
This vignette demonstrates how to use it in R Markdown or Quarto,
including zooming, panning, and rich tooltips with markdown support.

## Basic Usage

Create a chunk in your R Markdown or Quarto document and call
[`knit_targets_mermaid()`](https://myworkspace.bangyou.me/reference/knit_targets_mermaid.md)
with chunk options `results = "asis"` to render the diagram directly in
the document:

``` r
library(myworkspace)
# Basic pipeline
knit_targets_mermaid(targets_only = TRUE)
```

Comparing with the default
[`targets::tar_mermaid()`](https://docs.ropensci.org/targets/reference/tar_mermaid.html),
the following items are added:

- Change from horizontal (left-to-right) to vertical (top-to-bottom)
  layout.
- Add config to reduce the space between nodes and edges.
- Add tooltips that support markdown formatting.

## Customizing the Diagram

You can pass arguments to both
[`targets::tar_mermaid()`](https://docs.ropensci.org/targets/reference/tar_mermaid.html)
and
[`targets::tar_manifest()`](https://docs.ropensci.org/targets/reference/tar_manifest.html)
via `...`. For example, to use a custom pipeline script:

``` r
knit_targets_mermaid(script = "my_targets.R", targets_only = TRUE)
```

## Tooltips with Markdown

If you add a `description` to your targets, it will appear as a tooltip
when you hover over the node. Descriptions are parsed as markdown (if
the `markdown` package is installed) and rendered as HTML, so you can
use formatting, links, and more:

``` r
# In your _targets.R
list(
  tar_target(x, 1 + 1, description = "**Bold** _italic_ [link](https://r-project.org)"),
  tar_target(y, x * 2, description = "`y` is double x.\n- Bullet 1\n- Bullet 2")
)
```

## Example Output

Below is an example of a rendered diagram with tooltips:

``` r
script_file <- system.file("examples", "_targets.R", package = "myworkspace")
myworkspace::knit_targets_mermaid(script = script_file, targets_only = TRUE)
```

``` mermaid
%%{init: {
  "flowchart": {
    "nodeSpacing": 15,
    "rankSpacing": 20
  }
}}%%

graph LR
  style Legend fill:#FFFFFF00,stroke:#000000;
  style Graph fill:#FFFFFF00,stroke:#000000;
  subgraph Legend
    x2db1ec7a48f65a9b(["Outdated"]):::outdated
    xd03d7c7dd2ddda2b(["Regular target"]):::none
  end
  subgraph Graph
    direction TB
    x8e231551575db7a7(["raw_data"]):::outdated --> xc20eecc8ddbacc81(["clean_data"]):::outdated
    xc20eecc8ddbacc81(["clean_data"]):::outdated --> xedf5078b482d9f1e(["exploratory_plot"]):::outdated
    xc68371ecb8ec0be6(["model_fit"]):::outdated --> xf1e89fab85cd3b9b(["model_coefficients"]):::outdated
    xc20eecc8ddbacc81(["clean_data"]):::outdated --> xc68371ecb8ec0be6(["model_fit"]):::outdated
    x934c693ac98559cf(["model_formula"]):::outdated --> xc68371ecb8ec0be6(["model_fit"]):::outdated
    x3f329484235e9aee(["model_predictions"]):::outdated --> xfcaa52d2a8af57ca(["model_performance"]):::outdated
    xc20eecc8ddbacc81(["clean_data"]):::outdated --> x3f329484235e9aee(["model_predictions"]):::outdated
    xc68371ecb8ec0be6(["model_fit"]):::outdated --> x3f329484235e9aee(["model_predictions"]):::outdated
    x3f329484235e9aee(["model_predictions"]):::outdated --> xc0bc3e535892f95c(["performance_plot"]):::outdated
    xf1e89fab85cd3b9b(["model_coefficients"]):::outdated --> xbc8681067d9c1c8c(["report_summary"]):::outdated
    xfcaa52d2a8af57ca(["model_performance"]):::outdated --> xbc8681067d9c1c8c(["report_summary"]):::outdated
    xc20eecc8ddbacc81(["clean_data"]):::outdated --> xe968619b50acd55c(["site_summary"]):::outdated

    click x8e231551575db7a7 callback "Simulated raw field experiment dataset with site, rainfall, temperature and yield"
    click xc20eecc8ddbacc81 callback "Data cleaning step removing impossible values and missing observations"
    click xedf5078b482d9f1e callback "Exploratory scatter plot showing rainfall–yield relationship"
    click xc68371ecb8ec0be6 callback "Fitted linear regression model estimating climate effects on yield"
    click xf1e89fab85cd3b9b callback "Extracted regression coefficients with standard errors and p-values"
    click x934c693ac98559cf callback "Linear regression model specification for yield prediction"
    click x3f329484235e9aee callback "Model predictions of yield for each observation"
    click xfcaa52d2a8af57ca callback "Model evaluation metrics including RMSE and R-squared"
    click xc0bc3e535892f95c callback "Predicted vs observed yield plot to evaluate model performance"
    click xbc8681067d9c1c8c callback "Final analysis summary combining model coefficients and performance metrics"
    click xe968619b50acd55c callback "Site-level summary statistics used for exploratory analysis"
  end
  classDef outdated stroke:#000000,color:#000000,fill:#78B7C5;
  classDef none stroke:#000000,color:#000000,fill:#94a4ac;
```

Hover over the nodes to see the markdown-formatted tooltips. You can
zoom and pan the diagram with your mouse.
