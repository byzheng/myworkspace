# Build a path relative to the project root

Resolves the root directory differently depending on execution context:
interactive sessions use the active editor file (falling back to
\[base::getwd()\]), knitr uses the current input document, and
non-interactive runs use \`PROJECT_DIR\` when available, otherwise the
current project (or workspace when no project is found).

## Usage

``` r
path_prj(...)
```

## Arguments

- ...:

  Path components passed to \[base::file.path()\].

## Value

A path string relative to \[base::getwd()\] when possible.
