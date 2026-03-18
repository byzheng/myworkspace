# List files targeted by \`\_quarto.yml\` \`render:\` patterns

Reads the \`render:\` section from a Quarto project file and resolves it
to a concrete file list.

## Usage

``` r
list_quarto_render_files(
  quarto_yml = "_quarto.yml",
  root_dir = here::here(),
  exclude_targets_qmd = TRUE
)
```

## Arguments

- quarto_yml:

  Character scalar. Path to \`\_quarto.yml\`, relative to \`root_dir\`
  or absolute.

- root_dir:

  Character scalar. Project root used to resolve relative paths.
  Defaults to \`here::here()\`.

- exclude_targets_qmd:

  Logical scalar. If \`TRUE\`, excludes \`.qmd\` files discovered in
  \`targets::tar_manifest()\` \`command\` entries when those files exist
  and are part of the resolved render list.

## Value

Character vector of relative file paths matched by \`render:\` patterns.

## Examples

``` r
if (FALSE) { # \dontrun{
list_quarto_render_files()
} # }
```
