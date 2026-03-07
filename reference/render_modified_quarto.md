# Render only modified Quarto files

Uses content hashes for files matched by \`\_quarto.yml\` \`render:\`
patterns and renders only files whose content changed since the last
successful run.

## Usage

``` r
render_modified_quarto(
  quarto_yml = "_quarto.yml",
  root_dir = here::here(),
  cache_file = ".quarto/render-hashes.json",
  dry_run = FALSE,
  force = FALSE,
  ...
)
```

## Arguments

- quarto_yml:

  Character scalar. Path to \`\_quarto.yml\`, relative to \`root_dir\`
  or absolute.

- root_dir:

  Character scalar. Project root used to resolve relative paths.
  Defaults to \`here::here()\`.

- cache_file:

  Character scalar. Path to JSON hash cache file, relative to
  \`root_dir\` or absolute.

- dry_run:

  Logical scalar. If \`TRUE\`, returns files that would be rendered
  without invoking Quarto.

- force:

  Logical scalar. If \`TRUE\`, renders all matched files.

- ...:

  Additional arguments passed to \`quarto::quarto_render()\`

## Value

Character vector of relative files selected for rendering.

## Examples

``` r
if (FALSE) { # \dontrun{
render_modified_quarto()
render_modified_quarto(dry_run = TRUE)
} # }
```
