# List content hashes for files targeted by \`\_quarto.yml\` \`render:\` patterns

Resolves \`\_quarto.yml\` \`render:\` patterns to files and returns a
named character vector of MD5 hashes, where names are relative file
paths.

## Usage

``` r
list_quarto_render_hashes(quarto_yml = "_quarto.yml", root_dir = here::here())
```

## Arguments

- quarto_yml:

  Character scalar. Path to \`\_quarto.yml\`, relative to \`root_dir\`
  or absolute.

- root_dir:

  Character scalar. Project root used to resolve relative paths.
  Defaults to \`here::here()\`.

## Value

Named character vector of MD5 hashes. Names are relative file paths.

## Examples

``` r
if (FALSE) { # \dontrun{
list_quarto_render_hashes()
} # }
```
