# Copies the contents of a Quarto site's output directory (default '\_site') to a specified target directory. Handles overwriting, parent directory creation, and prevents copying into itself.

Copies the contents of a Quarto site's output directory (default
'\_site') to a specified target directory. Handles overwriting, parent
directory creation, and prevents copying into itself.

## Usage

``` r
copy_quarto_site(
  source_dir = "_site",
  target_dir = NULL,
  overwrite = TRUE,
  create_parent = TRUE,
  verbose = TRUE
)
```

## Arguments

- source_dir:

  Character scalar. Source directory to copy from (default '\_site').

- target_dir:

  Character scalar. Target directory to copy to. Must be provided. If
  null (default), it will attempt to copy to the workspace folder
  determined by \`find_ws()\` and project name from \`get_prj_name()\`.

- overwrite:

  Logical scalar. If TRUE, overwrites existing target directory (default
  TRUE).

- create_parent:

  Logical scalar. If TRUE, creates parent directory if it does not exist
  (default TRUE).

- verbose:

  Logical scalar. If TRUE, prints progress messages (default TRUE).

## Value

Invisibly returns the target directory path, or \`NULL\` if default
\`target_dir\` cannot be determined.

## Examples

``` r
if (FALSE) { # \dontrun{
copy_quarto_site(source_dir = "_site", target_dir = "docs")
} # }
```
