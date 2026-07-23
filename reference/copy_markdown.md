# Copy Markdown Files to a Destination Directory

Recursively copies markdown files from a source directory to a
destination directory while skipping excluded directories such as build
outputs or git metadata. Existing files are only overwritten when
requested.

## Usage

``` r
copy_markdown(
  src = ".",
  dst,
  overwrite = TRUE,
  exclude = c("_site", ".quarto", ".git", ".targets", "_freeze", ".Rproj.user",
    ".vscode")
)
```

## Arguments

- src:

  Character scalar. Source directory to scan for markdown files.
  Defaults to the current working directory.

- dst:

  Character scalar. Destination directory where markdown files will be
  copied.

- overwrite:

  Logical scalar. Whether to overwrite existing files in the destination
  when the source file is newer. Defaults to TRUE.

- exclude:

  Character vector. Directory names to skip when scanning for markdown
  files. Defaults to common build and editor directories.

## Value

Invisible character vector of relative paths copied to the destination
directory.

## Examples

``` r
if (FALSE) { # \dontrun{
copy_markdown(src = "./docs", dst = "./site-docs")
} # }
```
