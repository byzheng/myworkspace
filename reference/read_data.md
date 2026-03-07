# Read data from a file path

Reads \`.rds\` or \`.csv\` files using a path relative to the project
root. Optionally converts the result to a tibble when the \`tibble\`
package is available.

## Usage

``` r
read_data(path, as_tibble = TRUE, ...)
```

## Arguments

- path:

  Character scalar. Relative path to the data file.

- as_tibble:

  Logical scalar. Whether to coerce output to tibble when possible.
  Defaults to \`TRUE\`.

- ...:

  Additional arguments passed to \[base::readRDS()\] or
  \[utils::read.csv()\].

## Value

A data object read from disk, optionally converted to a tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
read_data("data/example.rds")
read_data("data/example.csv", stringsAsFactors = FALSE)
} # }
```
