# Build a path relative to the workspace root

Resolves path components against the workspace root discovered by
\[find_ws()\].

## Usage

``` r
path_ws(...)
```

## Arguments

- ...:

  Path components passed to \[base::file.path()\].

## Value

A path string relative to \[base::getwd()\] when possible.
