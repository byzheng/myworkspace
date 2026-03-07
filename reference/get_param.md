# Get a Quarto parameter value

Returns a value from \`params\` when available, otherwise evaluates an
optional default function.

## Usage

``` r
get_param(name, default_fun = NULL, ...)
```

## Arguments

- name:

  Character scalar. Name of the parameter to retrieve.

- default_fun:

  Optional function used to compute a default value when the parameter
  is missing or \`NULL\`.

- ...:

  Additional arguments passed to \`default_fun\`.

## Value

The parameter value if found and not \`NULL\`; otherwise the return
value from \`default_fun(...)\` when provided; otherwise \`NULL\`.

## Examples

``` r
get_param("missing")
#> NULL
get_param("x", function() 42)
#> [1] 42
```
