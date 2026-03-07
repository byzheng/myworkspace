# Try to Read a targets Target, or Run a Fallback Expression

Attempts to read a computed target from a targets pipeline using
\`targets::tar_read()\`. If the target cannot be read (e.g., it hasn't
been computed yet), runs a fallback expression instead. Useful in
interactive workflows where a target may not always be available.

## Usage

``` r
tar_or_run(target, expr, quiet = FALSE, store = here::here("_targets"), ...)
```

## Arguments

- target:

  Unquoted name of the target to read (not a string).

- expr:

  Expression to evaluate if target read fails. Will be evaluated lazily
  only if needed.

- quiet:

  Logical. If \`FALSE\` (default), print messages explaining why the
  target read failed and that the fallback is running. If \`TRUE\`,
  suppress these messages.

- store:

  Character path to the targets store. Defaults to
  \`here::here("\_targets")\`.

- ...:

  Additional arguments passed to \`targets::tar_read()\` (except
  \`store\`).

## Value

The value of the read target if successful, otherwise the result of
evaluating \`expr\`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Try to read a computed target, fall back to a simple value
tar_or_run(my_data, data.frame(x = 1:5))

# Suppress messages when using interactively
tar_or_run(results, compute_results(), quiet = TRUE)
} # }
```
