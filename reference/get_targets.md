# Load and Combine targets Objects

Recursively finds and sources target definition scripts, then collects
all objects in scope with names beginning with \`\_targets\_\` and
combines them into a single vector or list using \`c()\`.

## Usage

``` r
get_targets(
  path = c("script", "source", "story"),
  pattern = "^_targets_.*\\.R$"
)
```

## Arguments

- path:

  Character vector of directories to search for target scripts. Defaults
  to \`c("script", "source", "story")\`.

- pattern:

  Character scalar regular expression used to match target script file
  names. Defaults to \`"^\_targets\_.\*\\R\$"\`.

## Value

A combined object created from all objects whose names match
\`"^targets\_"\` after sourcing matched files, plus any tar_target
objects created as the final expression in sourced files (even if not
assigned).

## Details

All matched \`targets\_\*\` objects must be either:

- A single object inheriting from class \`"tar_target"\`, OR

- A list where all elements inherit from class \`"tar_target"\`

Alternatively, a file can contain a bare tar_target call (e.g.,
\`targets::tar_target(alpha, 1)\`) as its final expression, which will
be captured and named after its internal target name. The function
validates this after each sourced file and errors with file context if
any matched object does not meet these criteria. It also errors on
duplicate internal target names. If sourcing a matched file fails, the
function rethrows with the source file path.

## Examples

``` r
if (FALSE) { # \dontrun{
# Single tar_target per variable
dir.create("script")
writeLines(
  "targets_alpha <- targets::tar_target(alpha, 1)",
  "script/_targets_alpha.R"
)

# List of tar_targets per variable
writeLines(
  "targets_multi <- list(targets::tar_target(beta, 2), targets::tar_target(gamma, 3))",
  "script/_targets_multi.R"
)

get_targets(path = "script")
} # }
```
