# Source R Functions from Directory

Sources R files listed in \`\_project.yml\` and from fallback folders.

## Usage

``` r
load_functions(
  folders = c("Rcode/function", "Rcode/00_function", "script/function",
    "script/00_function")
)
```

## Arguments

- folders:

  Character vector. Paths to the directories containing R files relative
  to project root. Default is c("Rcode/function", "Rcode/00_function",
  "script/function", "script/00_function").

## Value

Invisible NULL. The function sources the R files for their side effects.

## Examples

``` r
if (FALSE) { # \dontrun{
# Source all R files from default directory
load_functions()
} # }
```
