# Source the nearest \`.Rprofile\` from a directory tree

Starting from \`start\`, this function searches for a \`.Rprofile\` file
in the current directory and then each parent directory until
\`stop_at\` (inclusive) or the filesystem root is reached. The first
\`.Rprofile\` found is sourced and the search stops immediately. The
file is sourced with \`chdir = TRUE\`, so relative file paths in
\`.Rprofile\` are resolved from the \`.Rprofile\` folder. Objects
created by \`.Rprofile\` are assigned into the caller environment.

## Usage

``` r
source_rprofile(start = getwd(), stop_at = "/")
```

## Arguments

- start:

  Character scalar. Directory where the search begins. Defaults to
  \[base::getwd()\].

- stop_at:

  Character scalar. Directory at which upward searching stops. Defaults
  to \`"/"\`.

## Value

Invisibly returns the path to the sourced \`.Rprofile\` file, or
\`NULL\` if no \`.Rprofile\` is found. Side effects from \`.Rprofile\`
are applied in the caller environment.
