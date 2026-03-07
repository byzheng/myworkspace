# Get the current project name

Infers the project name by calculating the relative path from workspace
root to project root. Useful for automatically determining which project
is being worked on.

## Usage

``` r
get_prj_name()
```

## Value

Character string with the project name (e.g., "A" for \`projects/A\`),
or \`NULL\` if no project is found or project is at workspace root.

## Details

This function finds the workspace and project roots, then extracts the
project identifier from the relative path. For a standard layout with
\`projects/A\`, returns "A".
