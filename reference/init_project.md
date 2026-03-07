# Initialize Project Structure

Creates a standardized project folder structure with index.qmd files.
The structure includes: - script/: code - source/: raw input data -
derived/: intermediate/computed results - output/: final outputs -
story/: analysis narratives & reports - story/source/: source
documentation - story/output/: output documentation

## Usage

``` r
init_project(name, root = ".")
```

## Arguments

- name:

  Character. Name of the project, used in titles and links.

- root:

  Character. Path to the project root directory. Default is the current
  directory (".").

## Value

Invisible NULL. Creates directories and index.qmd files for their side
effects.

## Details

Index.qmd files are created in the root, script/, story/source/, and
story/output/ folders. The root index.qmd includes links to these three
main sections.

## Examples

``` r
if (FALSE) { # \dontrun{
# Initialize project structure in current directory
init_project("MyAnalysis")

# Initialize in a specific directory
init_project("MyAnalysis", "path/to/project")
} # }
```
