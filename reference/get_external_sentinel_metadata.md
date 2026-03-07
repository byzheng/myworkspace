# Get External Sentinel Metadata

Get External Sentinel Metadata

## Usage

``` r
get_external_sentinel_metadata(sentinel_path)
```

## Arguments

- sentinel_path:

  Path to sentinel file, either relative to project root or absolute

## Value

List with completion time, input files (as relative paths), and custom
metadata

## Details

The sentinel path is resolved relative to the project root using
\`here::here()\`. Absolute paths are automatically converted to relative
paths.

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- get_external_sentinel_metadata(".external/job_complete.json")
cat("External process completed at:", meta$completed_at, "\n")
} # }
```
