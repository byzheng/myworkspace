# Check External Sentinel Staleness for targets

Check External Sentinel Staleness for targets

## Usage

``` r
check_external_sentinel(
  sentinel_path,
  input_files,
  on_missing = c("stop", "warn"),
  on_stale = c("stop", "warn", "delete")
)
```

## Arguments

- sentinel_path:

  Path to sentinel file, either relative to project root or absolute

- input_files:

  Character vector of input file paths, either relative to project root
  or absolute

- on_missing:

  Action when sentinel missing: "stop" (default) or "warn"

- on_stale:

  Action when sentinel stale: "stop" (default), "warn", or "delete"

## Value

Relative path to sentinel (invisibly) if valid, otherwise stops with
informative error

## Details

All paths are resolved relative to the project root using
\`here::here()\`. Absolute paths are automatically converted to relative
paths for comparison with stored paths, ensuring consistency across
platforms and working directories.

All timestamps are compared in UTC, ensuring correct staleness detection
even when sentinels are shared across machines in different timezones.

Validates sentinel by checking: 1. Sentinel file exists 2. Input files
haven't changed since external process ran (compared by file content
hash only) 3. All expected input files are tracked in sentinel

If validation fails, provides clear error message and optionally deletes
stale sentinel.

Note: Only file hashes are used for staleness checks. Modification times
are stored for information only.

## Examples

``` r
if (FALSE) { # \dontrun{
# Using relative paths (recommended)
check_external_sentinel(
  sentinel_path = ".external/job_complete.json",
  input_files = c("data/raw.csv", "params.json")
)
} # }
```
