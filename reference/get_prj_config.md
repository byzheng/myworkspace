# Get a project configuration value

Retrieves a specific configuration value from the project's
\`\_project.yml\` file. Supports nested keys using dot notation (e.g.,
"start.year").

## Usage

``` r
get_prj_config(name)
```

## Arguments

- name:

  The name of the configuration value to retrieve. Can be a nested key
  using dot notation.

## Value

The value of the specified configuration key
