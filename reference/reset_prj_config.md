# Reset the project configuration cache

This function clears the cached project configuration, forcing it to be
re-read from the \`\_project.yml\` file on the next call to
\`get_prj_config()\`. Useful for testing or when the configuration file
has been updated.

## Usage

``` r
reset_prj_config()
```
