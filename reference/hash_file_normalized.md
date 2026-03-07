# Cross-Platform File Hashing with EOL Normalization

Computes a hash of a file, normalizing end-of-line characters for text
files to ensure consistent hashes across Windows, Linux, and macOS.

## Usage

``` r
hash_file_normalized(
  file,
  algo = "sha256",
  text_extensions = c(".txt", ".csv", ".tsv", ".md", ".json", ".r", ".R", ".yaml",
    ".yml", ".qmd", ".Rmd", ".rmd")
)
```

## Arguments

- file:

  Path to the file to hash

- algo:

  Hash algorithm to use (default: "sha256")

- text_extensions:

  Character vector of file extensions to treat as text (default includes
  common text formats)

## Value

Hash string (character)

## Details

For text files (by extension), reads the file as text, replaces all line
endings with \n, and hashes the resulting string. For binary files,
hashes the raw file bytes.
