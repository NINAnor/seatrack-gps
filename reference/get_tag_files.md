# Get all files for a specific tag

Get all files for a specific tag

## Usage

``` r
get_tag_files(tag_id, target_file_path = NULL, file_paths = NULL)
```

## Arguments

- tag_id:

  The ID of the tag to retrieve files for.

- target_file_path:

  A path to the directory containing the tag files. Alternatively, you
  can provide a vector of file names directly using the `file_paths`
  parameter.

- file_paths:

  A vector of file names to search for the specified tag ID.

## Value

A vector of file paths associated with the specified tag ID.

## Examples

``` r
if (FALSE) { # \dontrun{
get_tag_files("61029", "path/to/directory")
} # }
```
