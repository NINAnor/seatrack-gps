# Load acceleration data for a specific tag

Load acceleration data for a specific tag

## Usage

``` r
load_tag_acc_data(tag_id, tag_files, archive_path = NULL, immersion = TRUE)
```

## Arguments

- tag_id:

  The ID of the tag to retrieve acceleration data for.

- tag_files:

  character vector of file paths associated with the specified tag ID.

- archive_path:

  The path to the archive file (if applicable).

- immersion:

  Logical indicating whether to read immersion data (default is TRUE).

## Value

A data frame containing the acceleration data with a POSIXct date_time
column and acceleration measurements.

## Examples

``` r
if (FALSE) { # \dontrun{
load_tag_acc_data("61029", "path/to/archive", immersion = TRUE)
} # }
```
