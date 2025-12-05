# Load position data for a specific tag

Load position data for a specific tag

## Usage

``` r
load_tag_pos_data(tag_id, tag_files, archive_path = NULL)
```

## Arguments

- tag_id:

  The ID of the tag to retrieve position data for.

- tag_files:

  character vector of file paths associated with the specified tag ID.

- archive_path:

  The path to the archive file (if applicable).

## Value

A data frame containing the position data with a POSIXct date_time
column
