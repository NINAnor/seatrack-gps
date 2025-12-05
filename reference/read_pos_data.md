# Get position data from a specific tag file

Get position data from a specific tag file

## Usage

``` r
read_pos_data(tag_id, file_connection)
```

## Arguments

- tag_id:

  The ID of the tag to retrieve position data for.

- file_connection:

  FILE CONNECTION

## Value

A data frame containing the position data with a POSIXct date_time
column and position measurements.

## Examples

``` r
if (FALSE) { # \dontrun{
get_pos_data("61029", "path/to/directory")
} # }
```
