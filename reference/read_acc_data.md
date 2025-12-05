# Get acceleration data from a file connection

Get acceleration data from a file connection

## Usage

``` r
read_acc_data(tag_id, file_connection, immersion = TRUE)
```

## Arguments

- tag_id:

  The ID of the tag to retrieve acceleration data for.

- file_connection:

  A connection to the file containing the acceleration data.

- immersion:

  Logical indicating whether to read immersion data (default is TRUE).

## Value

A data frame containing the acceleration data with a POSIXct date_time
column and acceleration measurements.

## Examples

``` r
if (FALSE) { # \dontrun{
get_acc_data("61029", "path/to/directory", immersion = TRUE)
} # }
```
