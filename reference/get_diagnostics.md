# Get diagnostics from position data

Get diagnostics from position data

## Usage

``` r
get_diagnostics(pos_data, acc_data = NULL)
```

## Arguments

- pos_data:

  A data frame containing position data with a POSIXct date_time column
  and position measurements OR an empty dataframe.

- acc_data:

  A data frame containing acceleration data, used as a fallback if an
  empt pos_data is provided

## Value

A data frame containing diagnostics information such as deployment
length, number of positions, and statistics on satellite counts and
accuracy. If an empty dataframe is provided, this will largely be NA.

## Examples

``` r
if (FALSE) { # \dontrun{
get_diagnostics(pos_data)
} # }
```
