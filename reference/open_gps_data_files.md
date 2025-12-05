# Find all compatible GPS data files in a directory structure

This function searches through a specified root directory and its
subdirectories for GPS data files that match the expected naming
patterns. It can handle both loose files and files contained within
archive formats such as .zip and .7z.

## Usage

``` r
open_gps_data_files(root_dir)
```

## Arguments

- root_dir:

  The root directory to start searching from.

## Value

A list of lists, each containing data frames for a specific tag:

- acc_immersion_data: A data frame containing acceleration data with
  immersion information.

- acc_data: A data frame containing acceleration data without immersion
  information.

- pos_data: A data frame containing position data.

- diag_data: A data frame containing diagnostics information.

## Examples

``` r
if (FALSE) { # \dontrun{
open_gps_data_files("path/to/root_directory")
} # }
```
