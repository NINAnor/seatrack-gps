# Read all data from an archive or folder

Given a folder path or an archive path, this function reads all tag data
files and returns a list of data frames for each tag, including
acceleration data (with and without immersion), position data, and
diagnostics.

## Usage

``` r
load_all_gps_data(folder_path = NULL, archive_path = NULL)
```

## Arguments

- folder_path:

  The path to the folder containing the tag data files (if applicable).

- archive_path:

  The path to the archive file (if applicable).

## Value

A list of lists, each containing data frames for a specific tag:

- acc_immersion_data: A data frame containing acceleration data with
  immersion information.

- acc_data: A data frame containing acceleration data without immersion
  information.

- pos_data: A data frame containing position data.

- diag_data: A data frame containing diagnostics information.

- root_folders: vector of filepaths from which tag data were loaded.

- base_path: folder_path or archive_path which was used to load files.

## Examples

``` r
if (FALSE) { # \dontrun{
load_all_data(folder_path = "path/to/directory") # Load data from a folder
load_all_data(archive_path = "path/to/archive.zip") # Load data from an archive
} # }
```
