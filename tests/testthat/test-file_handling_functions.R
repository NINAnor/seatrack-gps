library(archive)
# Mock data and helper functions
mock_folder <- "mock_data"
mock_archive <- "mock_data.zip"
mock_tag_id <- "61029"

# Create mock data for testing
setup_mock_data <- function() {
  tag_path <- file.path(mock_folder, paste0("Tag",mock_tag_id))
  dir.create(tag_path, showWarnings = FALSE, recursive = TRUE)

  # Create mock acceleration data (AccWetDry.txt)
  acc_wetdry_data <- c(
    "*************************************************************************************",
    "PathTrack Raw Accelerometer and Immersion Data File for Tag61029 (NanoFix Accelerometer Format)",
    "Created 07.06.25 21:32\t\t**57,3147**\t\t (F/W Ver. 030524-NanoFixAcc2_20)",
    "DO NOT MODIFY THIS HEADER",
    "*************************************************************************************",
    "2024 6 14 18 0 16,00 -0,0469 -0,7656 -0,7031 1,0406 0",
    "2024 6 14 18 1 16,00 -0,0625 -0,7344 -0,7031 1,0186 0",
    "2024 6 14 18 2 16,00 -0,0938 -0,7344 -0,6875 1,0103 0"
  )
  writeLines(acc_wetdry_data, file.path(tag_path, "Tag61029AccWetDry.txt"))

  # Create mock acceleration data (Accel.txt)
  accel_data <- c(
    "*************************************************************************************",
    "PathTrack Raw Accelerometer Data File for Tag61029 (NanoFix Accelerometer Format)",
    "Created 07.06.25 21:32\t\t**57,3147**\t\t (F/W Ver. 030524-NanoFixAcc2_20)",
    "DO NOT MODIFY THIS HEADER",
    "*************************************************************************************",
    "2024 6 14 18 0 16,04 -0,0625 -0,7344 -0,6875 1,0079",
    "2024 6 14 18 0 16,08 -0,0625 -0,7188 -0,7031 1,0074",
    "2024 6 14 18 0 16,12 -0,0625 -0,7344 -0,7031 1,0186"
  )
  writeLines(accel_data, file.path(tag_path, "Tag61029Accel.txt"))

  # Create mock position data (pos)
  pos_data <- c(
    "*************************************************************************************",
    "PathTrack Archival Tracking System Results File",
    "Created 07.06.25 21:39",
    "DO NOT MODIFY THIS HEADER",
    "*************************************************************************************",
    "14,06,24,18,00,25,64825.400,0,0.000000,0.000000,0.00,9999.999,9999.999000000000,4.14",
    "15,06,24,16,01,27,57687.400,0,0.000000,0.000000,0.00,9999.999,9999.999000000000,4.14",
    "16,06,24,14,02,29,50549.700,4,62.407327,5.596378,100.00,9999.999,0.000000767634,4.14"
  )
  writeLines(pos_data, file.path(tag_path, "Tag61029.pos"))
}

teardown_mock_data <- function() {
  unlink(mock_folder, recursive = TRUE)
}

setup_mock_archive <- function() {
  setup_mock_data()
  # Create zip archive with mock tag files using archive package
  archive::archive_write_dir(mock_archive, mock_folder, recursive = TRUE)
}

teardown_mock_archive <- function() {
  unlink(mock_archive)
}



# -------------------------------
# Tests for open_gps_data_files
# -------------------------------
test_that("open_gps_data_files finds compatible files", {
  setup_mock_data()
  result <- open_gps_data_files(mock_folder)
  expect_type(result, "list")
  expect_length(result, 1)
  teardown_mock_data()
})

test_that("open_gps_data_files returns empty list for empty folder", {
  dir.create(mock_folder, showWarnings = FALSE)
  result <- open_gps_data_files(mock_folder)
  expect_type(result, "list")
  expect_length(result, 0)
  teardown_mock_data()
})

test_that("open_gps_data_files finds compatible files in archive", {
  setup_mock_archive()
  teardown_mock_data()
  result <- open_gps_data_files(dirname(mock_archive))
  expect_type(result, "list")
  expect_length(result, 1)
  teardown_mock_archive()
})

# -------------------------------
# Tests for load_all_gps_data
# -------------------------------
test_that("load_all_gps_data loads data from a folder", {
  setup_mock_data()
  result <- load_all_gps_data(folder_path = mock_folder)
  expect_type(result, "list")
  expect_named(result, mock_tag_id)
  teardown_mock_data()
})

test_that("load_all_gps_data returns empty list for folder with no compatible files", {
  dir.create(mock_folder, showWarnings = FALSE)
  file.create(file.path(mock_folder, "random.txt"))
  result <- load_all_gps_data(folder_path = mock_folder)
  expect_type(result, "list")
  expect_length(result, 0)
  teardown_mock_data()
})

# -------------------------------
# Tests for load_all_gps_data (archive)
# -------------------------------
test_that("load_all_gps_data loads data from a zip archive", {
  setup_mock_archive()
  result <- load_all_gps_data(archive_path = mock_archive)
  expect_type(result, "list")
  expect_named(result, mock_tag_id)
  teardown_mock_archive()
  teardown_mock_data()
})

test_that("load_all_gps_data returns empty list for archive with no compatible files", {
  # Create empty folder and zip it using archive package
  dir.create(mock_folder, showWarnings = FALSE)
  file.create(file.path(mock_folder, "random.txt"))
  archive::archive_write_files(mock_archive, file.path(mock_folder, "random.txt"))
  result <- load_all_gps_data(archive_path = mock_archive)
  expect_type(result, "list")
  expect_length(result, 0)
  teardown_mock_archive()
  teardown_mock_data()
})

# -------------------------------
# Tests for get_tag_files
# -------------------------------
test_that("get_tag_files retrieves files for a specific tag", {
  setup_mock_data()
  result <- get_tag_files(mock_tag_id, target_file_path = mock_folder)
  expect_type(result, "character")
  expect_true(any(grepl("AccWetDry.txt", result)))
  teardown_mock_data()
})

test_that("get_tag_files returns character(0) for non-existent tag", {
  setup_mock_data()
  result <- get_tag_files("99999", target_file_path = mock_folder)
  expect_type(result, "character")
  expect_length(result, 0)
  teardown_mock_data()
})

# -------------------------------
# Tests for load_tag_acc_data
# -------------------------------
test_that("load_tag_acc_data loads acceleration data", {
  setup_mock_data()
  tag_files <- get_tag_files(mock_tag_id, target_file_path = mock_folder)
  result <- load_tag_acc_data(mock_tag_id, tag_files, immersion = TRUE)
  expect_s3_class(result, "data.frame")
  teardown_mock_data()
})

test_that("load_tag_acc_data returns NULL if AccWetDry.txt missing when immersion=TRUE", {
  setup_mock_data()
  file.remove(file.path(mock_folder, paste0("Tag",mock_tag_id), "Tag61029AccWetDry.txt"))
  tag_files <- get_tag_files(mock_tag_id, target_file_path = mock_folder)
  result <- load_tag_acc_data(mock_tag_id, tag_files, immersion = TRUE)
  expect_true(is.null(result))
  teardown_mock_data()
})

test_that("load_tag_acc_data returns NULL if Accel.txt missing when immersion=FALSE", {
  setup_mock_data()
  file.remove(file.path(mock_folder, paste0("Tag",mock_tag_id), "Tag61029Accel.txt"))
  tag_files <- get_tag_files(mock_tag_id, target_file_path = mock_folder)
  result <- load_tag_acc_data(mock_tag_id, tag_files, immersion = FALSE)
  expect_true(is.null(result))
  teardown_mock_data()
})

# -------------------------------
# Tests for load_tag_pos_data
# -------------------------------
test_that("load_tag_pos_data loads position data", {
  setup_mock_data()
  tag_files <- get_tag_files(mock_tag_id, target_file_path = mock_folder)
  result <- load_tag_pos_data(mock_tag_id, tag_files)
  expect_s3_class(result, "data.frame")
  teardown_mock_data()
})

test_that("load_tag_pos_data returns NULL if pos file is missing", {
  setup_mock_data()
  file.remove(file.path(mock_folder, paste0("Tag",mock_tag_id), "Tag61029.pos"))
  tag_files <- get_tag_files(mock_tag_id, target_file_path = mock_folder)
  result <- load_tag_pos_data(mock_tag_id, tag_files)
  expect_true(is.null(result))
  teardown_mock_data()
})

# -------------------------------
# Tests for get_diagnostics
# -------------------------------
test_that("get_diagnostics computes diagnostics from position data", {
  setup_mock_data()
  tag_files <- get_tag_files(mock_tag_id, target_file_path = mock_folder)
  pos_data <- load_tag_pos_data(mock_tag_id, tag_files)
  result <- get_diagnostics(pos_data)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("tag_id", "start_date", "end_date",
                        "n_positions", "deployment_length_days", "n_no_lat_lon",
                        "min_sat_no_fix", "max_sat_no_fix", "mean_sat_no_fix", "sd_sat_no_fix",
                        "n_with_lat_lon", "min_sat_with_fix", "max_sat_with_fix", "mean_sat_with_fix", "sd_sat_with_fix",
                        "min_altitude_with_fix", "max_altitude_with_fix", "mean_altitude_with_fix", "sd_altitude_with_fix",
                        "min_accuracy_with_fix", "max_accuracy_with_fix", "mean_accuracy_with_fix", "sd_accuracy_with_fix"))
  teardown_mock_data()
})

test_that("get_diagnostics returns null for an empty data.frame", {
  empty_df <- data.frame()
  result <- get_diagnostics(empty_df)
  expect_true(is.null(result))
})

test_that("get_diagnostics handles all positions with no lat/lon", {
  setup_mock_data()
  tag_files <- get_tag_files(mock_tag_id, target_file_path = mock_folder)
  pos_data <- load_tag_pos_data(mock_tag_id, tag_files)
  # Set all latitude/longitude to NA
  pos_data$latitude <- NA
  pos_data$longitude <- NA
  result <- get_diagnostics(pos_data)
  expect_equal(result$n_with_lat_lon, 0)
  teardown_mock_data()
})

test_that("get_diagnostics handles all positions with lat/lon", {
  setup_mock_data()
  tag_files <- get_tag_files(mock_tag_id, target_file_path = mock_folder)
  pos_data <- load_tag_pos_data(mock_tag_id, tag_files)
  # Set all latitude/longitude to non-NA
  pos_data$latitude <- rep(62.4, nrow(pos_data))
  pos_data$longitude <- rep(5.6, nrow(pos_data))
  result <- get_diagnostics(pos_data)
  expect_equal(result$n_no_lat_lon, 0)
  teardown_mock_data()
})

test_that("get_diagnostics generates a dataframe of NAs if empty data.frame but acc data provided", {
  setup_mock_data()
  tag_files <- get_tag_files(mock_tag_id, target_file_path = mock_folder)
  acc_data <- load_tag_acc_data(mock_tag_id, tag_files, immersion = FALSE)
  result <- get_diagnostics(data.frame(), acc_data)
  expect_equal(result$tag_id, acc_data$tag_id[1])
  expect_true(is.na(result$n_no_lat_lon))
  expect_true(result$n_positions == 0)
  teardown_mock_data()
})

test_that("get_diagnostics NA result binds to normal result", {
  setup_mock_data()
  tag_files <- get_tag_files(mock_tag_id, target_file_path = mock_folder)
  acc_data <- load_tag_acc_data(mock_tag_id, tag_files, immersion = FALSE)
  pos_data <- load_tag_pos_data(mock_tag_id, tag_files)
  na_result <- get_diagnostics(data.frame(), acc_data)
  result <- get_diagnostics(pos_data)
  new_result <- rbind(result, na_result)
  expect_true(nrow(new_result) == 2)
  teardown_mock_data()
})