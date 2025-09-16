
#' Get all files for a specific tag
#'
#' @param tag_id The ID of the tag to retrieve files for.
#' @param file_path The path to the directory containing the tag files.
#' @return A list of file paths associated with the specified tag ID.
#' @examples
#' get_tag_files("61029", "path/to/directory")
#' @export
get_tag_files <- function(tag_id, file_path) {
    tag_files <- list.files(raw_folder, pattern = paste0("Tag", logger_id), full.names = TRUE)
    return(tag_files)
}

#' Get acceleration data from a specific tag file
#' @param tag_id The ID of the tag to retrieve acceleration data for.
#' @param file_path The path to the directory containing the tag files.
#' @param immersion Logical indicating whether to read immersion data (default is TRUE).
#' @param sep The separator used in the data file (default is space).
#' @param dec The decimal point character used in the data file (default is comma).
#' @return A data frame containing the acceleration data with a POSIXct date_time column
#' and acceleration measurements.
#' @examples
#' get_acc_data("61029", "path/to/directory", immersion = TRUE)
#' @export
get_acc_data <- function(tag_id, file_path, immersion = TRUE, sep = " ", dec = ",") {
    tag_files <- get_tag_files(tag_id, file_path)
    if (immersion) {
        acc_data_file <- tag_files[grep("AccWetDry.txt", tag_files)]
    }else {
        acc_data_file <- tag_files[grep("Accel.txt", tag_files)]
    }
    acc_data <- read.table(acc_data_file, header = FALSE, skip = 5, sep = sep, dec = dec)
    time_cols <- c("year", "month", "day", "hour", "minute", "second")
    new_names <- c("x_acceleration", "y_acceleration", "z_acceleration", "acceleration_3d")
    if (immersion) new_names <- c(new_names, "wet")
    names(acc_data) <- c(time_cols, new_names)
    acc_data$tag_id <- tag_id
    acc_data$date_time <- as.POSIXct(paste(acc_data$year, acc_data$month, acc_data$day, acc_data$hour, acc_data$minute, acc_data$second), format = "%Y %m %d %H %M %S", tz = "UTC")
    acc_data <- acc_data[, c("tag_id", "date_time", new_names)]
    if (immersion) {
        acc_data$wet <- as.logical(acc_data$wet)
    }

    return(acc_data)
}

#' Get position data from a specific tag file
#' @param tag_id The ID of the tag to retrieve position data for.
#' @param file_path The path to the directory containing the tag files.
#' @param sep The separator used in the data file (default is comma).
#' @param dec The decimal point character used in the data file (default is period).
#' @return A data frame containing the position data with a POSIXct date_time column
#' and position measurements.
#' @examples
#' get_pos_data("61029", "path/to/directory")
#' @export
get_pos_data <- function(tag_id, file_path, sep = ",", dec = ".") {
    tag_files <- get_tag_files(tag_id, file_path)
    pos_data_file <- tag_files[grep(".pos", tag_files)]
    pos_data <- read.table(pos_data_file, header = FALSE, skip = 5, sep = sep, dec = dec)
    time_cols <- c("day", "month", "year", "hour", "minute", "second")
    new_names <- c("day_second", "n_satellites", "latitude", "longitude", "altitude", "clock_offset", "accuracy", "battery")
    names(pos_data) <- c(time_cols, new_names)
    pos_data$tag_id <- tag_id
    pos_data$date_time <- as.POSIXct(paste(pos_data$year, pos_data$month, pos_data$day, pos_data$hour, pos_data$minute, pos_data$second), format = "%y %m %d %H %M %S", tz = "UTC")
    pos_data <- pos_data[, c("tag_id", "date_time", new_names)]
    return(pos_data)
}

#' Get diagnostics from position data
#' @param pos_data A data frame containing position data with a POSIXct date_time column
#' and position measurements.
#' @return A data frame containing diagnostics information such as deployment length,
#' number of positions, and statistics on satellite counts and accuracy.
#' @examples
#' get_diagnostics(pos_data)
#' @export
get_diagnostics <- function(pos_data) {
    diag_data <- data.frame(
        tag_id = unique(pos_data$tag_id),
        start_date = min(pos_data$date_time, na.rm = TRUE),
        end_date = max(pos_data$date_time, na.rm = TRUE),
        n_positions = nrow(pos_data)
    )

    diag_data$deployment_length_days <- as.numeric(difftime(diag_data$end_date, diag_data$start_date, units = "days"))

    no_fix_bool <- pos_data$latitude == 0 & pos_data$longitude == 0

    # No fix statistics
    diag_data$n_no_lat_lon = sum(no_fix_bool, na.rm = TRUE)
    diag_data$min_sat_no_fix <- min(pos_data$n_satellites[no_fix_bool], na.rm = TRUE)
    diag_data$max_sat_no_fix <- max(pos_data$n_satellites[no_fix_bool], na.rm = TRUE)
    diag_data$mean_sat_no_fix <- mean(pos_data$n_satellites[no_fix_bool], na.rm = TRUE)
    diag_data$sd_sat_no_fix <- sd(pos_data$n_satellites[no_fix_bool], na.rm = TRUE)

    # With fix statistics
    diag_data$n_with_lat_lon = sum(!no_fix_bool, na.rm = TRUE)
    diag_data$min_sat_with_fix <- min(pos_data$n_satellites[!no_fix_bool], na.rm = TRUE)
    diag_data$max_sat_with_fix <- max(pos_data$n_satellites[!no_fix_bool], na.rm = TRUE)
    diag_data$mean_sat_with_fix <- mean(pos_data$n_satellites[!no_fix_bool], na.rm = TRUE)
    diag_data$sd_sat_with_fix <- sd(pos_data$n_satellites[!no_fix_bool], na.rm = TRUE)

    # Altitude with fix statistics
    diag_data$min_altitude_with_fix <- min(pos_data$altitude[!no_fix_bool], na.rm = TRUE)
    diag_data$max_altitude_with_fix <- max(pos_data$altitude[!no_fix_bool], na.rm = TRUE)
    diag_data$mean_altitude_with_fix <- mean(pos_data$altitude[!no_fix_bool], na.rm = TRUE)
    diag_data$sd_altitude_with_fix <- sd(pos_data$altitude[!no_fix_bool], na.rm = TRUE)

    #Accuracy with fix statistics
    diag_data$min_accuracy_with_fix <- min(pos_data$accuracy[!no_fix_bool], na.rm = TRUE)
    diag_data$max_accuracy_with_fix <- max(pos_data$accuracy[!no_fix_bool], na.rm = TRUE)
    diag_data$mean_accuracy_with_fix <- mean(pos_data$accuracy[!no_fix_bool], na.rm = TRUE)
    diag_data$sd_accuracy_with_fix <- sd(pos_data$accuracy[!no_fix_bool], na.rm = TRUE)

    return(diag_data)
}