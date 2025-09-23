#' Find all compatible GPS data files in a directory structure
#'
#' This function searches through a specified root directory and its subdirectories
#' for GPS data files that match the expected naming patterns. It can handle both loose
#' files and files contained within archive formats such as .zip and .7z.
#'
#' @param root_dir The root directory to start searching from.
#' @return A list of lists, each containing data frames for a specific tag:
#' - acc_immersion_data: A data frame containing acceleration data with immersion information.
#' - acc_data: A data frame containing acceleration data without immersion information.
#' - pos_data: A data frame containing position data.
#' - diag_data: A data frame containing diagnostics information.
#' @examples
#' open_gps_data_files("path/to/root_directory")
#' @export
open_gps_data_files <- function(root_dir) {
    # Loose files
    all_files <- list.files(root_dir, recursive = TRUE, full.names = TRUE)

    gps_files <- all_files[grepl("Tag.*(AccWetDry.txt|Accel.txt|.pos)$", all_files)]
    if (length(gps_files) == 0) {
       folder_data <- list()
    }else {
        gps_dirs <- dirname(gps_files)
        gps_dirs <- unique(gps_dirs)
        folder_data <- lapply(gps_dirs, load_all_gps_data)
        # Flatten list of lists
        folder_data <- do.call(c, folder_data)

        # remove empty entries
        folder_data <- folder_data[sapply(folder_data, function(x) length(x) > 0)]
    }


    archives <- all_files[grepl("\\.(zip|7z)$", all_files)]
    if (length(archives) == 0) {
        archive_data <- list()
    } else {
        archive_data <- lapply(archives, function(x) load_all_gps_data(archive_path = x))
        # Flatten list of lists
        archive_data <- do.call(c, archive_data)
        # remove NULL entries
        archive_data <- archive_data[sapply(archive_data, function(x) length(x) > 0)]
    }
    all_data <- c(folder_data, archive_data)
    return(all_data)
}

#' Read all data from an archive or folder
#'
#' Given a folder path or an archive path, this function reads all tag data files
#' and returns a list of data frames for each tag, including acceleration data (with and
#' without immersion), position data, and diagnostics.
#' @param folder_path The path to the folder containing the tag data files (if applicable).
#' @param archive_path The path to the archive file (if applicable).
#' @return A list of lists, each containing data frames for a specific tag:
#' - acc_immersion_data: A data frame containing acceleration data with immersion information.
#' - acc_data: A data frame containing acceleration data without immersion information.
#' - pos_data: A data frame containing position data.
#' - diag_data: A data frame containing diagnostics information.
#' @examples
#' load_all_data(folder_path = "path/to/directory") # Load data from a folder
#' load_all_data(archive_path = "path/to/archive.zip") # Load data from an archive
#' @export
load_all_gps_data <- function(folder_path = NULL, archive_path = NULL) {
    # Get all tags
    if (!is.null(folder_path)) {
        all_tag_files <- list.files(folder_path, pattern = "Tag")
        tags_only <- regmatches(all_tag_files, regexpr("(?<=Tag)[^\\.Acc]+", all_tag_files, perl = TRUE))
        tags <- unique(tags_only)
    } else if (!is.null(archive_path)) {
        archive_files <- archive(archive_path)
        archive_folders <- archive_files[archive_files$size == 0 & grepl("/$", archive_files$path), ]
        tags <- gsub("Tag|/", "", archive_folders$path)
    } else {
        stop("Either folder_path or archive_path must be provided.")
    }
    if (length(tags) == 0) {
        return(list())
    }
    # Loop through each tag and load data
    all_tag_data <- lapply(tags, function(tag_id) {
        # Get tag files
        if (!is.null(archive_path)) {
            tag_file_paths <- get_tag_files(tag_id, file_paths = archive_files$path)
        } else if (!is.null(folder_path)) {
            tag_file_paths <- get_tag_files(tag_id, target_file_path = folder_path)
        }

        # If no files found, return NULL
        if (length(tag_file_paths) == 0) {
            warning(paste("No files found for tag", tag_id))
            return(NULL)
        }

        # Load acceleration data
        acc_immersion_data <- load_tag_acc_data(tag_id, tag_file_paths, archive_path = archive_path, immersion = TRUE)
        acc_data <- load_tag_acc_data(tag_id, tag_file_paths, archive_path = archive_path, immersion = FALSE)
        # Load position data
        pos_data <- load_tag_pos_data(tag_id, tag_file_paths, archive_path = archive_path)
        # Get diagnostics
        if (!is.null(pos_data)) {
            diag_data <- get_diagnostics(pos_data)
        } else {
            diag_data <- get_diagnostics(data.frame(), acc_data)
        }
        return(list(
        acc_immersion_data = acc_immersion_data,
        acc_data = acc_data,
        pos_data = pos_data,
        diag_data = diag_data))
    })

    names(all_tag_data) <- tags

    # Remove NULL entries
    all_tag_data <- all_tag_data[!sapply(all_tag_data, is.null)]

    return(all_tag_data)
}

#' Get all files for a specific tag
#'
#' @param tag_id The ID of the tag to retrieve files for.
#' @param target_file_path A path to the directory containing the tag files.
#' Alternatively, you can provide a vector of file names directly using the `file_paths` parameter.
#' @param file_paths A vector of file names to search for the specified tag ID.
#' @return A vector of file paths associated with the specified tag ID.
#' @examples
#' get_tag_files("61029", "path/to/directory")
#' @export
get_tag_files <- function(tag_id, target_file_path = NULL, file_paths = NULL) {
    if (!is.null(target_file_path)) {
        tag_files <- list.files(target_file_path, pattern = paste0("Tag", tag_id), full.names = TRUE)
    } else if (!is.null(file_paths)) {
        tag_files <- file_paths[grep(paste0("Tag", tag_id), file_paths, fixed = TRUE)]
    } else {
        stop("Either target_file_path or file_paths must be provided.")
    }

    tag_files <- tag_files[grepl("Tag.*(AccWetDry.txt|Accel.txt|.pos)$", tag_files)]

    return(tag_files)
}

#' Load acceleration data for a specific tag
#' @param tag_id The ID of the tag to retrieve acceleration data for.
#' @param tag_files character vector of file paths associated with the specified tag ID.
#' @param archive_path The path to the archive file (if applicable).
#' @param immersion Logical indicating whether to read immersion data (default is TRUE).
#' @param sep The separator used in the data file (default is space).
#' @param dec The decimal point character used in the data file (default is comma).
#' @return A data frame containing the acceleration data with a POSIXct date_time column
#' and acceleration measurements.
#' @examples
#' load_tag_acc_data("61029", "path/to/archive", immersion = TRUE)
#' @export
load_tag_acc_data <- function(
    tag_id, tag_files, archive_path = NULL,
    immersion = TRUE, sep = " ", dec = ",") {
    if (immersion) {
        acc_data_file <- tag_files[grep("AccWetDry.txt", tag_files)]
    } else {
        acc_data_file <- tag_files[grep("Accel.txt", tag_files)]
    }

    if (length(acc_data_file) == 0) {
        warning(paste("No acceleration data file found for tag", tag_id))
        return(NULL)
    }

    if (is.null(archive_path)) {
        file_conn <- file(acc_data_file, open = "r")
    } else if (!is.null(archive_path)) {
        file_conn <- archive_read(archive_path, acc_data_file)
    }

    acc_data <- read_acc_data(tag_id, file_conn, immersion = immersion, sep = sep, dec = dec)
    if (is.null(archive_path)) {
        close(file_conn)
    }
    return(acc_data)
}

#' Get acceleration data from a file connection
#' @param tag_id The ID of the tag to retrieve acceleration data for.
#' @param file_connection A connection to the file containing the acceleration data.
#' @param immersion Logical indicating whether to read immersion data (default is TRUE).
#' @param sep The separator used in the data file (default is space).
#' @param dec The decimal point character used in the data file (default is comma).
#' @return A data frame containing the acceleration data with a POSIXct date_time column
#' and acceleration measurements.
#' @examples
#' get_acc_data("61029", "path/to/directory", immersion = TRUE)
#' @export
read_acc_data <- function(tag_id, file_connection, immersion = TRUE, sep = " ", dec = ",") {
    acc_data <- read.table(file_connection, header = FALSE, skip = 5, sep = sep, dec = dec)
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

#' Load position data for a specific tag
#' @param tag_id The ID of the tag to retrieve position data for.
#' @param tag_files character vector of file paths associated with the specified tag ID.
#' @param archive_path The path to the archive file (if applicable).
#' @param sep The separator used in the data file (default is comma).
#' @param dec The decimal point character used in the data file (default is period).
#' @return A data frame containing the position data with a POSIXct date_time column
load_tag_pos_data <- function(tag_id, tag_files, archive_path = NULL, sep = ",", dec = ".") {
    pos_data_file <- tag_files[grep(".pos", tag_files, fixed = TRUE)]
    if (length(pos_data_file) == 0) {
        warning(paste("No position data file found for tag", tag_id))
        return(NULL)
    }
    if (is.null(archive_path)) {
        file_conn <- file(pos_data_file, open = "r")
    } else if (!is.null(archive_path)) {
        file_conn <- archive_read(archive_path, pos_data_file)
    }

    pos_data <- read_pos_data(tag_id, file_conn, sep = sep, dec = dec)
    if (is.null(archive_path)) {
        close(file_conn)
    }
    return(pos_data)
}

#' Get position data from a specific tag file
#' @param tag_id The ID of the tag to retrieve position data for.
#' @param file_connection FILE CONNECTION
#' @param sep The separator used in the data file (default is comma).
#' @param dec The decimal point character used in the data file (default is period).
#' @return A data frame containing the position data with a POSIXct date_time column
#' and position measurements.
#' @examples
#' get_pos_data("61029", "path/to/directory")
#' @export
read_pos_data <- function(tag_id, file_connection, sep = ",", dec = ".") {
    pos_data <- read.table(file_connection, header = FALSE, skip = 5, sep = sep, dec = dec)
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
#' and position measurements OR an empty dataframe.
#' @param acc_data A data frame containing acceleration data, used as a fallback if an empt pos_data is provided
#' @return A data frame containing diagnostics information such as deployment length,
#' number of positions, and statistics on satellite counts and accuracy. If an empty dataframe is provided, this will largely be NA.
#' @examples
#' get_diagnostics(pos_data)
#' @export
get_diagnostics <- function(pos_data, acc_data = NULL) {

    if (nrow(pos_data) == 0 && !is.null(acc_data)) {
        na_diag <- data.frame(
            tag_id = acc_data$tag_id[1],
            start_date = min(acc_data$date_time, na.rm = TRUE),
            end_date = max(acc_data$date_time, na.rm = TRUE),
            n_positions = 0,
            deployment_length_days = NA,
            n_no_lat_lon = NA,
            min_sat_no_fix = NA,
            max_sat_no_fix = NA,
            mean_sat_no_fix = NA,
            sd_sat_no_fix = NA,
            n_with_lat_lon = 0,
            min_sat_with_fix = NA,
            max_sat_with_fix = NA,
            mean_sat_with_fix = NA,
            sd_sat_with_fix = NA,
            min_altitude_with_fix = NA,
            max_altitude_with_fix = NA,
            mean_altitude_with_fix = NA,
            sd_altitude_with_fix = NA,
            min_accuracy_with_fix = NA,
            max_accuracy_with_fix = NA,
            mean_accuracy_with_fix = NA,
            sd_accuracy_with_fix = NA
        )

        na_diag$deployment_length_days <- as.numeric(difftime(na_diag$end_date, na_diag$start_date, units = "days"))

        return(na_diag)
    }else if (nrow(pos_data) == 0 && is.null(acc_data)) {
        return(NULL)
    }

    diag_data <- data.frame(
        tag_id = unique(pos_data$tag_id),
        start_date = min(pos_data$date_time, na.rm = TRUE),
        end_date = max(pos_data$date_time, na.rm = TRUE),
        n_positions = nrow(pos_data)
    )

    diag_data$deployment_length_days <- as.numeric(difftime(diag_data$end_date, diag_data$start_date, units = "days"))

    no_fix_bool <- pos_data$latitude == 0 & pos_data$longitude == 0

    # No fix statistics
    diag_data$n_no_lat_lon <- sum(no_fix_bool, na.rm = TRUE)
    diag_data$min_sat_no_fix <- min(pos_data$n_satellites[no_fix_bool], na.rm = TRUE)
    diag_data$max_sat_no_fix <- max(pos_data$n_satellites[no_fix_bool], na.rm = TRUE)
    diag_data$mean_sat_no_fix <- mean(pos_data$n_satellites[no_fix_bool], na.rm = TRUE)
    diag_data$sd_sat_no_fix <- sd(pos_data$n_satellites[no_fix_bool], na.rm = TRUE)

    # With fix statistics
    diag_data$n_with_lat_lon <- sum(!no_fix_bool, na.rm = TRUE)
    diag_data$min_sat_with_fix <- min(pos_data$n_satellites[!no_fix_bool], na.rm = TRUE)
    diag_data$max_sat_with_fix <- max(pos_data$n_satellites[!no_fix_bool], na.rm = TRUE)
    diag_data$mean_sat_with_fix <- mean(pos_data$n_satellites[!no_fix_bool], na.rm = TRUE)
    diag_data$sd_sat_with_fix <- sd(pos_data$n_satellites[!no_fix_bool], na.rm = TRUE)

    # Altitude with fix statistics
    diag_data$min_altitude_with_fix <- min(pos_data$altitude[!no_fix_bool], na.rm = TRUE)
    diag_data$max_altitude_with_fix <- max(pos_data$altitude[!no_fix_bool], na.rm = TRUE)
    diag_data$mean_altitude_with_fix <- mean(pos_data$altitude[!no_fix_bool], na.rm = TRUE)
    diag_data$sd_altitude_with_fix <- sd(pos_data$altitude[!no_fix_bool], na.rm = TRUE)

    # Accuracy with fix statistics
    diag_data$min_accuracy_with_fix <- min(pos_data$accuracy[!no_fix_bool], na.rm = TRUE)
    diag_data$max_accuracy_with_fix <- max(pos_data$accuracy[!no_fix_bool], na.rm = TRUE)
    diag_data$mean_accuracy_with_fix <- mean(pos_data$accuracy[!no_fix_bool], na.rm = TRUE)
    diag_data$sd_accuracy_with_fix <- sd(pos_data$accuracy[!no_fix_bool], na.rm = TRUE)

    diag_data[is.numeric(diag_data) && is.infinite(diag_data)] <- NA

    return(diag_data)
}
