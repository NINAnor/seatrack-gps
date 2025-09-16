source("R/functions.R")
logger_id <- "61029"
raw_folder <- file.path("test_data", "Great skua", "rawdata")

acc_data <- get_acc_data(logger_id, raw_folder, FALSE)
acc_wet_data <- get_acc_data(logger_id, raw_folder)
pos_data <- get_pos_data(logger_id, raw_folder)

get_diagnostics(pos_data)
