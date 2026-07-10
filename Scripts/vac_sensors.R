# Erabec Vacuum Sensor Readouts
# Loading from CSV and graphing

# Hannah McNulty
# 07/10/2026


library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)


##### Winter 2026 Readout

# 1. Path to files
folder_path <-"/Users/hannahmcnulty/Documents/github/PMRC_LTS/CleanedData/vac_data"

# Get all CSV files
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Remove manifest file
file_list <- file_list[!grepl("manifest", basename(file_list), ignore.case = TRUE)]

# Source vac_sensor_function
source("Functions/vac_sensor_function.R")

# Run
combined <- combine_vac_files(file_list)

# Check
head(combined)




ggplot(combined, aes(x = datetime, y = pressure, color = factor(sensor))) +
  geom_line() +
  labs(
    x = "Datetime",
    y = "Pressure",
    color = "Sensor"
  ) +
  theme_minimal()

