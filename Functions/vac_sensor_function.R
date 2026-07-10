
# Used in vac_sensors.R

# Combines csv files and adds "sensor" category

# Used in vac_sensors.R
# Combines csv files and adds "sensor" category, remove "timestamp" column, seperate "datetime" into functional columns

combine_vac_files <- function(file_list) {

  combined <- purrr::map_dfr(file_list, function(file) {

    readr::read_csv(file, show_col_types = FALSE) %>%
      dplyr::mutate(
        sensor = as.integer(stringr::str_extract(basename(file), "\\d+"))
      )

  }) %>%
    dplyr::select(-any_of("timestamp")) %>%
    dplyr::mutate(
      datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S"),
      date = as.Date(datetime),
      time = format(datetime, "%H:%M:%S")
    )

  return(combined)
}
