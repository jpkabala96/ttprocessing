#' Check battery level
#'
#' @description function to fastly check the battery level of the devices.
#' @param link_string Link to the cloud for which the battery levels are to be
#'   checked.
#' @param ndays_ago Number of days ago to consider. Defaults to \code{60}.
#'
#'@export

fastBatteryCheck <- function(link_string, ndays_ago = 60){
  #input check
  # assertthat::assert_that(is.string(link_string),
  #                    msg = "The link_string arg must be a string: the link to
  #                    the server or the path to the downloaded data.")
  # assertthat::assert_that(is.scalar(ndays_ago),
  #                    msg = "The ndays_ago argument must be a numeric of length
  #                    one, indicating the number of days ago to look to")

  data <- readTTData(link_string = link_string)
  data <- data %>% filter(V4 > max(V4) - 60*60*24*ndays_ago)
  TT_voltage_table <- clean4DData(data = data) %>%
    dplyr::select("id", "date_hour", "voltage") %>%
    dplyr::group_by(id) %>%
    dplyr::filter(date_hour == max(date_hour)) %>%
    dplyr::ungroup()
  server_voltage <- cloudData4B(raw_data = data) %>%
    dplyr::filter(date_hour == max(date_hour)) %>%
    dplyr::select("id", "date_hour", "voltage")
  server_voltage$voltage <- as.numeric(server_voltage$voltage)

  report_table <- rbind(server_voltage, TT_voltage_table)
  print(report_table)
  return(report_table)
}
