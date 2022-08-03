#' Filtering TT data by date
#' @description This function makes it easy to filter data by dates
#' @param data A dataframe with clean TT data, as for example the
#'   output of \code{clean4DData} function.
#' @param start The first date of the period to consider. A Date object
#'   or an object that is coercible to Date
#' @param end The last date to be considered. A Date object, or an object
#'   that is coercible to Date.
#' @details This function makes use of the \code{dplyr} function \code{filter}
#'   to filter the data, and returns the filtered data.frame.
#' @export
#'

filterByDate <- function(data, start = "2020-01-01", end = Sys.Date()){
  #assertthat::assert_that(length(start) == 1,
  #                        msg = "Start must be length 1.")
  #assertthat::assert_that(length(end) == 1,
  #                        msg = "End must be length 1.")
  start <- as.Date(start)
  end <- as.Date(end)
  output_data <- data %>%
    dplyr::filter(date >= start) %>%
    dplyr::filter(date <= end)
  return(output_data)
}
