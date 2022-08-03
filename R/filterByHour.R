#' Filter by hour
#' @description filter that allows the user to filter the TT data by hour.
#' @param data4D The 4D TT+ data
#' @param hours A character vectors of the hours to be included: e.g.:
#'   c("2","3","4") or "12".
#' @export
#'
#'
#'
filterByHour <- function(data, hours){
  output <- data %>% dplyr::filter(f_hour %in% hours)
  return(output)
}
