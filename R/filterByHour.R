#' Filter by hour
#' @description filter that allows the user to filter the TT data by hour.
#' @param data4D The 4D TT+ data
#' @param hours A character vectors of the hours to be included: e.g.:
#'   c("2","3","4") or "12".
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' clean_data <- clean4DData(raw4d_ex_data)
#' plotByMH(clean_data)
#' filtered_data <-  filterByHour(clean_data, c("8", "9", "10", "11", "12", "13", "14", "15", "16"))
#' plotByMH(filtered_data)
#' }
#' @export
#'
#'
#'
filterByHour <- function(data4D, hours){
  output <- data4D %>% dplyr::filter(f_hour %in% hours)
  return(output)
}
