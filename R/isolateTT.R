#' Function for filtering TT+ data
#'
#' @description This function retrieves the id from the raw data downloaded from
#'   the server (as the ones returned by \code{readTTdata}), and returns
#'   only the rows with ids corresponding to the TT+ devices. There is need to
#'   use this function as there can be different devices connected to the same
#'   TTcloud: for example the TTsoils, the TTreference or the TTG. Those other
#'   devices can be filtered out using this function. The ids associated to the
#'   TT+ devices should be provided to the ttplus_ids argument as a character
#'   vector.
#' @param raw_tt_data The raw data, as the ones provided as output by the
#'   \code{readTTData} function.
#' @param ttplus_ids The ids associated with the TT+ devices. A character
#'   vector.
#' @return ttplus_data A data.frame, with the id isolated from the raw data
#'   and only the TT+ data

#' @export
isolateTT <- function(raw_tt_data, ttplus_ids){
  raw_tt_data$id <- obtainId(raw_tt_data)
  ttplus_data <- raw_tt_data %>%
    dplyr::filter(id %in% ttplus_ids)
  return(ttplus_data)
}
