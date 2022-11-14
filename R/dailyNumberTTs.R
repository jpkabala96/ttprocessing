#' Tree talkers with valid data per day 
#' @description Function that computes the number of TT with valid data per day,
#'   given the output of the \code{dailyQualityCheck} function output
#' @param qcdata Dataframe with quality check data, produced by 
#'   \code{dailyQualityCheck}.
#' @param variable_of_interest String, the variable of interest. Must be one of
#'   the colnames of qcdata. In the present version one of \code{"Tref0", "Theat0", "Tref1", "Theat1", "voltage", "Tair",
#'   "RH", "do_sap_flow", "vpd", "sap_flow_one_probe", "asgharinia_sap_flow"}. 
#'   Default value \code{"do_sap_flow"}.
#' @param min_valid_obs Numeric, minimum of valid observations per day required
#'   for each device, to consider its data valid. Default value \code{24}.
#' @return A dataframe with the number of TT with valid data.
#' @export
#' 
dailyNumberTTs <- function(qcdata, 
                           variable_of_interest = "do_sap_flow",
                           min_valid_obs = 24){
  TTdaydata <- qcdata %>% 
    dplyr::mutate(sufficient_obs = ifelse(eval(parse(text = paste("complete_obs",
                                                                  variable_of_interest,
                                                                  sep = ".")))>= min_valid_obs,
                                          TRUE,
                                          FALSE)) %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(NTTS = sum(sufficient_obs, na.rm = T))
  print("number of TTs per day with")
  print("complete data of:")
  print(variable_of_interest)
  return(TTdaydata)
}
