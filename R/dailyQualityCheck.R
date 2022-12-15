#' Data quality check
#'
#' @description  Checks the data quality by counting the number of daily
#'   valid records each day for each tree talker for each variable.
#'   Returns a data.frame with these data.
#' @param TTdata4d Dataframe with Tree talker 4D data, such as the one produced
#'   by \code{clean4DData}. 
#'@export




dailyQualityCheck <- function(TTdata4d){
  first_cols <- c("id", "date")
  cols_to_assess <- c("Tref0", "Theat0", "Tref1", "Theat1", "voltage", "Tair",
                      "RH", "do_sap_flow", "vpd", "asgharinia_sap_flow", "VWC")
  pl <- TTdata4d %>%
    tidyr::pivot_longer(cols = cols_to_assess, names_to = "col", values_to = "value") %>%
    dplyr::group_by(id, date, col) %>%
    dplyr::summarise(oss_complete = sum(!is.na(value)))
  print("pt1 done")
  pw <- tidyr::pivot_wider(pl, names_from = col, names_prefix = "complete_obs.", values_from = oss_complete)
  fc <- dplyr::select(TTdata4d, first_cols)
  join <- dplyr::left_join(fc, pw, by = c("id" = "id", "date" = "date")) %>%
    dplyr::distinct()

  return(join)
}
