#' Data quality check
#'
#' @description  Checks the data quality by counting the number of daily
#'   valid records each day for each tree talker for each variable.
#'   Returns a data.frame with these data.
#'@export




dailyQualityCheck <- function(dati){
  first_cols <- c("id", "date")
  cols_to_assess <- c("Tref0", "Theat0", "Tref1", "Theat1", "growth08",
                      "growth1.5_9", "growth1.5_5", "voltage", "Tair",
                      "RH", "granier_sap_flow", "do_sap_flow", "vpd", "STWC")
  pl <- dati %>%
    tidyr::pivot_longer(cols = cols_to_assess, names_to = "col", values_to = "value") %>%
    dplyr::group_by(id, date, col) %>%
    dplyr::summarise(oss_complete = sum(!is.na(value)))
  print("pt1 done")
  pw <- tidyr::pivot_wider(pl, names_from = col, names_prefix = "complete_obs.", values_from = oss_complete)
  fc <- dplyr::select(dati, first_cols)
  join <- dplyr::left_join(fc, pw, by = c("id" = "id", "date" = "date"))

  return(join)
}
