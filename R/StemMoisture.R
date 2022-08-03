#' Stem Moisture content
#' @description Calculates the Stem Moisture content
#'
#' @export


stemMoistureIndex <- function(data4D){

  SMI <- data4D %>% dplyr::group_by(f_year,id) %>%
    dplyr::summarise(MIN_ECF = min(Ecf_Hz))
  return(SMI)
}
