#' Vapour Pressure Deficit
#' @description This function calculates Vapour Pressure Deficit using the air
#'   temperature and relative humidity data recorded by the Tree Talker. It takes
#'   as input two numeric vectors.
#' @param Tair A numeric vector containing the Tair values
#' @param RH A numeric vector containing relative humidity data. Must be the same length
#'   as Tair.
#'
#'
#' @export
#'

VPD <- function(Tair, RH){
  #input control
  assertthat::assert_that(is.numeric(Tair),
                          msg = "Tair must be a numeric vector")
  assertthat::assert_that(is.numeric(RH),
                          msg = "RH must be a numeric vector")
  assertthat::assert_that(length(Tair) == length(RH),
                          msg = "The two arguments must be the same length")
  #calculation
  vpd <- ((6.1078*exp((17.08085*Tair)/(234.175+Tair)))-(6.1078*exp((17.08085*Tair)/(234.175+Tair)))*RH/100)/10
  return(vpd)

}
