#' Clean the raw Ecf Hz values
#' @description Clean the Ecf Hz sensor values, removing those less than 10000
#'   and higher than 50000. This is true for 50 kHz sensor. A later version 
#'   will include also the other sensors. 
#' @export

cleanEcfHz <- function(Ecf_Hz){
  Ecf_Hz[Ecf_Hz < 10000] <- NA
  Ecf_Hz[Ecf_Hz > 50000] <- NA
  return(Ecf_Hz)
}