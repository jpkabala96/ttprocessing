#' Clean the raw Ecf Hz values
#' @description Clean the Ecf Hz sensor values, removing those less than 10000
#'   and higher than 50000. This is true for 50 kHz sensor. A later version 
#'   will include also the other sensors.
#' @param Ecf_Hz The numeric values of Ecf.
#' 
#' @examples
#' # Internal function of the package,
#' # Not meant to be run by the users
#' \dontrun{
#' ecf_example <- sample(c(8000:100000), size = 10)
#' print(ecf_example)
#' cleanEcfHz(ecf_example)
#' }
#' 
#' @export

cleanEcfHz <- function(Ecf_Hz){
  Ecf_Hz[Ecf_Hz < 10000] <- NA
  Ecf_Hz[Ecf_Hz > 50000] <- NA
  return(Ecf_Hz)
}