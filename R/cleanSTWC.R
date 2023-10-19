#' Clean Stem Water Content converted data
#'
#' @description This function cleans the converted Stem Water Content data,
#'   removing values that are regarded to be not plausible. By default the
#'   function regards as not valid values lower than 0 and higher than 100,
#'   that are certainly not true, as the Stem Water Content is a percentage.
#'   The user can manually specify the lowest and highest values accepted, by
#'   specifying the correspondig parameters.
#'   This is mainly a helper function of \code{clean4DData}, but can also be used for
#'   handling the data manually.
#' @param x Stem Water Content data converted to percentage.
#' @param lower.VWC Lowest STWC value accepted. Defaults to 0.
#' @param higher.VWC Highest STWC value accepted. Defaults to 100.
#' 
#' @return Stem water content: a numeric with values below \code{lower.STWC} and above
#' \code{higher.STWC} replaced with NA.
#' 
#' @examples
#' # example code
#' \dontrun{
#' stem_water <- sample(c(1:100), size = 10)
#' print(stem_water)
#' cleanSTWC(stem_water)
#' }
#' @export








cleanSTWC <- function(x, lower.VWC = 0, higher.VWC = 100){
  x[x > higher.VWC] <- NA #il contenuto di acqua deve essere compreso tra 0 e 100
  x[x < lower.VWC] <- NA
  return(x)
}
