#' Clean Air Temperature
#'
#' @description Cleans the air temperatures recorded by the Tree Talker, replacing
#'   with NA's the values outside the range specified by lower.TAir and higher.TAir.
#' @param x An air temperature vector of class numeric.
#' @param lower.TAir A value below which temperature is regarded as non plausible,
#'   and thus replaced with NA.
#' @param higher.TAir A value above which temperature is regarded as non plausible,
#'   and thus replaced with NA.
#' @return A numeric vector, with non plausible temperature values replaced by NA's.
#' @export


cleanTAir <- function(x, lower.TAir = -15, higher.TAir = 50){
  x <- as.numeric(x)
  x[x > higher.TAir] <- NA
  x[x < lower.TAir] <- NA
  return(x)
}

