#' Clean Relative Humidity
#'
#' @description This function removes the relative humidity values outside the
#'   range regarded as plausible. This range has been set as default to 35 as
#'   lowest value and 95 as highest value accepted. Values not accepted are
#'   changed to NA.
#' @param x A vector containing relative air humidity measured by the Tree Talker
#'   a numeric
#' @param lower.RH Values lower than this are changed to NA. Defaults to 35.
#' @param higher.RH Values higher than this are changed to NA. Defaults to 95.
#' @return A "numeric".
#' @export




cleanRH <- function(x, lower.RH = 35, higher.RH = 95){
  x <- as.double(x)
  x[x > higher.RH] <- NA
  x[x < lower.RH] <- NA
  return(x)
}
