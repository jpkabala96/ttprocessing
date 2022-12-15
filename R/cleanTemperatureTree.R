#' Clean the temperature recorded inside the trunk
#' @name cleanTemperatureTree
#' @description For the purpose of measuring the sap flow, temperature is measured
#'   by two probes put inside the trunk. Those values are used later for calculating
#'   the sap flow. For a correct calculation it is important to remove values
#'   that are not plausible. We have chosen to remove temperatures under 0 degrees 
#'   C and
#'   above 40 degrees C, but the function allows the user to choose manually the range
#'   of temperature he regards as acceptable. Values outside the range are replaced
#'   with NA.
#' @param x Temperature in degrees C, as obtained by \code{convertTemperature}. A
#'   numeric vector.
#' @param lower.TTree A value for the lowest temperature value accepted.
#' @param higher.TTree A value for the highest temperature value accepted.
#' @return A numeric vector, with values outside the range replaced with NA's.
#' @export


cleanTemperatureTree <- function(x, 
                                 lower.TTree = -10, 
                                 higher.TTree = 40){
  x[x < lower.TTree]<- NA
  x[x > higher.TTree] <- NA
  return(x)
}
