#' Convert trunk temperature from DN to °C
#'
#' @description Converts the temperature measured inside the trunk from DN to °C
#'  following the equation in the TT+ manual. Works mainly as a helper function
#'  inside clean4DData, but can also be called manually.
#' @param x A numeric vector containing the DN of the trunk temperature measurment
#'@export
#'

convertTemperature <- function(x){
  x <- as.double(x)
  t <- 127.6 - (x*0.006045)+(0.000000126*x^2)-(0.00000000000115*x^3)#converto il DN della sonda di riferimento al tempo 0 in temperatura
  return(t)
}
