#' Convert trunk temperature from DN to degrees (C)
#'
#' @description Converts the temperature measured inside the trunk from DN to Celsius
#'  degrees
#'  using the lookup table of the instrument, that contains, for each possible 
#'  digital number, the corresponding temperature value. 
#' @param x A numeric vector containing the DN of the trunk temperature measurment.
#' @return A numeric vector with the temperatures expressed as Celsius degrees.
#'@export
#'
convertTemperature <- function(x){
  #utils::data(temperatureLookUp, package = "ttprocessing")

  temperatures <- data.frame(temperatureDN = as.character(x)) %>%
    dplyr::left_join(temperatureLookUp, by = c("temperatureDN" = "DNS")) %>%
    dplyr::select(filled) 
  temperatures <- as.numeric(temperatures$filled)
  temperatures <- unlist(temperatures)
  return(temperatures)
  
}
