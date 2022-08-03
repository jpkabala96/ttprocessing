#' Growth conversion
#' @description Converts the growth DN to a measurment in cm according to the 
#'   equation in the Tree Talkers manual: 
#'   distanceCM <- (237908.4541-(growthDN*1.1171))/(199.4330+growthDN).
#'   The sign is reversed, as it indicates the distance of the sensor 
#'   from the tree, that decreases in time. 
#' @param growthDN The raw value from the growth sensor (the DN). A numeric.
#' @export


growthConversion <- function(growthDN){
  growthDN <- as.numeric(growthDN)#ensure the DN is expressed as double,
  #to do properly the calculations
  distanceCM <- (237908.4541-(growthDN*1.1171))/(199.4330+growthDN)
  #reverse the sign
  distanceCM <- -distanceCM
  return(distanceCM)
}
