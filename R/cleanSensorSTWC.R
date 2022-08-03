#' Clean the raw data of the STWC sensor
#' @description The raw stem water content data (the D.N. recorded by the Tree
#'   Talker) can be considered only if in the range between 7000 and 14000, according
#'   to the manual. So values outside this range are removed by this function.
#'   This function is mainly a helper of \code{clean4DData} but can also
#'   be called manually by the user while cleaning data by hand.
#' @param x Data recorded by the Stem Water Content sensor of the Tree Talker. A numeric.
#' @return A numeric with the data in input, with values outside the range of
#'   validity replaced by NA.
#'   @example
#'   STWC_DN <- c(2000, 7800, 8399, 13890, 17900)
#'   cleanSensorSTWC(STWC_DN)
#'




cleanSensorSTWC <- function(x){
  x <- as.numeric(x)
  x[x > 50000] <- NA
  x[x < 10000] <- NA
  return(x)
}
