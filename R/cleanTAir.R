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
#'
#' @examples
#' # Remove temperature values out of range
#' #and replace with NA
#' #The user doesn't need to call this function
#' #as it works embedded in clean4DData
#' \dontrun{
#' temperature <- sample(c(-20,80), size = 10)
#' print(temperature)
#'  cleanTAir(temperature, 
#'  lower.TAir = -5, 
#'  higher.TAir = 35)
#' }
#' 
#' 
#' @export


cleanTAir <- function(x, lower.TAir = -15, higher.TAir = 50){
  x <- as.numeric(x)
  x[x > higher.TAir] <- NA
  x[x < lower.TAir] <- NA
  return(x)
}

