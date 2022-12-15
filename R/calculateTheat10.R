#' Calculate Theat 10 
#' 
#' @description Calculates the temperature of the trunk 10 minutes after the 
#'   heating start, as it was if no heating from the probe occurred. The value is 
#'   computed by linear interpolation with Theat0 of the following hour. The procedure 
#'   allows for correcting for changes in trunk temperature, due to daily cycles.
#' @param data Data of a single TT+ devices, arranged by \code{date_hour}.
#' @return The same data.frame as data, plus a column called \code{Theat10} 
#'   containing the estimated value.
#' @export
#' 
calculateTheat10 <- function(data){
  data <- dplyr::mutate(data, 
                        Theat10 = (Theat0*50 + 10*dplyr::lead(Theat0, n = 1))/60)
  return(data)
}