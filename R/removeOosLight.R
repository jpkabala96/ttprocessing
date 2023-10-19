#' Remove invalid light values
#' @description This function removes the values that are not valid from the
#'   dataset (the values higher than 70.000).
#'   
#' @examples
#' \dontrun{
#' example_values <- sample(c(1:100000), size = 30)
#' print(example_values)
#' removeOosLight(example_values)
#' }
#' 
#' @export
#'

removeOosLight <- function(x){
  #remove values greater than 70 000 as they are out of scale
  x[x>65000]<- NA
  return(x)
}
