#' Filter IDs
#' @description Function for filtering only certain ids from the dataset. Based 
#'   on the dplyr::filter().
#' @param data4D The data frame with 4D data
#' @param ids A vector with the ids of interest
#' 
#' 
#' @export

filterIDs <- function(data4D, ids = ""){
  if(length(ids) >= 1){
  if(ids[[1]] != ""){
    data4D <- data4D %>% dplyr::filter(id %in% ids)
  }
  return(data4D)
}


