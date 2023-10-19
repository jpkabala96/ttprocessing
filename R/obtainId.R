#' Obtain TT ID
#'
#' @description This function retrieves the devices' ids from the first column of
#'   the raw Tree Talkers data. It takes as argument the first column of the TT data.
#'   returned by the function \code{readTTData}. The ids are returned
#'   as a character vector. This function works within the \code{isolateTT()}
#'   function, but can also be called manually.
#' @param x The string V1 of the tree talkers data, that contains the server
#'   date-time and the id of the Tree Talker.
#' @return A character vector with the devices' ids.
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' ids <- obtainId(raw4d_ex_data$V1)
#' print(ids)
#' }
#' 
#' @export


obtainId<- function(x){
  stringa_rotta <- x %>% stringr::str_split(",") %>% unlist()
  indices <- c(1:length(x)) * 2 # prende gli elementi pari (gli id)
  ids <- stringa_rotta[indices]
  return(ids)
}
