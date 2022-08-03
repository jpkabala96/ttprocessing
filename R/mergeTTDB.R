#' Merge treetalker databases
#'
#' @description Merge two TT databases by rbinding them before processing.
#'   First ensures they have the same row names (and so the same length).
#' @param rawdata1 First database.
#' @param rawdata2 Second database.
#'
#' @export

mergeTTDB <- function(rawdata1, rawdata2){
  #check if colnames are the same
  #assertthat::assert_that(names(clean_data1) == names(clean_data2))
  data <- rbind(rawdata1, rawdata2)
  return(data)
}
