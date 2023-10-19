#' Merge treetalker databases
#'
#' @description Merge two TT databases by rbinding them before processing.
#'   First ensures they have the same row names (and so the same length).
#' @param rawdata1 First database.
#' @param rawdata2 Second database.
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' #simulate two databases by splitting the example data in 2 part
#' example_db1 <- raw4d_ex_data[V4 < 1623837601,]
#' example_db2 <- raw4d_ex_data[V4 >= 1623837601,]
#' nrow(example_db1)
#' nrow(example_db2)
#' summary(example_db1)
#' summary(example_db2)
#' merged_database <- mergeTTDB(example_db1, example_db2)
#' print(merged_database)
#' summary(merged_database)
#' }
#' 
#' @export

mergeTTDB <- function(rawdata1, rawdata2){
  #check if colnames are the same
  #assertthat::assert_that(names(clean_data1) == names(clean_data2))
  data <- rbind(rawdata1, rawdata2)
  return(data)
}
