#' Join tree attributes
#' @description Function that allows to join the TT+ data with a table of
#'   tree attributes. The Tree attributes table must include a column labelled
#'   id, with the Tree talker id associated with each tree. Can contain an
#'   arbitrary number of columns of other data.
#' @param TT_data A data frame with the Tree Talkers 4D data, like the one returned
#'   by \code{clean4DData}. Must include a column named id, with the TT ids
#' @param tree_attributes A data frame with the tree data to join. Must contain
#'   a column id with the TT ids.
#'   @return A joined data.frame, with suffix .TT on the columns from the TT data,
#'   and suffix .tree on the columns from the tree attributes.
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' clean_4d <- clean4DData(raw4d_ex_data)
#' ids <- unique(clean_4d$id)
#' example_ancillary_data <- data.frame(
#' id = ids, 
#' var1 = rnorm(length(ids), 1,0),
#' var2 = runif(length(ids), 10, 30)
#' ) 
#' print(example_ancillary_data)
#' colnames(example_ancillary_data)
#' join_result <- joinTreeAttributes(clean_4d,
#' example_ancillary_data)
#' colnames(join_result)
#' print(join_result)
#' }
#' 
#'
#' @export

joinTreeAttributes <- function(TT_data, tree_attributes){
  assertthat::assert_that("id" %in% names(tree_attributes),
                          msg = "tree_attributes must contain a
                          column named \"id\" with the tree talkers ids" )
  assertthat::assert_that("id" %in% names(TT_data),
                          msg = "TT_data must include the id column")
  join <- dplyr::left_join(TT_data, tree_attributes,
                           by = c("id" = "id"),
                           suffix = c(".TT", ".tree"))
  return(join)
}
