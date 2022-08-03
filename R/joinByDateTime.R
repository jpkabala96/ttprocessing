#' Join TT data with other temporal data
#'
#' @description This function facilitates the join of the TT data with other
#'   data recorded every hour. This fuction rounds the records in both
#'   dataframes to an hourly resolution and then joins them by date and hour by
#'   left join, so keeping all the rows of TT data, and leaving NA's where the
#'   second data.frame doesn't have records.
#'   The variables in the returned data.frame have the suffix ".TT" for the
#'   TT variables, and ".data" for the other data Joined.
#' @param TT_data The clean 4D Data of the tree talkers.
#' @param other_data The other dataframe to be joined to the TT data.
#' @param datetime_name The name of the column indicating date_time in the
#'   other data.
#' @return A data.frame, with the joined data.
#'
#'
#' @export

joinByDateTime <- function(TT_data, other_data, datetime_name){
  assertthat::assert_that(is.data.frame(TT_data),
                          msg = "TT data must be a data.frame with the
                          Tree Talkers data.")
  assertthat::assert_that(is.data.frame(other_data),
                          msg = "other data must be a data.frame")
  assertthat::assert_that(assertthat::is.string(datetime_name),
                          msg = "datetime_name must be a string, specifying
                          the name of the variable in the second data frame
                          that contains the date-time of the records.")
  assertthat::assert_that(datetime_name %in% names(other_data),
                          msg = "datetime_name must be one of the colnames of
                          the dataframe provided as 'other_data'")
  #togliere minuti e secondi.

  joined_data <- dplyr::left_join(TT_data,
                                  other_data,
                                  by = c("date_hour" = datetime_name),
                                  suffix = c(".TT", ".data"))
  return(joined_data)



}
