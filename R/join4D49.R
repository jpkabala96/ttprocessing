#' Join string 4D with string 49
#' @description Joins the string 4D and the string 49 by left join, using the
#'   \code{dplyr} \code{left_join} function. Joins by id and date_hour of the
#'   observation.
#' @param data4D The 4d data data.frame.
#' @param data49 The 49 data data.frame.
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' clean_4d <- clean4DData(raw4d_ex_data,
#' lower.TTree = 0,
#' higher.TTree = 40,
#' lower.TAir = -5,
#' higher.TAir = 40)
#' colnames(clean_4d)
#' processed_49 <- string49Handling(raw4d_ex_data)
#' colnames(processed_49)
#' joined_data <- join4D49(clean_4d, processed_49)
#' #joined data contains now both the 49 and 4D data, joined by id
#' colnames(joined_data) 
#' print(joined_data)
#' }
#' @export


join4D49 <- function(data4D, data49){
  jdata <- dplyr::left_join(data4D %>%
                              dplyr::select(-c(.data$Record)),
                            data49 %>%
                              dplyr::select(-c(.data$date,
                                               .data$f_hour,
                                               .data$hour,
                                               .data$timestamp,
                                               .data$Tipo_stringa,
                                               .data$Record,
                                               .data$Record_number)),
                            by = c("id" = "id",
                                   "date_hour" = "date_hour"))

  return(jdata)
}
