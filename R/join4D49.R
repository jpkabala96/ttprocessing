#' Join string 4D with string 49
#' @description Joins the string 4D and the string 49 by left join, using the
#'   \code{dplyr} \code{left_join} function. Joins by id and date_hour of the
#'   observation.
#' @param data4D The 4d data data.frame.
#' @param data49 The 49 data data.frame.
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
