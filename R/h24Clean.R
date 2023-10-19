#' Removes days with NA's recorded during the day
#' @description Removes the data for all the days where there are only few
#'   complete records. The number of not NA records to retain the data of that
#'   day is to be specified in the complete_required argument (defaults to 24).
#'   Recommended to run this function on the Sap Flow Data.
#'   This because the Sap Flow estimation depends on the delta t max recorded
#'   during all the day by the probes. Invalid values, or not complete observation
#'   Might lead to a bad estimation of the Sap Flow.
#'   Can be applied also to the other columns.
#' @param data The data.frame of 4D data to be cleaned.
#' @param cols_to_clean A character vector with the names of the columns to clean.
#' @param complete_required Complete observations during the day, to retain the
#'   values recorded during that day. Defaults to 24 (all the records during
#'    that day must be complete to retain the data of that day)
#'    
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' clean_data <- clean4DData(raw4d_ex_data)
#' h24_cleaned1 <- h24Clean(clean_data)
#' View(h24_cleaned1)
#' h24_cleaned2 <- h24Clean(clean_data, variables = c("do_sap_flow", "Tair"))
#' View(h24_cleaned2)
#' }
#' 
#' 
#' @importFrom rlang na_dbl
#' @import suncalc
#'
#' @export

h24Clean <- function(data, cols_to_clean = "do_sap_flow", complete_required = 24){
  # assertthat::assert_that(is.character(cols_to_clean),
  #                         msg = glue::glue("cols_to_clean is a {class(cols_to_clean)\n}
  #                                          It must be a character."))
  # assertthat::assert_that(length(cols_to_clean >=1),
  #                         msg = "cols_to_clean must be a character of legnth >= 1")
  # assertthat::assert_that(is.scalar(complete_required),
  #                         msg = glue::glue("complete_required is a {class(complete_required)\n
  #                                          It must be a numeric of length 1.}"))
  pl <- data %>%
      tidyr::pivot_longer(cols = cols_to_clean,
                   names_to = "col",
                   values_to = "value") %>%
      dplyr::group_by(id, date, col) %>%
      dplyr::summarise(oss_complete = sum(!is.na(value)))
    print("pt1 done")
    pw <- tidyr::pivot_wider(pl, names_from = col,
                             names_prefix = "oss_complete.",
                             values_from = oss_complete)
    join <- dplyr::left_join(data,
                             pw,
                             by = c("id" = "id", "date" = "date")) %>%
      as.data.frame()
    for(i in 1:length(cols_to_clean)) {
      print(class(join[,cols_to_clean[[i]]]))
      join[,cols_to_clean[[i]]] <- dplyr::if_else(eval(parse(text = paste0("join$", "oss_complete.", cols_to_clean[[i]]))) < complete_required,
                                          na_dbl,
                                          as.numeric(join[,cols_to_clean[[i]]]),)
      }
  join <- dplyr::select(join, -paste0("oss_complete.", cols_to_clean))
  print(paste0("Cols cleaned: \n", paste(cols_to_clean, sep = "\n", collapse = " ")))
  return(join)
}



