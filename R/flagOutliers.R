#' Flag outliers
#' @description This function creates new columns in the TT+ data dataframe,
#'   that are outlier flags. It exploits the detectOutliers function, which 
#'   applies the boxplot rule. Data are grouped by hour and by week or month,
#'   and then the function in applied. 
#' @param TTdata Dataframe with TreeTalker data
#' @param variables Variables to which to apply the function.
#' @param week Flag (\code{TRUE or FALSE}). if TRUE data are grouped by week, 
#'   else are grouped by month for applying the procedure. 
#' @export
#' 

flagOutliers <- function(TTdata, variables, week = T){
  
  if(week == T){
    for(i in 1:length(variables)){
      TTdata <- TTdata %>% 
        dplyr::mutate(week = as.character((format(.data$date, "%W"))),
                      year = lubridate::year(.data$date),
                      f_wy = paste(year, week, sep = "_"))
      quantiles <- TTdata %>%
        dplyr::group_by(.data$f_wy,
                        .data$f_hour,
                        .data$id) %>% 
        dplyr::summarise("Q25_{variables[[i]]}" := stats::quantile(eval(parse(text = variables[[i]])), 0.25, na.rm = T),
                         "Q75_{variables[[i]]}" := stats::quantile(eval(parse(text = variables[[i]])), 0.75, na.rm = T)) %>%
        dplyr::ungroup() 
      print("quantiles 1 done week")
      quantiles <- quantiles %>% 
        dplyr::mutate(
          "IQR_{variables[[i]]}" := eval(parse(text = paste("Q25",variables[[i]], sep = "_")))  - eval(parse(text = paste("Q75",variables[[i]], sep = "_"))),
          "QMAX_{variables[[i]]}" := eval(parse(text = paste("Q75",variables[[i]], sep = "_"))) + 1.5 * eval(parse(text = paste("IQR",variables[[i]], sep = "_"))),
          "QMIN_{variables[[i]]}" := eval(parse(text = paste("Q25",variables[[i]], sep = "_"))) - 1.5 * eval(parse(text = paste("IQR",variables[[i]], sep = "_"))))
      print("quantiles 2 done week")
      TTdata <- dplyr::left_join(TTdata, quantiles, by = c("f_wy" = "f_wy",
                                                           "f_hour" = "f_hour",
                                                           "id" = "id")) 
      print("Join done")
      TTdata <- TTdata %>%
        dplyr::mutate("{variables[[i]]}_o" := ifelse(
          eval(parse(text = variables[[i]])) < eval(parse(text = paste("QMIN", variables[[i]], sep = "_"))),
          T,
          ifelse(eval(parse(text = variables[[i]])) > eval(parse(text = paste("QMAX", variables[[i]], sep = "_"))),
                 T,
                 F)))#creare la variabile flag
    }
    print(paste0(variables[[i]]," band done"))
    
    
  }
  if(week != T){
    for(i in 1:length(variables)){
      TTdata <- TTdata %>% 
        dplyr::mutate(f_month = as.character(lubridate::month(.data$date)),
                      year = lubridate::year(.data$date),
                      f_my = paste(year, f_month, sep = "_"))
      quantiles <- TTdata %>%
        dplyr::group_by(.data$f_my, 
                        .data$f_hour,
                        .data$id) %>% 
        dplyr::summarise("Q25_{variables[[i]]}" := stats::quantile(eval(parse(text = variables[[i]])), 0.25, na.rm = T),
                         "Q75_{variables[[i]]}" := stats::quantile(eval(parse(text = variables[[i]])), 0.75, na.rm = T)) %>%
        dplyr::ungroup()
      print("quantiles1 done month")
      quantiles <- quantiles %>% 
        dplyr::mutate(
          "IQR_{variables[[i]]}" :=   eval(parse(text = paste("Q75",variables[[i]], sep = "_"))) - eval(parse(text = paste("Q25",variables[[i]], sep = "_"))),
          "QMAX_{variables[[i]]}" := eval(parse(text = paste("Q75",variables[[i]], sep = "_"))) + 1.5 * eval(parse(text = paste("IQR",variables[[i]], sep = "_"))),
          "QMIN_{variables[[i]]}" := eval(parse(text = paste("Q25",variables[[i]], sep = "_"))) - 1.5 * eval(parse(text = paste("IQR",variables[[i]], sep = "_")))) %>%
        dplyr::ungroup()
      print("quantiles 2 done month")
      TTdata <- dplyr::left_join(TTdata, quantiles %>% dplyr::select(-tidyselect::starts_with("Q7"),
                                                                     -tidyselect::starts_with("Q2"),
                                                                     -tidyselect::starts_with("IQ")), 
                                 by = c("id" = "id",
                                        "f_hour" = "f_hour",
                                        "f_my" = "f_my")) %>%
        dplyr::mutate("{variables[[i]]}_o" := ifelse(
          eval(parse(text = variables[[i]])) < eval(parse(text = paste("QMIN", variables[[i]], sep = "_"))),
          T,
          ifelse(eval(parse(text = variables[[i]])) > eval(parse(text = paste("QMAX", variables[[i]], sep = "_"))),
                 T,
                 F)))#creare la variabile flag
      print(paste0(variables[[i]], " band done"))
    }
    
  }
  TTdata<- TTdata %>% dplyr::select(-tidyselect::starts_with("Q")) %>%
    dplyr::select(-tidyselect::starts_with("IQR"))
  return(TTdata)
}

