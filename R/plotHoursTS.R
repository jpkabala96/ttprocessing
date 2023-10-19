#' Plot single hours
#'  @description This function allows the user to plot the tree talker time 
#'    series as points. Useful while selecting single specific hours. 
#' @param data The input data (the clean table).
#' @param variable The variable to be represented on the y axis.
#' @param statistic Defaults to "median", a summary statistic to be calculated
#'   from the data. Data are grouped by date.
#'   Can be "median", "mean" or "none". If "none" the data of all the tree 
#'   talkers are displayed.
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' clean_data<- clean4DData(raw4d_ex_data,
#' lower.TTree = 0,
#' higher.TTree = 40,
#' lower.TAir = -5,
#' higher.TAir = 40)
#' filtered_data <- filterByHour(clean_data, hours = c("11", "12", "13"))
#' plotHoursTS(filtered_data, variable = "do_sap_flow", statistic = "none")
#' filtered_data2 <- filterByHour(clean_data, hours = c("13"))
#' plotHoursTS(filtered_data, variable = "Tair", statistic = "median")
#' }
#' @export

plotHoursTS <- function(data,
                        variable = "do_sap_flow",
                        statistic = "median"){
  summaries <- data %>%
    dplyr::mutate(time = as.factor(lubridate::make_datetime(year = lubridate::year(.data$date_hour),
                                                            month = lubridate::month(.data$date_hour),
                                                            day = lubridate::day(.data$date_hour),
                                                            hour = .data$hour))) %>%
    dplyr::group_by(.data$time) %>%
    dplyr::summarise("median.{variable}" := median(eval(parse(text = variable)), na.rm = T),
                     "mean.{variable}" := mean(eval(parse(text = variable)), na.rm = T),
                     "sd.{variable}" := sd(eval(parse(text = variable)), na.rm = T),
                     hour = dplyr::first(f_hour)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(time = as.POSIXct(.data$time))
  
  
  median_plot <- ggplot2::ggplot()+
    ggplot2::geom_point(data = summaries,
                       ggplot2::aes(x = .data$time,
                                    y = eval(parse(text  = paste0("median.", variable))),
                                    color = .data$hour))+
    ggplot2::ggtitle(paste(variable, "median"),
                     subtitle = glue::glue("Hours: {unique(data$f_hour)}"))+
    ggplot2::theme_bw()
  
  mean_plot <- ggplot2::ggplot()+
    ggplot2::geom_point(data = summaries,
                       ggplot2::aes(x = .data$time,
                                    y = eval(parse(text  = paste0("mean.", variable))),
                                    color = .data$hour))+
    ggplot2::ggtitle(paste(variable, "mean"),
                     subtitle = glue::glue("Hours: {unique(data$f_hour)}"))+
    ggplot2::theme_bw()
  
  if(statistic == "none"){
    all_plot <- ggplot2::ggplot()+
      ggplot2::geom_point(data = data, ggplot2::aes(x = date_hour,
                                                   y = eval(parse(text = variable)),
                                                   color = id,
                                                   shape = .data$hour))+
      ggplot2::ggtitle(variable,
                       subtitle = "Hours: {unique(data$f_hour)}")+
      ggplot2::theme_bw()
  }
  M1 <- glue::glue("Median of {variable} plotted")
  M2 <- glue::glue("Mean of {variable} plotted")
  M3 <- glue::glue("All TTs plotted")
  switch (statistic,
          none = {
            print(M3)
            print(all_plot)
            return(all_plot)},
          mean = {
            print(M2)
            print(mean_plot)
            return(mean_plot)
          },
          median = {
            print(M1)
            print(median_plot)
            return(median_plot)}
  )
}