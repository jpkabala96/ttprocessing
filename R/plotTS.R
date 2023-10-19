#' Plot the time series
#' @description This function plots the time series of the variable selected.
#'   It can be run two ways: by summarising the data of all the TTs with a
#'   single statistic (mean or median) and plotting a single line, or by
#'   plotting the data of each TT as a separate coloured line.
#'   Also the time series of different sites can be plotted. In that case
#'   only a summary statistic is plotted for each site, with lines of a
#'   different color for each site.
#' @param data A data frame with the tree talkers 4D data, as the output of
#'   the \code{read4DData} function, or furtherly modified data.
#' @param statistic A string with value "mean", "median" or "none". Specifies if
#'   data shall be grouped and summarised by mean, median, or shall not be
#'   summarised. Defaults to median, as we recommend to use the median
#'   for the TT data, as a robust indicator.
#' @param variable A chararacter string which specifies a valid variable
#'   name in the data data.frame supplied to the function.
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' clean_data<- clean4DData(raw4d_ex_data,
#' lower.TTree = 0,
#' higher.TTree = 40,
#' lower.TAir = -5,
#' higher.TAir = 40)
#' plotTS(clean_data, variable = "Tair", statistic = "median")
#' filtered_data <- filterByDate(clean_data, "2021-05-10", "2021-05-20")
#' plotTS(filtered_data, variable = "do_sap_flow", statistic = "none")
#' }
#'@export
#'

plotTS <- function(data,
                   variable = "do_sap_flow",
                   statistic = "median")   {
  # assertthat::assert_that(assertthat::is.flag(sites),
  #                         msg = "sites must be a flag (TRUE or FALSE)")
  # assertthat::assert_that(assertthat::is.flag(sd),
  #                         msg = "sd must be a flag (TRUE or FALSE).
  #                         It indicates whether sd must be added to the plot
  #                         if mean is plotted.")
    summaries <- data %>%
      #dplyr::mutate(time = as.factor(lubridate::make_datetime(year = lubridate::year(.data$date_hour),
      #                                                        month = lubridate::month(.data$date_hour),
      #                                                        day = lubridate::day(.data$date_hour),
      #                                                        hour = lubridate::hour(.data$hour)))) %>%
      dplyr::mutate(time = .data$date_hour) %>%
      dplyr::group_by(.data$time) %>%
      dplyr::summarise("median.{variable}" := median(eval(parse(text = variable)), na.rm = T),
                       "mean.{variable}" := mean(eval(parse(text = variable)), na.rm = T),
                       "sd.{variable}" := sd(eval(parse(text = variable)), na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(time = as.POSIXct(.data$time))


    median_plot <- ggplot2::ggplot()+
      ggplot2::geom_line(data = summaries,
                         ggplot2::aes(x = .data$time,
                                      y = eval(parse(text  = paste0("median.", variable)))))+
      ggplot2::ggtitle(paste(variable, "median"))+
      ggplot2::theme_bw()
    mean_plot <- ggplot2::ggplot()+
      ggplot2::geom_line(data = summaries,
                         ggplot2::aes(x = .data$time,
                             y = eval(parse(text  = paste0("mean.", variable)))))+
      ggplot2::ggtitle(paste(variable, "mean"))+
      ggplot2::theme_bw()

  if(statistic == "none"){
    all_plot <- ggplot2::ggplot()+
      ggplot2::geom_line(data = data, ggplot2::aes(x = date_hour,
                                                   y = eval(parse(text = variable)),
                                                   color = id))+
      ggplot2::ggtitle(variable)+
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
