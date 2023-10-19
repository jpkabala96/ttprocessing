#' Plot 4D data by month and hour
#' @description This function is a wrapper around ggplot, to display
#'   the TT data by month and hour, helping in EDA. The function returns
#'   a ggplot object, to which can be added other layers such as those to
#'   specify the theme. The plot is also printed.
#' @param clean_data A data frame with TT data, as those created by the
#'   functions of this package as for example \code{read4DData}.
#' @param variable The variable to plot. It must be one of the names of the
#'   input data.frame.
#' @param statistic The statistic to be plotted. Can be one of "mean" and
#'   "median". We have chosen "median" as default, as the TT data aren't always
#'   very clean, so the median will be a more robust indicator, and we suggest
#'   to use it.
#' @details The summary statistic is calculated with na.rm = T, as it is very
#'   likely that the data will contain NA.
#' @return a ggplot object, that can be further customized.
#'
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' clean_data <- clean4DData(raw4d_ex_data,
#' lower.TTree = 0,
#' higher.TTree = 40,
#' lower.TAir = -5,
#' higher.TAir = 40)
#' plotByMH(clean_data, variable = "do_sap_flow")
#' plotByMH(clean_data, variable = "Tair", statistic = "mean")
#' }
#'
#'@export

plotByMH <- function(clean_data, variable = "do_sap_flow", statistic = "median"){
  assertthat::assert_that(assertthat::is.string(variable),
                          msg = "variable must be one of the columns of
                          the data frame")
  assertthat::assert_that(variable %in% names(clean_data),
                          msg = "variable must be one of the columns of
                          the data frame")
  assertthat::assert_that(assertthat::is.string(statistic),
                          msg = "statistic must be a string,
                          one of 'mean' and 'median'")
  var_summary <- clean_data %>%
    dplyr::group_by(f_year, f_month, f_hour) %>%
    dplyr::summarise("mean.{variable}" := mean(eval(parse(text = variable)), na.rm = T),
                   "median.{variable}" := median(eval(parse(text = variable)), na.rm = T)) %>%
    dplyr::ungroup()
  print(names(var_summary))
  names(var_summary) <- gsub("[(]",".", names(var_summary))
  names(var_summary) <- gsub("[)]","", names(var_summary))
  print(names(var_summary))
  median_plot <- ggplot2::ggplot()+
    ggplot2::geom_col(data = var_summary,
                      ggplot2::aes(x = f_hour,
                                   y = eval(parse(text = paste0("median.", variable)))))+
    ggplot2::theme_bw()+
    ggplot2::xlab("Hour")+
    ggplot2::ylab(paste(variable, "median"))+
    ggplot2::facet_wrap(ggplot2::vars(f_year, f_month))+
    ggplot2::ggtitle(paste(variable, "by hour and month"))

  mean_plot <- ggplot2::ggplot()+
    ggplot2::geom_col(data = var_summary,
                      ggplot2::aes(x = f_hour,
                                   y = eval(parse(text = paste0("mean.", variable)))))+
    ggplot2::theme_bw()+
    ggplot2::xlab("Hour")+
    ggplot2::ylab(paste(variable, "mean"))+
    ggplot2::facet_wrap(ggplot2::vars(f_year, f_month))+
    ggplot2::ggtitle(paste(variable, "by hour and month"))
  switch (statistic,
          mean = {
            print(mean_plot)
            return(mean_plot)
          },
          median = {
      print(median_plot)
      return(median_plot)
    }
  )
}
