#' Plot data by quarter of the year
#' @description This function plots the data by quarter of the year, similarly
#'   to how plotByMH works.
#' @param clean_data The data frame of clean data obtained with \code{clean4DData}
#'   or \code{h24Clean}.
#' @param variable The variable to be plotted
#' @param statistic \code{"median"} or \code{"mean"}. Defaults to median. The
#'   statistic to be plotted.
#' @return A ggplot object. Additional layers can be added to it.
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' clean_data <- clean4DData(raw4d_ex_data,
#' lower.TTree = 0,
#' higher.TTree = 40,
#' lower.TAir = -5,
#' higher.TAir = 40)
#' plotByQuarter(clean_data, variable = "do_sap_flow")
#' plotByQuarter(clean_data, variable = "Tair", statistic = "mean")
#' }
#' @export
#'

plotByQuarter <- function(
  clean_data,
  variable = "do_sap_flow",
  statistic = "median"){
  #create the summary
  var_summary <- clean_data %>%
    dplyr::group_by(yq, f_hour) %>%
    dplyr::summarise("mean.{variable}" := mean(eval(parse(text = variable)), na.rm = T),
                     "median.{variable}" := median(eval(parse(text = variable)), na.rm = T)) %>%
    dplyr::ungroup()
  #create the median plot
  median_plot <- ggplot2::ggplot()+
    ggplot2::geom_col(data = var_summary,
                      ggplot2::aes(x = f_hour,
                                   y = eval(parse(text = paste0("median.", variable)))))+
    ggplot2::theme_bw()+
    ggplot2::xlab("Hour")+
    ggplot2::ylab(paste(variable, "median"))+
    ggplot2::facet_wrap(ggplot2::vars(yq))+
    ggplot2::ggtitle(paste(variable, "by hour and month"))
  #create the mean plot
  mean_plot <- ggplot2::ggplot()+
    ggplot2::geom_col(data = var_summary,
                      ggplot2::aes(x = f_hour,
                                   y = eval(parse(text = paste0("mean.", variable)))))+
    ggplot2::theme_bw()+
    ggplot2::xlab("Hour")+
    ggplot2::ylab(paste(variable, "mean"))+
    ggplot2::facet_wrap(ggplot2::vars(yq))+
    ggplot2::ggtitle(paste(variable, "by hour and month"))
  #return the right plot
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
