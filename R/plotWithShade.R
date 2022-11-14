#' Plot shaded Timeseries
#'
#' @description Plots the timeseries of TT+ data with nights shaded, by using
#'   the \code{plotTS} function, adding some ggplot layers and retrieving
#'   sunrise and sunset from the suncalc library.
#' @param data The clean 4D data of the tree talkers
#' @param variable The variable to be plotted. Same as plot TS (this is a
#' a variant of the plotTS function)
#' @param statistic The summary arg. Must be one of "mean" or "median".
#'   The default is "median".
#' @param lat Latitude at which is located the study site (in degrees).
#' @param lon Longitude at which is located the study site (in degrees).
#' @export
#'

plotWithShade<-  function(data,
                          variable = "do_sap_flow",
                          statistic = "median",
                          lat = 40,
                          lon = 14){

  sun <- suncalc::getSunlightTimes(data$date,
                                   lat = lat,
                                   lon = lon) %>%
    dplyr::distinct()
  
  plt <- plotTS(data = data, variable = variable, statistic = statistic) +
    ggplot2::geom_rect(data = sun, ggplot2::aes(xmin = as.POSIXct(.data$date),
                                                xmax = .data$sunrise,
                                                ymin = -Inf,
                                                ymax = Inf),
                       fill = "#222222", alpha = 0.5) +
    ggplot2::geom_rect(data = sun, ggplot2::aes(xmin = .data$sunset,
                                                xmax = as.POSIXct(.data$date + as.difftime(1, units = "days")),
                                                ymin = -Inf,
                                                ymax = Inf),
                       fill = "#222222", alpha = 0.5)
  return(plt)
}
