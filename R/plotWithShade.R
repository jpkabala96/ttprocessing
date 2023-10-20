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
#' 
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' clean_data<- clean4DData(raw4d_ex_data,
#' lower.TTree = 0,
#' higher.TTree = 40,
#' lower.TAir = -5,
#' higher.TAir = 40)
#' filtered_data <- filterByDate(clean_data, "2021-05-10", "2021-05-20")
#' plotWithShade(filtered_data, 
#' variable = "Tair", 
#' statistic = "mean",
#' lat = 40, #when working with your data place correct values of latitude
#' lon = 14) #and longitude of your study site
#' 
#' plotWithShade(filtered_data, 
#' variable = "do_sap_flow", 
#' statistic = "none",
#' lat = 40, #when working with your data place correct values of latitude
#' lon = 14) #and longitude of your study site
#' }
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
