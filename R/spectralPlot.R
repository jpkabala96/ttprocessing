#' TT+ spectral plot
#' @description Function for creating a spectral plot of the TT+ data. The 
#'   spectral plot is a plot which has on the x axis the wavelength of the bands
#'   and on the Y axis the energy amount (microWatt/cm^2).
#' @param data49 A data.frame with light data from the string 49, as the one produced 
#'   by the string49Handling function.
#' @param id The Tree talker of interest ID (numeric).
#' @param year The year of interest (numeric).
#' @param month The month of interest (numeric).
#' @param day The day of interest (numeric).
#' @param hour The hour of interest (numeric).
#' @param tz The time zone of the place where the TT+ are installed. Default
#'   value: \code{Sys.timezone} the system timezone.
#' @param print a flag, set TRUE to print the plot, FALSE to only return it
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' data49 <- string49Handling(raw4d_ex_data, tz = "CET")
#' spectralPlot(data49, id = "11111111", year = 2021, month = 5, day = 17, hour = 10)
#' spectralPlot(data49, id = "22222222", year = 2021, month = 5, day = 18, hour = 9)
#' }
#' 
#' @export
#' 

spectralPlot <- function(data49, 
                         id,
                         year = 2021,
                         month = 6,
                         day = 1,
                         hour = 9,
                         tz = Sys.timezone(),
                         print = T){
  #build time of interest
  time_of_interest <- lubridate::make_datetime(
    year = year,
    month = month,
    day = day,
    hour = hour,
    tz = tz)
  #filter the original data and transpose
  hdata <- data49 %>% 
    dplyr::filter(.data$id == id) %>%
    dplyr::filter(.data$date_hour == time_of_interest) %>%
    dplyr::select(dplyr::starts_with("DN_")) %>%
    as.matrix() %>%
    t() %>% 
    as.data.frame()
  colnames(hdata) <- c("value")
  hdata$wavelength <- as.numeric(substr(row.names(hdata),start =  4, stop = 6))
  #build the plot
  spectral.plot <- ggplot2::ggplot(hdata)+
    ggplot2::geom_point(ggplot2::aes(x = wavelength, y = value))+
    ggplot2::geom_line(ggplot2::aes(x = wavelength, y = value))+
    ggplot2::theme_bw()+
    ggplot2::ggtitle("Spectral plot",
                     subtitle = paste("ID =", id, ", at", time_of_interest))+
    ggplot2::xlab("wavelength (nm)")+
    ggplot2::ylab("microWatt/cm^2")
  #print and return
  if(print == T){
    print(spectral.plot)
  }
  return(spectral.plot)
}