#' TT+ spectral plot
#' @description Function for creating a spectral plot of the TT+ data. The 
#'   spectral plot is a plot which has on the x axis the wavelength of the bands
#'   and on the Y axis the energy amount (microWatt/cm^2).
#' @param data49 A data.frame with light data from the string 49, as the one produced 
#'   by the string49Handling function.
#' @param year The year of interest (numeric).
#' @param month The month of interest (numeric).
#' @param day The day of interest (numeric).
#' @param tz The time zone of the place where the TT+ are installed. Default
#'   value: \code{Sys.timezone} the system timezone.
#' @param print a flag, set TRUE to print the plot, FALSE to only return it
#' 
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' data49 <- string49Handling(raw4d_ex_data)
#' spectralPlotEnsemble(data49, year = 2021, month = 5, day = 17, tz = "CET")
#' spectralPlotEnsemble(data49,  year = 2021, month = 5, day = 18, tz = "CET")
#' }
#' @export
#' @import ggpubr
#' 
#' 

spectralPlotEnsemble <- function(data49, 
                                 year = 2021,
                                 month = 6,
                                 day = 1,
                                 tz = Sys.timezone(),
                                 print = T){
  day_of_interest <- lubridate::make_date(year = year, month = month, day = day)
  data49 <- dplyr::filter(data49, .data$date == day_of_interest)
  #create a vector with ids
  plotlist <- list()
  ids <- unique(data49$id)
  band_wavelengths <- data49 %>%
    dplyr::select(dplyr::starts_with("DN_")) %>%
    colnames() %>%
    substr(start = 4, stop = 6) %>% 
    as.numeric()
  ##ripristina la conversione in numerico
  
  for(h in 1:24){
    #build time of interest
    time_of_interest <- lubridate::make_datetime(
      year = year,
      month = month,
      day = day,
      hour = h,
      tz = tz)
    #loop over the ids
    df_hour <- data.frame(matrix(data = band_wavelengths, nrow = 12, ncol = 1))
    colnames(df_hour) <- "wavelength"
    print("wave")
    for(i in 1:length(ids)){
      #filter the original data and transpose
      hdata <- data49 %>% 
        dplyr::filter(.data$id == ids[[i]]) %>%
        dplyr::filter(.data$date_hour == time_of_interest) %>%
        dplyr::select(dplyr::starts_with("DN_")) %>%
        as.matrix() %>%
        t() %>% 
        as.data.frame()
      #print(ncol(hdata))
      #print(nrow(hdata))
      if(ncol(hdata) == 0){
        hdata <- data.frame(matrix(data = NA, ncol = 1, nrow = 12))
      }
      colnames(hdata) <- c(paste0("id_",ids[[i]]))
      #print(colnames(hdata))
      #print(hdata)
      hdata <- hdata %>% dplyr::select(dplyr::starts_with("id_"))
      df_hour <- cbind(df_hour,hdata)
    }
    #print(df_hour)
    # df_hour_mean <- df_hour %>%  
    #   dplyr::select(dplyr::starts_with("id_")) %>%
    #   as.matrix() %>% 
    #   apply(MARGIN = 1,FUN = mean, na.rm = T) %>%
    #   unlist()
    # df_hour_mean$mean <- df_hour_mean
    # df_hour_mean$wavelength <- band_wavelengths
    
    df_hour_long <- tidyr::pivot_longer(df_hour , 
                                        cols = dplyr::starts_with("id_"),
                                        names_to = "id",
                                        values_to = "value")
    try({
      plotlist[[h]] <- ggplot2::ggplot()+
        ggplot2::geom_point(ggplot2::aes(x = wavelength,
                                y = value,
                                color = id),
                            data = df_hour_long)+
        ggplot2::geom_line(ggplot2::aes(x = wavelength,
                               y = value,
                               color = id),
                           data = df_hour_long)+
        ggplot2::theme_bw()+
        ggplot2::ggtitle("Spectral plot",
                         subtitle = time_of_interest)})
  }
  ggpubr::ggarrange(plotlist[[1]],
                    plotlist[[2]],
                    plotlist[[3]],
                    plotlist[[4]],
                    plotlist[[5]],
                    plotlist[[6]],
                    plotlist[[7]],
                    plotlist[[8]],
                    plotlist[[9]],
                    plotlist[[10]],
                    plotlist[[11]],
                    plotlist[[12]],
                    plotlist[[13]],
                    plotlist[[14]],
                    plotlist[[15]],
                    plotlist[[16]],
                    plotlist[[17]],
                    plotlist[[18]],
                    plotlist[[19]],
                    plotlist[[20]],
                    plotlist[[21]],
                    plotlist[[22]],
                    plotlist[[23]],
                    plotlist[[24]],
                    ncol = 2, 
                    nrow = 12,
                    common.legend = T,
                    legend = "bottom")
  #save as pdf, with name identifying day, month year and id
  #quite large page to contain all the plots
  ggplot2::ggsave(filename = paste(year, month, day, "pdf", sep = "."),
                  units = "in", width = 10, height = 24)   
  
  
  
  
}
