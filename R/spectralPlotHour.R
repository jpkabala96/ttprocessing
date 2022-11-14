#' Spectral hourly plot
#' @description Function for plotting all the data of the same hour during a 
#' selected month.
#' @param data49 The 49 string data. A dataframe as the one output by the
#'   \code{string49Handling} function.
#' @param year Year of interest (scalar). Default: \code{2021}.
#' @param month Month of interest (scalar). Default: \code{6}.
#' @param hour Hour of interest (scalar). Default: \code{8}.
#' @param tz Time zone. Defaults to the System timezone.
#' @param print Flag \code{TRUE} or \code{FALSE}. If to print the plot or return
#'  it.
#' 
#' 
#' 
#' @export
#' @import ggpubr

spectralPlotHour <- function(data49, 
                                 year = 2021,
                                 month = 6,
                                 hour = 8,
                                 tz = Sys.timezone(),
                                 print = T){
  
  data49 <- dplyr::filter(data49, 
                          lubridate::year(.data$date) == year,
                          lubridate::month(.data$date) == month)
  #create a vector with ids
  plotlist <- list()
  ids <- unique(data49$id)
  band_wavelengths <- data49 %>%
    dplyr::select(dplyr::starts_with("DN_")) %>%
    colnames() %>%
    substr(start = 4, stop = 6) %>% 
    as.numeric()
  ##ripristina la conversione in numerico
  
  for(d in 1:28){
    #build time of interest
    time_of_interest <- lubridate::make_datetime(
      year = year,
      month = month,
      day = d,
      hour = hour,
      tz = tz)
    #loop over the ids
    df_hour <- data.frame(matrix(data = band_wavelengths, nrow = 12, ncol = 1))
    colnames(df_hour) <- "wavelength"
    print("wave")
    for(i in 1:length(ids)){
      #filter the original data and transpose
      hdata <- data49 %>% 
        dplyr::filter(id == ids[[i]]) %>%
        dplyr::filter(date_hour == time_of_interest) %>%
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
      plotlist[[d]] <- ggplot2::ggplot()+
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
                    plotlist[[25]],
                    plotlist[[26]],
                    plotlist[[27]],
                    plotlist[[28]],
                    ncol = 7, 
                    nrow = 4,
                    common.legend = T,
                    legend = "bottom")
  #save as pdf, with name identifying day, month year and id
  #quite large page to contain all the plots
  ggplot2::ggsave(filename = paste(year, month, "hour_", hour, "pdf", sep = "."),
                  units = "in", width = 35, height = 16)   
  
  
  
  
}

