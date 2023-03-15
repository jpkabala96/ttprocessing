#' Graphical report of TT+ technical funtioning
#' @description This function creates a graphical report of dayly probe state and
#'   device battery voltage for a specific Tree talker during a time specified by
#'   the user. 
#' @param TTdata4d Tree talker 4D data, as the dataframe produced by the  
#'   \code{clean4DData}) function.    
#' @param id Device id as string. Must be one of the values stored in the 
#'   4d data column id. 
#' @param start_date Date of start of the time frame of interest.
#' @param end_date Date of end of the time frame of interest.
#' @param min_valid_obs Number of minimum valid records required. If that
#'   minimum value is reached, the corresponding cell il colored in green, 
#'   while if not it is colored in red in the output plot. 
#' @return A ggplot object, the plot with the graphical assesment of the functioning
#'   of the device corresponding to that ID. 
#' @export
#' 

deviceFunctioningAssessment <- function(TTdata4d, 
                                        id,
                                        start_date = "2021-01-01",
                                        end_date = "2021-12-31",
                                        min_valid_obs = 24){
  #funzione per dividere giorni con sonde funzionanti e non funzionanti
  discriminate <- function(x){
    result <- ifelse(x >= min_valid_obs, T, F)
    return(result)
  }
  #isolo i dati di interesse
  data_of_interest <- TTdata4d %>%
    filterByDate(start = start_date, 
                 end = end_date) %>%
    filterIDs(ids = id)  
  #calcolo il voltaggio medio giornaliero
  #mean_voltage <- data_of_interest %>% 
  #  dplyr::group_by(date) %>%
  #  dplyr::summarise(mean_voltage = mean(as.numeric(voltage), na.rm = T)) %>%
  #  dplyr::ungroup()
  #faccio il quality check
  QC <- dailyQualityCheck(data_of_interest)
  #Converto il dataset a binario e faccio il pivot wider per poter fare un 
  #grafico raster
  functioning_data <- QC %>%
    dplyr::select(-id, -date) %>% 
    dplyr::mutate_all(discriminate) %>%
    cbind(dplyr::select(QC, date)) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with("complete_"),
                       names_to = "variable",
                       values_to = "value") %>%
    #dplyr::left_join(mean_voltage, by = c("date" = "date")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(variable = substr(variable, start = 14, stop = nchar(variable)))
  #produco il grafico
  sf_plot <- ggplot2::ggplot(functioning_data)+
    ggplot2::geom_raster(ggplot2::aes(x = date, y = variable, fill = value))+
    ggplot2::theme_bw()+
    ggplot2::ggtitle(label = paste0("Device ", id), 
                     subtitle = paste(paste("from ", start_date), 
                                      paste("to ", end_date)))+
    ggplot2::scale_fill_manual(values  = c("red", "green"))
  
  return(sf_plot)
    
  
}
