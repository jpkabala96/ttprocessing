#' Plot device functioning
#' @description Function for plotting the assessment of the functioning of
#'   several devices. Takes as input the same params as 
#'   \code{deviceFunctioningAssesment}. Can take a character containing the ids
#'   to assess, and creates an ensemble plot. 
#' @param TTdata4d Tree talker data processed with the clean4DData function.
#' @param ids A character vector of ids to be assessed. The ids must among those 
#'   in the id column of the TTdata4d. The ids present in the data can be easily 
#'   checked with \code{unique(TTdata4d$id)}. 
#' @param start_date The date of beginning of the time period to be assessed.
#' @param end_date The date of end of the time period to be assessed. 
#' @param min_valid_obs The number of valid observations required for each 
#'   variable to consider the data valid. Default value = \code{24}.
#' @return A ggplot object.
#' @export
#' 
#' 
plotDeviceFunctioning <- function(TTdata4d,
                                  ids,
                                  start_date = "2021-01-01", 
                                  end_date = "2021-12-31",
                                  min_valid_obs = 24){
  plotlist <- list()
  for(i in 1:length(ids)){
    plotlist[[i]] <- deviceFunctioningAssessment(TTdata4d = TTdata4d,
                                                 id = ids[[i]],
                                                 start_date = start_date,
                                                 end_date = end_date,
                                                 min_valid_obs = min_valid_obs)
  }
  plot_array <- ggpubr::ggarrange(plotlist = plotlist, 
                                  ncol = 1, 
                                  nrow = length(plotlist),
                                  common.legend = T,
                                  legend = "bottom")
  return(plot_array)
}