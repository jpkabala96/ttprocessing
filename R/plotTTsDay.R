#' Plot number of TTs per day 
#' @description Function to graphically display the output of 
#'   the \code{dailyNumberTTs} result. Provides a plot with the number of TTs
#'   available for that day with complete observations of the variable of 
#'   interest.
#' @param TTdaydata The dataframe output by the \code{dailyNumberTTs} function.
#' @param print Default value = \code{TRUE}
#' @return A ggplot object
#' 
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' qc_data <- clean4DData(raw4d_ex_data)
#' nttsdaily <- dailyNumberTTs(qc_data)
#' plotTTsDay(nttsdaily, print = T)
#' }
#' 
#' @export
#' 
plotTTsDay <- function(TTdaydata, print = TRUE){
  ntts_plot <- ggplot2::ggplot(TTdaydata)+ 
    ggplot2::geom_line(ggplot2::aes(x = date, y = NTTS))+
    ggplot2::geom_point(ggplot2::aes(x = date, y = NTTS))+
    ggplot2::ylab("Number of TTs with complete data")
  if(print == T){
    print(ntts_plot)
  }
  return(ntts_plot)
}
