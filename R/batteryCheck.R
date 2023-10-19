#' Battery check
#' @description Allows the user to check the battery level of the devices
#'   and the last time they sent a string.
#' @param raw_data The raw data obtained from \code{readTTdata}
#' 
#' @examples
#' \dontrun{
#' # example battery check
#' data(raw4d_ex_data)
#' batteryCheck(raw4d_ex_data)
#' }
#' 
#' 
#'
#' @export

batteryCheck <- function(raw_data){
  #process the data
  TTdata <- raw_data %>% convertStringsEnsemble(startdate = "2015-01-01", 
                                                enddate = Sys.Date()+ as.difftime(1, units = "days"))
  cloud_data <- raw_data %>% cloudData4B()
  #obtain last voltage
  v_sum_tt <- TTdata %>% dplyr::group_by(.data$id) %>%
    dplyr::filter(!is.na(.data$voltage)) %>%
    dplyr::filter(.data$date_hour == max(.data$date_hour)) %>%
    dplyr::select(.data$id, .data$date_hour, .data$voltage)
  v_sum_cl <- cloud_data %>%
    dplyr::filter(!is.na(.data$voltage)) %>%
    dplyr::filter(.data$date_hour == max(.data$date_hour)) %>%
    dplyr::select(.data$id, .data$date_hour, .data$voltage)
  v_sum_cl$voltage <- as.numeric(v_sum_cl$voltage)
  #build report table
  report_table <- rbind(v_sum_cl,v_sum_tt)
  return(report_table)
}
