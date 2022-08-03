#' TT cloud 4B string processing
#'
#' @description this function isolates the 4B string, renames its columns,
#'   retrieves the id and converts the timestamp to date-time.
#' @param raw_data The raw Tree Talkers data, as those obtained by the readTTData
#'   function.
#' @return A data frame with the 4B data properly organized.
#' @export

cloudData4B <- function(raw_data){
  data_4B <- raw_data %>% dplyr::filter( V3 == "4B")
  print("filtering server done")
  data_4B <- data_4B[,1:12] #select the columns
  colnames(data_4B) <- c("Record","Rec_num", "string_type", "t", "rec_in_mem", "rec_to_be_sent", "op_MCC", "op_MNC", "GSM_reg", "GSM_field", "voltage", "firmware")
  print("cols renamed")
  data_4B$id <- obtainId(data_4B$Record)
  data_4B$date_hour <- as.POSIXct(as.double(data_4B$t),
                                  origin = '1970-01-01 00:00.00',
                                  tz = "UTC") %>%
    tzConvert()
  return(data_4B)
}
