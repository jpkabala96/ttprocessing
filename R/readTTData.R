#' Read TreeTalkers data from server
#'
#' @description Uses the fread function from data.table package with arguments set properly
#' @param link_string Should be the string of the server link to the Tree Talkers data, you can use also the path to the data file if you have downloaded them
#' @param start_date A string representing the first possible date. Timestamps
#'   representing time before this date will be discarded. Default value 
#'   \code{"2015-01-01"}.
#' @param end_date A string representing the date corresponding to the maximum 
#'   timestamp allowed in the data. Strings with higher timestamp values will be
#'   discarded. Default value: \code{Sys.Date() + as.difftime(1, units = "days")}.
#' @returns an object of class "tbl" , "data.table", "data.frame"
#' 
#' @examples
#' \dontrun{
#'  library(readr)
#'  data(raw4d_ex_data)
#'  #create a local file with data
#'  write_csv2(raw4d_ex_data, file = "filename.txt", col_names = F)
#'  #read the data from the created file
#'  rawTTdata <- readTTData("path to filename.txt")#replace with the file path
#'  print(rawTTdata)
#' }
#'
#' @export
#'
#'

readTTData <- function(link_string, 
                       start_date = "2015-01-01", 
                       end_date = Sys.Date() + as.difftime(1, units = "days")){
  print("Attempting reding with data.table")
  print(Sys.time())
  rawData <- data.table::fread(link_string, sep = ";", header = FALSE, fill = TRUE)
  if(ncol(rawData) < 30){
    print("reading with data.table failed")
    print("reading manually")
    print(Sys.time())
    rawData <- readLines(link_string)
    print("data read")
    print(Sys.time())
    semicolons <- stringr::str_count(rawData,pattern = ";")
    rawData <- data.frame(rawData = rawData)
    rawData <- tidyr::separate(rawData,col = .data[["rawData"]], 
                               sep = "[;]",
                               into = paste0("V", c(1:30)), convert = T,
                               fill = "right")
    print("data transformed")
    print(Sys.time())
  }
  print("Prefiltering for broken timestamps")
  print(Sys.time())
  first_timestamp <- as.numeric(as.POSIXct(as.Date(start_date)))
  last_timestamp <- as.numeric(as.POSIXct(as.Date(end_date)))
  rawData <- rawData %>% 
    dplyr::filter(V4 > first_timestamp) %>%
    dplyr::filter(V4 < last_timestamp)
  print("Prefiltering done")
  print(Sys.time())
  return(rawData)
}
