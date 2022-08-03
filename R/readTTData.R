#' Read TreeTalkers data from server
#'
#' @description Uses the fread function from data.table package with arguments set properly
#' @param link_string should be the string of the server link to the Tree Talkers data, you can use also the path to the data file if you have downloaded them
#' @returns an object of class "tbl" , "data.table", "data.frame"
#'


#' @export
#'
#'

readTTData <- function(link_string){
  data <- data.table::fread(link_string, sep = ";", header = FALSE, fill = TRUE)
  if(ncol(data) < 30){
    print("File has to be read with meltcsv2")
    print("Might take a long time")
    data <- meltr::melt_csv2(link_string)
    print("read")
    data <- data %>%
      dplyr::select(-.data$data_type) %>%
      tidyr::pivot_wider(names_from = col,
                  values_from = value,
                  values_fill = NA,
                  names_prefix = "V") %>%
      dplyr::select(-.data$row)
    #trasformo le colonne in numeriche
    col_num <- paste0("V",c(5:30))
    data[col_num] <- sapply(data[col_num], as.numeric)#change to numeric the cols that are numeric
  }
  print("done")
  return(data)
}
