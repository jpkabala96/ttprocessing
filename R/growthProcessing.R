#' Growth data processing
#' @description This function processes the 4D data by a simplified procedure,
#'   to focus on the growth sensor. The data are left raw, and are calculated
#'   using the new equation. Week, biweek, threeweek columns are created.
#'   Other unuseful columns are dropped.
#' @param raw_data The raw data downloaded with the \code{readTTData} function.
#' @details This function will be united to \code{clean4DData} when we come to
#'   a definitive version of the processing to be done to the growth data.
#' @export



growthProcessing <- function(raw_data){
  data4D <- raw_data %>% dplyr::filter(V3 == "4D")#filter for 4D data
  print("filtering done")
  data4D$id <- obtainId(data4D$V1)#obtain the id
  data4D$date_hour <- as.POSIXct(as.double(data4D$V4), origin = "1970-01-01 00:00.00",
                                 tz = "UTC") %>% tzConvert() #convert the timestamp
  print("data ora done")
  data4D$t <- data4D$date_hour %>% as.Date()#calculate the date
  #set proper colnames
  colnames(data4D)[1:21] <- c("Record", "Record_number", "TipoStringa",
                              "tempo", "Tref0_DN", "Theat0_DN", "growth_DN", "adc_bandgap",
                              "nbits", "RH", "Tair", "g_z_mean", "g_z_devst", "g_y_mean",
                              "g_y_devst", "g_x_mean", "g_x_devst", "Tref1_DN", "Theat1_DN",
                              "Ecf_Hz", "adc_vbat")
  colnames(data4D)[31:33] <- c("id", "date_hour", "date")
  print("colnames done")
  data4D$growth_DN_chr <- as.character(data4D$growth_DN)
  data4D$growth_DN_num <- as.numeric(data4D$growth_DN)
  data4D[data4D$growth_DN <= 0,]$growth_DN <- NA
  data4D$growth_new_eqn <- growthConversion(growthDN = data4D$growth_DN)
  data4D$settimana <- lubridate::week(data4D$date)
  data4D$f_settimana <- paste(as.character(lubridate::year(data4D$date)),
                              ifelse(nchar(as.character(data4D$settimana)) == 2,
                                     as.character(data4D$settimana),
                                     paste0("0",as.character(data4D$settimana))),
                              sep = ".")
  data4D$settimanaT <- as.numeric(data4D$f_settimana)
  data4D$duesettimane <- ceiling(data4D$settimana/2)
  data4D$f_duesettimane <- paste(as.character(lubridate::year(data4D$date)),
                                 ifelse(nchar(as.character(data4D$duesettimane)) == 2,
                                        as.character(data4D$duesettimane),
                                        paste0("0", as.character(data4D$duesettimane))),
                                 sep = ".")
  data4D$duesettimaneT <- as.numeric(data4D$f_duesettimane)
  data4D$tresettimane <- ceiling(data4D$settimana/3)
  data4D$f_tresettimane <- paste(as.character(lubridate::year(data4D$date)),
                                 ifelse(nchar(as.character(data4D$tresettimane)) == 2,
                                        as.character(data4D$tresettimane),
                                        paste0("0", as.character(data4D$tresettimane))),
                                 sep = ".")
  data4D$tresettimaneT <- as.numeric(data4D$f_tresettimane)
  data4D <- data4D %>% dplyr::select(.data$Record, .data$Record_number, .data$TipoStringa,
                                     .data$tempo, .data$id, .data$date_hour, .data$date,
                                     .data$growth_DN_chr, .data$growth_DN,
                                     .data$growth_DN_num, .data$growth_new_eqn, .data$settimana,
                                     .data$f_settimana, .data$settimanaT, .data$duesettimane,
                                     .data$f_duesettimane, .data$duesettimaneT,
                                     .data$tresettimane, .data$f_tresettimane, .data$tresettimaneT)
  return(data4D)
}
