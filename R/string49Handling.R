#' Function for cleaning string 49 data
#' @description This function isolates the 49 string, containing light data and 
#'   uniforms the data
#'   setting them to the same gain. Then data are multiplyied by the proper
#'   coefficient to convert DNs to microWatt per squared centimetre.
#'   The coefficients used are the ones provided by Belelli Marchesini et al.
#' @param raw_data Raw data as those downloaded with readTTData.
#' @param time_zone Time zone to be used. Defaults to Sys.timezone().
#'
#' @export
#'
#'

string49Handling <- function(raw_data, time_zone = Sys.timezone()){
  names_49 <- c("Record",
                "Record_number",
                "Tipo_stringa",
                "timestamp",
                "DN_610",
                "DN_680",
                "DN_730",
                "DN_760",
                "DN_810",
                "DN_860",
                "DN_450",
                "DN_500",
                "DN_550",
                "DN_570",
                "DN_600",
                "DN_650",
                "integration_time",
                "gain",
                "id")

  data49 <- raw_data %>% dplyr::filter(V3 == "49")
  print("filtering done")
  data49 <- data49[,1:18]#retain only the columns of interest

  #obtain id (if not already done)
  if("id" %in% colnames(data)){
    print("id already done")
  } else{
    data49$id <- obtainId(data49$V1)
  }
  print("id done")

  colnames(data49) <- names_49
  print("colnames done")

  data49[,4:17] <- lapply(data49[,4:17],
                          as.double)
  print("conversion to double done")

  data49$divider <- ifelse(data49$gain == 0,1,
                              ifelse(data49$gain == 1,3.7,
                                     ifelse(data49 == 2, 16, 64)))
  print("multiplier done")

  DNS <- data49[,5:16]
  DNS <- DNS %>% apply(MARGIN = 2,
                       FUN = removeOosLight)
  DNSg2 <- (DNS / data49$divider) * 16
  #it seems there are no intercepts for the calibration
  #for now set them equal to 0
  #in the future maybe this part of the code will be removed
  #matrix of intercepts
  # cal.i <- as.data.frame(matrix(nrow = nrow(DNSg2), ncol = ncol(DNSg2)))
  # colnames(cal.i) <- colnames(DNSg2)
  # 
  # cal.i$DN_610 <- 0
  # cal.i$DN_680 <- 0
  # cal.i$DN_730 <- 0
  # cal.i$DN_760 <- 0
  # cal.i$DN_810 <- 0
  # cal.i$DN_860 <- 0
  # cal.i$DN_450 <- 0
  # cal.i$DN_500 <- 0
  # cal.i$DN_550 <- 0
  # cal.i$DN_570 <- 0
  # cal.i$DN_600 <- 0
  # cal.i$DN_650 <- 0
  # 
  # cal.i <- as.matrix(cal.i)

  #matrix of m
  cal.m <- as.data.frame(matrix(data = NA, nrow = nrow(DNSg2), ncol = ncol(DNSg2)))
  colnames(cal.m) <- colnames(DNSg2)
  #version with the new coefficients by Belelli Marchesini (in preparation)
  cal.m$DN_610 <- 989.1
  cal.m$DN_680 <- 957.1
  cal.m$DN_730 <- 943.2
  cal.m$DN_760 <- 915.2
  cal.m$DN_810 <- 1000.9
  cal.m$DN_860 <- 994.3
  cal.m$DN_450 <- 2214.6
  cal.m$DN_500 <- 2002.7
  cal.m$DN_550 <- 1715.4
  cal.m$DN_570 <- 1690.9
  cal.m$DN_600 <- 1605.8
  cal.m$DN_650 <- 1542.0

  cal.m <- as.matrix(cal.m)

  DNS_corr <-  (DNSg2 / cal.m)
  ##DNS_corr[DNS_corr < 0] <- 0
  #convert the timestamp to local time
  data49$date_hour <- as.POSIXct(as.double(data49$timestamp),
                                 origin = '1970-01-01 00:00.00',
                                 tz = "UTC") %>%
    tzConvert(tz = time_zone)

  data49$date <- as.Date(data49$date_hour)
  data49$hour <- lubridate::hour(data49$date_hour)
  data49$f_hour <- as.factor(data49$hour)
  print("data ora done")


  #select data to return
  data49 <- data49 %>%
    dplyr::select(.data$Record,
                  .data$Record_number,
                  .data$Tipo_stringa,
                  .data$timestamp,
                  .data$integration_time,
                  .data$id,
                  .data$divider,
                  .data$date_hour,
                  .data$date,
                  .data$hour,
                  .data$f_hour) %>%
    cbind(DNS_corr) %>%
    dplyr::distinct(.data$id,
                    .data$date_hour,
                    .keep_all = T)

  return(data49)
}
