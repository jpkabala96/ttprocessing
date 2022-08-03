#' Function for cleaning string 49 data
#' @description This function isolates the 49 string, and uniforms the data
#'   setting them to the same gain.
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

  #matrix of intercepts
  cal.i <- as.data.frame(matrix(nrow = nrow(DNSg2), ncol = ncol(DNSg2)))
  colnames(cal.i) <- colnames(DNSg2)

  cal.i$DN_610 <- -312.45
  cal.i$DN_680 <- -561.56
  cal.i$DN_730 <- -1511.2
  cal.i$DN_760 <- -1012.5
  cal.i$DN_810 <- 91.58
  cal.i$DN_860 <- 334.88
  cal.i$DN_450 <-  -212.62
  cal.i$DN_500 <- -232.13
  cal.i$DN_550 <- -842.1
  cal.i$DN_570 <- -666.72
  cal.i$DN_600 <- -328.08
  cal.i$DN_650 <- 202.77

  cal.i <- as.matrix(cal.i)

  #matrix of m
  cal.m <- as.data.frame(matrix(data = NA, nrow = nrow(DNSg2), ncol = ncol(DNSg2)))
  colnames(cal.m) <- colnames(DNSg2)

  cal.m$DN_610 <- 1.6699
  cal.m$DN_680 <- 1.5199
  cal.m$DN_730 <- 1.6209
  cal.m$DN_760 <- 1.4549
  cal.m$DN_810 <- 0.8414
  cal.m$DN_860 <- 0.531
  cal.m$DN_450 <-  0.4562
  cal.m$DN_500 <- 0.6257
  cal.m$DN_550 <- 1.0546
  cal.m$DN_570 <- 1.0462
  cal.m$DN_600 <- 0.8654
  cal.m$DN_650 <- 0.7829

  cal.m <- as.matrix(cal.m)

  DNS_corr <- cal.i + (DNSg2 * cal.m)
  DNS_corr[DNS_corr < 0] <- 0
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
