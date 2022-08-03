#' Clean 4D data
#' @description This is the main function in this package. Its purpose is to
#'   filter and convert the 4D data from the TT+ raw data downloaded from the server
#'   The data are converted from DNs to physical quantities using the equations in the
#'   TT+ manual. The data are also cleaned, removing values that are regarded
#'   not to be plausible (e.g. temperatures that are too high, too low etc.) by
#'   calling the helper functions.
#' @export

clean4DData <- function(data,
                        lower.TTree = 0,
                        higher.TTree = 40,
                        lower.TAir = -15,
                        higher.TAir = 50,
                        lower.RH = 35,
                        higher.RH = 100,
                        lower.sap.flow = 0,
                        higher.sap.flow = 1080,
                        lower.VWC = 0,
                        higher.VWC = 100,
                        species = "FagusS"){

  #filter keeping only 4D data
  data4D <- data %>% dplyr::filter( V3 == "4D")

  print("filtering done")


  #obtain id (if not already done)
  if("id" %in% colnames(data)){
    print("id already done")
  } else{
    data4D$id <- obtainId(data4D$V1)
  }

  #convert the timestamp to local time
  data4D$time <- as.POSIXct(as.double(data4D$V4),
                                 origin = '1970-01-01 00:00.00',
                                 tz = "UTC") %>% tzConvert()
  #clean for second discrepancies
  data4D$date_hour<- lubridate::make_datetime(year = lubridate::year(data4D$time),
                           month = lubridate::month(data4D$time),
                           day = lubridate::day(data4D$time),
                           hour = lubridate::hour(data4D$time))
  print("data ora done")
  data4D <- data4D %>% dplyr::filter(date_hour > "2000-01-01 00:00.00 UTC")#filter removing
  #observations with too old date



  all_times <- seq(from = min(data4D$date_hour),
                   to = max(data4D$date_hour),
                   by = "1 hour")
  all_ids <- unique(data4D$id)
  grd <- expand.grid(all_times, all_ids)#create a grid with all dates and times
  #possible for the time interval from the first to the last observation
  colnames(grd) <- c("date_hour", "id")
  data4D <- dplyr::left_join(grd,
                             data4D,
                             by = c("id" = "id", "date_hour" = "date_hour"))

  #obtain the date
  data4D$t <- data4D$date_hour %>%
    as.Date()

  #assign proper colnames
  colnames(data4D)[1:23] <- c("date_hour",
                              "id",
                              "Record",
                              "Record_number",
                              "TipoStringa",
                              "tempo",
                              "Tref0_DN",
                              "Theat0_DN",
                              "growth_DN",
                              "adc_bandgap",
                              "nbits",
                              "RH",
                              "Tair",
                              "g_z_mean",
                              "g_z_devst",
                              "g_y_mean",
                              "g_y_devst",
                              "g_x_mean",
                              "g_x_devst",
                              "Tref1_DN",
                              "Theat1_DN",
                              "Ecf_Hz",
                              "adc_vbat")
  print("colnames done")

  #create a clean data data.frame
  clean_data <- data4D %>% dplyr::select(.data$Record,
                                         .data$date_hour,
                                         .data$id,
                                         .data$TipoStringa,
                                         .data$tempo,
                                         .data$growth_DN,
                                         .data$Ecf_Hz)
  clean_data$Ecf_Hz <- as.double(clean_data$Ecf_Hz)
  clean_data$growth_DN <- as.double(clean_data$growth_DN)
  head(clean_data)
  clean_data$date <- as.Date(clean_data$date_hour)
  #create a unique code by id and date. This is necessary for sap flow calculation
  #as one must obtain the max delta T value over the 24 hours
  clean_data$id_date <- paste0(clean_data$id, as.character(clean_data$date))








  #add columns to the clean_data by applying the correct formulas and cleaning
  #functions.
  clean_data$Tref0 <-
    convertTemperature(data4D$Tref0_DN) %>%
    cleanTemperatureTree(lower.TTree = lower.TTree,
                         higher.TTree = higher.TTree)
  clean_data$Theat0 <-
    convertTemperature(data4D$Theat0_DN) %>%
    cleanTemperatureTree(lower.TTree = lower.TTree,
                         higher.TTree = higher.TTree)
  clean_data$Tref1 <-
    convertTemperature(data4D$Tref1_DN) %>%
    cleanTemperatureTree(lower.TTree = lower.TTree,
                         higher.TTree = higher.TTree)
  clean_data$Theat1 <-
    convertTemperature(data4D$Theat1_DN) %>%
    cleanTemperatureTree(lower.TTree = lower.TTree,
                         higher.TTree = higher.TTree)


  #convert the data produced by the growth sensor.
  #the data are converted to double as by default they are recorded as integers
  #and read as integers by the readTTData function
  #the user might use the column correspondig to the distance the sensor was mounted
  #clean_data$growth08 <- as.double(data4D$growth_DN)*0.1817/(109108.5561-as.double(data4D$growth_DN)) #converto il segnale di crescita per valori da 0 a 0.8 cm
  #clean_data$growth1.5_9 <- -1.5242+(111.3154*2373.2628)/(2373.2628+as.double(data4D$growth_DN)) #converto il segnale di crescita per valori da 1.5 a 9 cm
  #clean_data$growth1.5_5 <- 18.2823-(0.0006*as.double(data4D$growth_DN))+(0.0000000069143*(as.double(data4D$growth_DN^2)))-(0.000000000000030237*(as.double(data4D$growth_DN)^3))#converto il segnale di crescita per valori che vanno da 1.5 a 5 cm

  #convert the battery voltage
  clean_data$voltage <- 2200*as.double(data4D$adc_vbat)/as.double(data4D$adc_bandgap)#calcolo il voltaggio della batteria

  #convert T air (dividing by 10) and clean using the proper function
  clean_data$Tair <- cleanTAir(as.double(data4D$Tair)/10,
                               lower.TAir = lower.TAir,
                               higher.TAir = higher.TAir)  #calcolo la temperatura dell'aria dividendo per 10

  #clean the relative humidity calling the proper function
  clean_data$RH <- as.double(data4D$RH) %>%
    cleanRH(lower.RH = lower.RH,
            higher.RH = higher.RH)

  #obtain the Delta T
  clean_data$DT <- (as.double(clean_data$Theat1)-as.double(clean_data$Tref1))-(as.double(clean_data$Theat0)-as.double(clean_data$Tref0))
  clean_data$DT_oneprobe <- as.double(clean_data$Theat1)-as.double(clean_data$Theat0)
  #obtain the max Delta T during the day, and ad it to the clean_data data.frame
  #by using a join function
  clean_data <- clean_data %>%
    dplyr::group_by(id_date) %>%
    dplyr::summarise(DT_max24h = max(DT),
                     DT_max24h_op = max(DT_oneprobe)) %>%
    dplyr::right_join(clean_data, by = c("id_date" = "id_date"))

  #calculate the sap flow using the formulas available
  # clean_data$granier_sap_flow <- 12.95*(clean_data$DT_max24h/((clean_data$Theat1-clean_data$Tref1)-(clean_data$Theat0-clean_data$Tref1))-1) %>%
  #   cleanSapFlow(lower.sap.flow = lower.sap.flow, higher.sap.flow = higher.sap.flow)
  # clean_data$do_sap_flow_old <- (12.95*(((clean_data$DT_max24h/clean_data$DT)-1))*27.777)*3.6 %>%
  #   cleanSapFlow(lower.sap.flow = lower.sap.flow, higher.sap.flow = higher.sap.flow)
  clean_data$K1 <- ((clean_data$DT_max24h / clean_data$DT) -1)
  clean_data$K1 <- ifelse(clean_data$K1 < 0, 0, clean_data$K1)
  clean_data$do_sap_flow <- 12.95 * clean_data$K1 * 100 #calculate the sap flow using the old formula
  #clean the values obtained
  clean_data$do_sap_flow <- cleanSapFlow(clean_data$do_sap_flow,
                                         lower.sap.flow = lower.sap.flow,
                                         higher.sap.flow = higher.sap.flow)
  #one probe sap
  clean_data$K1_op <- ((clean_data$DT_max24h_op/clean_data$DT_oneprobe)-1)
  clean_data$K1_op <- ifelse(clean_data$K1_op < 0, 0, clean_data$K1_op)
  clean_data$sap_flow_one_probe <- 4.79 * clean_data$K1_op * 100
  #clean the values obtained
  clean_data$sap_flow_one_probe <- cleanSapFlow(clean_data$sap_flow_one_probe,
                                                lower.sap.flow = lower.sap.flow,
                                                higher.sap.flow = higher.sap.flow)

  #calculate vapour pressure deficit
  clean_data$vpd <- VPD(clean_data$Tair, clean_data$RH)
  print("cleaning done")

  #build the coefficients table
  STWC_coef <- data.frame(matrix(nrow = 2, ncol = 6))
  colnames(STWC_coef) <- c("Species","m", "a3", "a2", "a1", "a0")
  STWC_coef$Species <- c("FagusS", "FagusN")
  STWC_coef$m <- c(-42.3, -42.3)
  STWC_coef$a3 <- c( -1.40 * 10^-12, -5.87 * 10^-12)
  STWC_coef$a2 <- c(5.44 * 10^-8, 1.58*10-7)
  STWC_coef$a1 <- c(-0.002426, -0.0029182)
  STWC_coef$a0 <- c(49.89541, 55.959579)
  STWC_coef <- dplyr::filter(STWC_coef, .data$Species == species)
  #if the species does not exist use NA
  if(nrow(STWC_coef) == 0){
    STWC_coef <- data.frame(matrix(data = NA, nrow = 1, ncol = 6))
    colnames(STWC_coef) <- c("Species","m", "a3", "a2", "a1", "a0")
  }

  clean_data$EcfHz_clean  <- cleanSensorSTWC(as.double(data4D$Ecf_Hz))
  clean_data <- clean_data %>%
    group_by(.data$id) %>%
    summarise(Min_EcfHz = min(.data$EcfHz_clean, na.rm = T)) %>%
    right_join(clean_data, by = c("id" = "id"))
  #calculate the Frequency Temperature Index
  clean_data$FTI <- clean_data$EcfHz_clean * (STWC_coef$m*clean_data$Tref0 + clean_data$Min_EcfHz)
  #calculate the percentage relative Water Volumetric Content
  clean_data$VWC <- STWC_coef$a3 * clean_data$FTI^3 + STWC_coef$a2 * clean_data$FTI^2 + STWC_coef$a1 * clean_data$FTI + STWC_coef$a0
  #clean removing the values lower than 0 and higher than 100
  clean_data$VWC <- cleanSTWC(clean_data$VWC,
                              lower.VWC = lower.VWC,
                              higher.VWC = higher.VWC)


  #create time variables and factors

  clean_data$hour <- lubridate::hour(clean_data$date_hour)
  clean_data$f_hour <- as.factor(clean_data$hour)

  #obtain the month and build also a factor variable for the month
  clean_data$month <- lubridate::month(clean_data$date_hour)
  clean_data$f_month <- as.factor(clean_data$month)

  #obtain the year
  clean_data$year <- lubridate::year(clean_data$date_hour)
  clean_data$f_year <- as.factor(clean_data$year)

  #obtain the quarter
  clean_data$quarter <- quarters(clean_data$date)
  clean_data$yq <- paste(as.character(clean_data$year),
                         clean_data$quarter,
                         sep = "-")

  #keep only one record for hour
  clean_data$date_hour_str <- paste(clean_data$date, clean_data$hour)
  clean_data$dhid <- paste0(clean_data$date_hour_str, clean_data$id)
  clean_data <- clean_data %>%
    dplyr::distinct(dhid, .keep_all = T) %>%
    dplyr::select(-dhid)

  #rimuovo valori tecnicamente impossibili
  #in realta andranno rimossi un numero maggiore di valori

  return(clean_data)
}
