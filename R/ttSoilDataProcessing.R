#' Function for handling TT-soil data.
#' 
#' @description This function implements the calibration equations from the 
#'  TT-soil manual, and is able to handle the TT-soil data according to them.
#'  
#' @param TTdata Raw TT+ data, containing the TT-soil 45, 4D or 55 string.
#' @param string_type The type of string produced by the TT-soil. Can be:
#'    \code{"4D"}, \code{"55"} or \code{"59"}. Default value: "4D".
#' @param equation The equation from the manual to be used. Possible 
#'   names of equations are: \code{"S1"}, \code{"S2"}, \code{"S3"},
#'   \code{"S4"}, \code{"S5"}, \code{"S6"} or \code{"S7"}.
#' @param lower.TSoil Lower threshold for the soil temperature records. Values
#' ,lower than this will be discarded.
#' @param higher.TSoil Higher threshold for the soil temperature records. Values
#' higher than this will be discarded. 
#' @param lower.TAir Lower threshold for the air temperature records. Values 
#' lower than this will be discarded.
#' @param higher.TAir Higher threshold for the air temperature records. Values
#' higher than this will be discarded.
#' @param lower.RH Lower threshold for the relative humidity. Values lower than
#'  this will be discarded.
#' @param higher.RH Higher threshold for the relative humidity. Values higher
#' than this will be discarded. 
#' @param tz Timezone. Defaults to system timezone.
#' @details For handling the TT-soil data, the user has to choose the equation
#'   that matches the probe type and the soil bulk density at the study site.
#'   Than the raw data are processed the following way:
#'   1. Temperature is converted from DN to degrees C with the lookup table.
#'   2. The values out of range of the raw Ecf are replaced with \code{NA}
#'   3. It is corrected for temperature sensitivity
#'   4. The Ecf corrected is used for estimating soil water content.
#' 
#'   Table with equation coefficients that are used in the equations.
#' 
#' | Equation | Probe freq.   | Soil bulk density | m (T sensitivity)| b (T sensitivity) | B1         | B2        | B3    |
#' | -------- |:-------------:| :----------------:|:----------------:|:-----------------:|:----------:|:---------:|:-----:|
#' | S1       | 50000         | 1.622             | -5.6             | 830               | 3.08x10^-7 |-0.005394  |26     |
#' | S2       | 50000         | 1.194             | -5.6             | 830               | 1.30x10^-8 |-0.0009713 |31     |
#' | S3       | 3000000       | 0.669             | -758             | 154844            | 1.9x10^-11 |-6.16x10^-5|59     |
#' | S4       | 3000000       | 1.221             | -758             | 154844            | 6.4x10^-12 |-1.93x10^-5|49     |
#' | S5       | 3000000       | 0.9708            | -758             | 154844            | 7.1x10^-12 |-1.78x10^-5|17     |
#' | S6       | 3000000       | 0.871             | -758             | 154844            | 5.7x10^-10 |-0.0003022 |53     |
#' | S7       | 3000000       | 0.3866            | -758             | 154844            | 1.68x10^-9 |-0.000521  |41     |
#' @md
#'   
#' 
#' 
#' @export


ttSoilDataProcessing <- function(TTdata,
                                 string_type = "4D",
                                 equation = "S1",
                                 lower.TSoil = -15,
                                 higher.TSoil = 40,
                                 lower.TAir = -15,
                                 higher.TAir = 50,
                                 lower.RH = 20,
                                 higher.RH = 100,
                                 tz = Sys.timezone()
                                 ){
  print(paste(paste("Conversion equation", equation), "has been chosen"))
  print("Full available equations table: ")
  #DECLARE CONVERSION TABLE
  conversion_table <- data.frame(
    name = c("S1", "S2", "S3", "S4", "S5", "S6", "S7"),
    bulk_density = c(1.622, 1.194, 0.669, 1.221, 0.9708, 0.871, 0.3866),
    frequency = c(50000, 50000, 3000000, 3000000, 3000000, 3000000, 3000000),
    #tempertature sensitivity
    m = c(-5.6, -5.6, -758, -758, -758, -758, -758),
    b = c(830, 830, 154844, 154844, 154844, 154844, 154844),
    # calibration coefficients
    b1 = c(3.08*10^-7, 1.30*10^-8, 1.89*10^-11, 6.39*10^-12, 7.09*10^-12, 5.73*10^-10, 1.86*10^-9),
    b2 = c(-0.0055394, -0.0009713, -6.16*10^-5, -1.93*10^-5, -1.78*10^-5, -0.0003022, -0.000521),
    b3 = c(26, 31, 59, 49, 17, 53, 41),
    RMSE = c(3.461, 3.413, 1.4, 2.3, 3.1, 4.6, 1.5),
    R_squared = c(0.825, 0.835, 0.99, 0.846, 0.643, 0.896, 0.989),
    validity_min = c(937, 1418, 332981, 196820, 126151, 148790, 125179),
    validity_max = c(16620, 39687, 1938663, 1903202, 1788494, 403581, 226426)
  )
  print(conversion_table)
  
  
  
  
  print("table declared")
  data4D <- TTdata %>% dplyr::filter(V3 == string_type)
  print("filtering done")
  #obtain id (if not already done)
  if("id" %in% colnames(data)){
    print("id already done")
  } else{
    data4D$id <- obtainId(data4D$V1)
  }
  print("id done!")
  #convert the timestamp to local time
  data4D$time <- as.POSIXct(as.double(data4D$V4),
                            origin = '1970-01-01 00:00.00',
                            tz = "UTC") %>% tzConvert(tz = tz)
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
  print("join done")
  #obtain the date
  data4D$t <- data4D$date_hour %>%
    as.Date()
  #set proper colnames
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
  clean_data$date <- as.Date(clean_data$date_hour)
  
  clean_data$Tref0 <-
    convertTemperature(data4D$Tref0_DN) %>%
    as.double() %>%
    cleanTemperatureTree(lower.TTree = lower.TSoil,
                         higher.TTree = higher.TSoil)
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
  clean_data$vpd <- VPD(clean_data$Tair, clean_data$RH)
  print("cleaning done")
  
  #Isolate the appropriate equation
  actual_coeff <- conversion_table %>%
    dplyr::filter(name == equation)
  
  #convert the Ecf 
  clean_data$Ecf_clean <- ifelse(clean_data$Ecf_Hz < actual_coeff$validity_min,
                           NA,
                           ifelse(clean_data$Ecf_Hz > actual_coeff$validity_max,
                                  NA, 
                                  clean_data$Ecf_Hz))
  clean_data <- clean_data %>%
    dplyr::mutate(
      Ecf_clean = ifelse(Ecf_Hz < actual_coeff$validity_min,
                         NA,
                         ifelse(Ecf_Hz > actual_coeff$validity_max,
                                NA, 
                                Ecf_Hz)),
      Ecf_t = Ecf_clean - (actual_coeff$m*(Tref0-20) + actual_coeff$b),
      VWC = actual_coeff$b1*(Ecf_t^2) + actual_coeff$b2 * Ecf_t + actual_coeff$b3
    )
  print("Coefficients used")
  print(actual_coeff)
  return(clean_data)
}
