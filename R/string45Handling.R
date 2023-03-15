#' Function for handling the string 45 data
#' 
#' @description Function for handling the string 45 of the old Tree Talkers. 
#' The function is very similar to the \code{clean4DData} function in its inputs and outputs. 
#' @param TTdata Raw ata from the Tree Talker devices. The data returned by the 
#'   readTTdata function. 
#' @param lower.TTree Lower threshold of temperature inside the trunk. 
#'    Values lower than this will be regarded as non valid and replaced with NA.
#'    Default value = \code{0}.
#' @param higher.TTree Higher threshold of temperature inside the trunk. 
#'    Values higher than this will be regarded as non valid and replaced with NA.
#'    Default value = \code{40}.
#' @param lower.TAir Lower threshold of air temperature regarded as valid. 
#'    Values lower than this will be regarded as non valid and replaced with NA.
#'    Default value = \code{-15}.
#' @param higher.TAir Higher threshold of air temperature regarded as valid. 
#'    Values higher than this will be regarded as non valid and replaced with NA.
#'    Default value = \code{50}.
#' @param lower.RH Lower threshold of Relative Humidity (%) regarded as valid. 
#'    Values lower than this will be regarded as non valid and replaced with NA.
#'    Default value = \code{35}. 
#' @param higher.RH Higher threshold of Relative Humidity (%) regarded as valid. 
#'    Values higher than this will be regarded as non valid and replaced with NA.
#'    Default value = \code{100}
#' @param lower.sap.flow Lower threshold of sap flow. Values lower than this will 
#'    be replaced with NA. Default value = \code{0}.
#' @param higher.sap.flow Higher threshold of sap flow. Values higher than this will be 
#' considered invalid and replaced with NA. Default value = \code{1080}.
#' @param tz Timezone of the site analyzed, to properly convert the timestamp in
#'   the local time value. Defaults to \code{Sys.timezone}.
#' @param remove.raw If to remove columns of raw data or retain them. Default value:
#'    \code{"yes"}
#'   to remove them, in any other case they will be retained.
#' @param rectangular If to retain dates and hours of missing strings. If \code{"no"}
#'   they will be discarded, getting lighter data. In any other case the full grid of
#'   ids x times will be kept.
#' @return A data frame with TT+ 4D data: air temperature, relative humidity, vpd, 
#'   temperatures of the sap flow probes, K index, sap flux estimated with the 
#'   Do et al. (2011) equation, for two and one probe, sap flux estimated with the 
#'   equation in Asgharinia et al. (2022), growth raw data (DN), stem Water content
#'   sensor raw values and values calculated using the species specific calibration 
#'   equations. 
#' @references Do, F.C.; Isarangkool Na Ayutthaya, S.; Rocheteau, A. Transient thermal dissipation method for xylem sap flow measurement: Implementation with a single probe. Tree Physiol. 2011, 31, 369– 380, doi:10.1093/treephys/tpr020.
#'   Asgharinia, S., Leberecht, M., Belelli Marchesini, L., Friess, N., Gianelle, D., Nauss, T., ... & Valentini, R. (2022). Towards Continuous Stem Water Content and Sap Flux Density Monitoring: IoT-Based Solution for Detecting Changes in Stem Water Dynamics. Forests, 13(7), 1040.
#' @describeIn clean4DData. 
#' @export


string45Handling <- function(TTdata,
                             lower.TTree = 0, higher.TTree = 40,
                             lower.TAir = -15, higher.TAir = 50,
                             lower.RH = 35, higher.RH = 100, 
                             lower.sap.flow = 0, higher.sap.flow = 1080,
                             remove.raw = "yes", rectangular = "no",
                             tz = Sys.timezone()){
  colnames_string45 <- c("record", "record_number", "string_type", "timestamp","Tref0_r", "Theat0_r", "growth_DN",
                         "vbat_r", "nbits", "RH_r", "Tair_r", "gz_mean", "gz_sd", "gy_mean", "gy_sd",
                         "gx_mean", "gx_sd", "Tref1_r","Theat1_r", "Ecf_Hz")
  #filter the "45" data
  string45_data <- TTdata %>% dplyr::filter(.data$V3 == "45")
  string45_data[,4:30]<- lapply(string45_data[,4:30],
                                MARGIN = 2, 
                                FUN = as.numeric)
  string45_data <- string45_data[,1:20]
  #set properly the colnames and convert all numeric variables in numeric
  colnames(string45_data) <- colnames_string45
  
  string45_data <- string45_data %>% 
    dplyr::mutate(id = obtainId(record),
                  time = tzConvert(as.POSIXct(as.double(.data$timestamp), origin = "1970-01-01 00:00.00", 
                                              tz = "UTC"), tz = tz),
                  date_hour = lubridate::make_datetime(year = lubridate::year(.data$time), 
                                                       month = lubridate::month(.data$time), 
                                                       day = lubridate::day(.data$time), 
                                                       hour = lubridate::hour(.data$time)),
                  is_record = T)
  string45_data <- string45_data %>% 
    dplyr::filter(date_hour > "2000-01-01 00:00.00 UTC")
  all_times <- seq(from = min(string45_data$date_hour), to = max(string45_data$date_hour), 
                   by = "1 hour")
  all_ids <- unique(string45_data$id)
  grd <- expand.grid(all_times, all_ids)
  colnames(grd) <- c("date_hour", "id")
  string45_data <- dplyr::left_join(grd, 
                                    string45_data,
                                    by = c("id" = "id", 
                                           "date_hour" = "date_hour"))
  string45_clean <- string45_data %>%
    dplyr::mutate(Tref0 = cleanTemperatureTree(.data$Tref0_r/10,
                                               lower.TTree = lower.TTree,
                                               higher.TTree = higher.TTree),
                  Tref1 = cleanTemperatureTree(.data$Tref1_r/10,
                                               lower.TTree = lower.TTree,
                                               higher.TTree = higher.TTree),
                  Theat0 = cleanTemperatureTree(.data$Theat0_r/10,
                                                lower.TTree = lower.TTree,
                                                higher.TTree = higher.TTree),
                  Theat1 = cleanTemperatureTree(.data$Theat1_r/10,
                                                lower.TTree = lower.TTree,
                                                higher.TTree = higher.TTree),
                  voltage = 650+(131072*(1100/.data$vbat_r)),
                  Tair = cleanTAir(.data$Tair_r/10, 
                                   lower.TAir = lower.TAir,
                                   higher.TAir = higher.TAir),
                  RH = cleanRH(RH_r, lower.RH = lower.RH, higher.RH = higher.RH),
                  vpd = VPD(Tair = Tair, RH = RH),
                  DT = .data$Theat1-.data$Tref1 - .data$Theat0 + .data$Tref0,
                  DT_oneprobe = .data$Theat1-.data$Theat0,
                  date = as.Date(date_hour),
                  hour = lubridate::hour(.data$date_hour),
                  f_hour = as.character(lubridate::hour(.data$date_hour)),
                  month = lubridate::month(.data$date_hour),
                  f_month = as.character(lubridate::month(.data$date_hour)),
                  year = lubridate::year(.data$date_hour),
                  f_year = as.character(lubridate::year(.data$date_hour)),
                  quarter = quarters(date),
                  yq = paste(.data$f_year, .data$quarter, sep = "-"),
                  id_date = paste(id, date, sep = "-")
    )
  string45_clean <- string45_clean[order(string45_clean$date_hour),] %>%
    dplyr::group_by(id) %>%
    tidyr::nest() %>%
    dplyr::mutate(data2 = purrr::map(.data$data, calculateTheat10),
                  data = NULL) %>%
    tidyr::unnest(cols = .data$data2) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DT = .data$Theat1-.data$Theat10)
  print("Delta T done")
  
  
  string45_clean <- string45_clean %>% dplyr::group_by(.data$id_date) %>% 
    dplyr::summarise(DT_max24h = max(.data$DT)) %>% 
    dplyr::ungroup() %>%
    dplyr::right_join(string45_clean, by = c("id_date" = "id_date"))
  
  string45_clean <- string45_clean %>%
    dplyr::mutate(
      K = ((.data$DT_max24h/.data$DT)-1),
      do_sap_flux = cleanSapFlow(12.95*.data$K*100,
                                 lower.sap.flow = lower.sap.flow,
                                 higher.sap.flow = higher.sap.flow),
      asgharinia_sap_flux = cleanSapFlow(100 * ((11.3 * .data$K/(1 -.data$K))^0.77),
                                         lower.sap.flow = lower.sap.flow,
                                         higher.sap.flow = higher.sap.flow))
  
  if(remove.raw == "yes"){
    string45_clean <- string45_clean %>%
      dplyr::select(-.data$record, 
                    -.data$record_number, 
                    -.data$timestamp, 
                    -.data$Tref0_r,
                    -.data$Tref1_r,
                    -.data$Theat0_r,
                    -.data$Theat1_r,
                    -.data$vbat_r,
                    -.data$Tair_r,
                    -.data$RH_r)
  }
  if(rectangular == "no"){
    string45_clean <- string45_clean %>%
      dplyr::filter(.data$is_record == T)
  }
  return(string45_clean)
  
}
