#' Function for converting the 4D, 45 and 59 strings all together
#' @description Wrapper for the clean4DData, string45Handling and clean59Data
#'   functions, Its main purpose is to power the launchNewGUI function, and to
#'   ease the conversion of the TT data in clouds where different types of TT 
#'   devices are mixed.
#' @param data Tree talker data, read with the \code{readTTData} function of the package.
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
#' @param lower.VWC Lower threshold for volumetric water content. Values lower than
#'   this will be considered as non valid and replaced with NA. Default value = 
#'   \code{0}.
#' @param higher.VWC Higher threshold for volumetric water content. Values higher 
#'   than this will be considered as non valid and replaced with NA. 
#'   Default value = \code{100}.
#' @param species Species the TT+ are installed on. Needed to retrieve the 
#'   species specific coefficient for Stem Water Content estimation. 
#'   Possible values are \code{"Fagus"} and \code{"Oak"}, with \code{"Fagus"}
#'   as default value. As more calibrations become available they will be added.
#' @param tz Timezone of the site analyzed, to properly convert the timestamp in
#'   the local time value. Defaults to \code{Sys.timezone}.
#' @return A data frame with TT+ 4D data: air temperature, relative humidity, vpd, 
#'   temperatures of the sap flow probes, K index, sap flux estimated with the 
#'   Do et al. (2011) equation, for two and one probe, sap flux estimated with the 
#'   equation in Asgharinia et al. (2022), growth raw data (DN), stem Water content
#'   sensor raw values and values calculated using the species specific calibration 
#'   equations. 
#' @references Do, F.C.; Isarangkool Na Ayutthaya, S.; Rocheteau, A. Transient thermal dissipation method for xylem sap flow measurement: Implementation with a single probe. Tree Physiol. 2011, 31, 369 380, doi:10.1093/treephys/tpr020.
#' 
#'   Asgharinia, S., Leberecht, M., Belelli Marchesini, L., Friess, N., Gianelle, D., Nauss, T., ... & Valentini, R. (2022). Towards Continuous Stem Water Content and Sap Flux Density Monitoring: IoT-Based Solution for Detecting Changes in Stem Water Dynamics. Forests, 13(7), 1040.
#'   
#' @export


convertStringsEnsemble <- function(data,
                        lower.TTree = 0,
                        higher.TTree = 40,
                        lower.TAir = -15,
                        higher.TAir = 50,
                        lower.RH = 35,
                        higher.RH = 100,
                        lower.sap.flow = 0,
                        higher.sap.flow = 300,
                        lower.VWC = 0,
                        higher.VWC = 100,
                        species = "Fagus",
                        tz = Sys.timezone(),
                        startdate,
                        enddate){
  if("4D" %in% unique(data$V3)){
    dati4D <- clean4DData(data,
                         lower.TTree = lower.TTree,
                         higher.TTree = higher.TTree,
                         lower.TAir = lower.TAir,
                         higher.TAir = higher.TAir,
                         lower.RH = lower.RH,
                         higher.RH = higher.RH,
                         lower.sap.flow = lower.sap.flow,
                         higher.sap.flow = higher.sap.flow,
                         lower.VWC = lower.VWC,
                         higher.VWC = higher.VWC,
                         species = species,
                         tz = tz
    ) %>%
      h24Clean() %>%
      filterByDate(start = startdate, end = enddate)
  } else{
    dati4D <- ttprocessing::empty4DData
  }
   
  if("59" %in% unique(data$V3)){
    dati59 <- clean59Data(data,
                lower.TTree = lower.TTree,
                higher.TTree = higher.TTree,
                lower.TAir = lower.TAir,
                higher.TAir = higher.TAir,
                lower.RH = lower.RH,
                higher.RH = higher.RH,
                lower.sap.flow = lower.sap.flow,
                higher.sap.flow = higher.sap.flow,
                lower.VWC = lower.VWC,
                higher.VWC = higher.VWC,
                species = species,
                tz = tz
    ) %>% 
      h24Clean() %>%
      filterByDate(start = startdate, end = enddate)
  } else{
    dati59 <- ttprocessing::empty59Data
  }
  
  if("45" %in% unique(data$V3)){
    dati45 <- string45Handling(data,
                        lower.TTree = lower.TTree,
                        higher.TTree = higher.TTree,
                        lower.TAir = lower.TAir,
                        higher.TAir = higher.TAir,
                        lower.RH = lower.RH,
                        higher.RH = higher.RH,
                        lower.sap.flow = lower.sap.flow,
                        higher.sap.flow = higher.sap.flow,
                        tz = tz
    ) %>%
      h24Clean() %>%
      filterByDate(start = startdate, end = enddate)
  } else{
   dati45 <-  ttprocessing::empty45Data
  }
  dati45 <- dplyr::select(dati45, -nbits)                 
  dati4D <- dplyr::select(dati4D, -nbits)
  dati59 <- dplyr::select(dati59, -nbits)
  print(dati45)
  print(class(dati45))
  print(dati4D)
  print(class(dati4D))
  print(dati59)
  print(class(dati59))
  # 
  # df_list <- list()
  # df_list[[1]] <- dati45
  # df_list[[3]] <- dati4D
  # df_list[[3]] <- dati59
  # 
  final_result <- dplyr::bind_rows(list(df45 = dati45, 
                                        df4D = dati4D,
                                        df59 = dati59))
  final_result$f_hour <- factor(final_result$f_hour, levels = as.character(c(0:23)))
  return(final_result)
  
}
