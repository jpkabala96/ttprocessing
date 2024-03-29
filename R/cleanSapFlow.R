#'Clean Sap Flow data
#'
#' @description cleanSapFlow cleans the sap flow data, once converted, excluding values that are not plausible.
#' This is mainly a helper function that works inside clean4DData, but can also be used for cleaning the data manually.
#' By default the lower value is set to 0, the upper to 300, but the user can set those parameters manually. The values outside this range are set to NA.
#' @param sap_flow A numeric vector with the converted sap flow data
#' @param lower.sap.flow The lowest value accepted (defaults to 0)
#' @param higher.sap.flow The highest value accepted (defaults to 300)
#' @return A numeric vector with the invalid values replaced by NA's
#' 
#' @examples
#' # remove sap flow values outside the range specified by the user
#' #and replaces with NA
#' \dontrun{
#' sap_flow_example <- sample(c(0:1000), size = 20)
#' print(sap_flow_example)
#' cleanSapFlow(sap_flow_example)
#' cleanSapFlow(sap_flow_example, lower.sap.flow = 0, higher.sap.flow = 500)
#' }
#' 

#' @export



cleanSapFlow <- function(sap_flow, lower.sap.flow = 0, higher.sap.flow = 300){
  sap_flow[sap_flow < lower.sap.flow] <- NA
  sap_flow[sap_flow > higher.sap.flow] <- NA
  return(sap_flow)
}
