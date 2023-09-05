#'tzConvert converts the time to local time
#'
#' @description This function converts the time from UTC as recorded by the Tree Talkers to the user's local time. It is used mainly as a helper function in clean4DData(), but can also be used for cleaning the data manually.
#' @usage tzConvert(t, tz = Sys.timezone())
#' @param t A time vector containing the POSIXct time of the observations.
#' @param tz Defaults to the timezone set on the device used.
#' Can be set manually to any timezone accepted by lubridate
#' @return A time vector "POSIXct" with the local time corresponding to the observations
#' @export



tzConvert <- function(t, tz = Sys.timezone()){
    step1 <- lubridate::with_tz(t, tz)
  oryear <- lubridate::year(step1)
  ormonth <- lubridate::month(step1)
  ordd <- lubridate::day(step1)
  orhour <- lubridate::hour(step1)
  result <- lubridate::make_datetime(oryear, ormonth, ordd, orhour)
  return(result)
}
