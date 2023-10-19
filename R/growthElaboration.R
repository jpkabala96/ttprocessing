#' Growth data elaboration
#' @description The raw DNs from the growth sensor are aggregated weekly,
#'   biweekly or by intervals of three weeks.
#'   Then the highest mode is taken. This value is than converted to measurment
#'   in cms. For each id the initial value is subtracted to all the values measured.
#' @param data4d The dataframe returned by \code{clean4DData}, with inside the
#'   growth_DN column
#' @param nweeks The number of consecutive weeks to aggregate together. Must be
#'   1, 2 or 3. The function might accept also higher values, but we don't
#'   recommend to use them as the procedure has not been tested for those values.
#' @examples
#' \dontrun{
#' data(raw4d_ex_data)
#' clean_data <- clean4DData(raw4d_ex_data)
#' growthElaboration(clean_data, nweeks = 1)
#' }
#' 
#' @export

growthElaboration <- function(data4d, nweeks){
  data4d <- data4d %>%
    dplyr::mutate(tp = floor(lubridate::week(.data$date)/nweeks),
                  ftp = paste(.data$year, .data$tp, sep = "."))

  #group by time ids and time intervals and obtain mode
  gs <- data4d %>%
    dplyr::group_by(.data$id, .data$ftp) %>%
    dplyr::summarise(central_date = stats::median(.data$date, na.rm = T),
                     growth_mode_r = as.double(dplyr::last(DescTools::Mode(.data$growth_DN, na.rm = T))),
                     growth_mode_cm = growthConversion(growth_mode_r)) %>%
    dplyr::ungroup()

  #calculate the initial value measured by the sensor
  baseline <- gs[!is.na(gs$growth_mode_cm),] %>%
    dplyr::group_by(.data$id) %>%
    dplyr::filter(.data$central_date == min(.data$central_date)) %>%
    dplyr::select(.data$id, .data$growth_mode_cm) %>%
    dplyr::rename(initial_g = .data$growth_mode_cm) %>%
    dplyr::ungroup()

  #subtract the initial value
  gs <- dplyr::left_join(gs,
                   baseline,
                   by = c("id" = "id")) %>%
    dplyr::mutate(growth_s0 = .data$growth_mode_cm - .data$initial_g)
  return(gs)
}
