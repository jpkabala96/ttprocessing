
calculateRSI <- function(data4d){
  MinVal <- data4d %>%
    dplyr::group_by(.data$id, .data$f_year) %>%
    dplyr::summarise(Ecf_min = first(min(na.omit(.data$EcfHz_clean)))) %>%
    dplyr::left_join(data4d %>% dplyr::select(id, f_year, EcfHz_clean, Tref0),
                     by = c("id" ="id",
                            "f_year" = "f_year",
                            "Ecf_min" = "EcfHz_clean")) %>%
    dplyr::rename(Ecf_sat = Ecf_min ,
                  Tsat = .data$Tref0) %>%
    naniar::replace_with_na(replace = list(Ecf_sat = c(Inf,-Inf)))

  data4d <- data4d %>% dplyr::left_join(MinVal, by = c("id" = "id", "f_year" = "f_year") )

  data4d <- data4d %>%
    dplyr::mutate(Ecf_adj = -81 * (.data$Tref0 - .data$Tsat) + Ecf_sat,
                  RSI = (1 - (.data$EcfHz_clean - .data$Ecf_adj)/Ecf_adj)*100)
  return(data4d)

}

