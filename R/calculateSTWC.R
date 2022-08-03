#' @title Calculate Stem Water Content
#'
#' @description This function calculates the Stem Water Content from the TT data
#'   Nowdays formulas are available only for Fagus sylvatica and Pinus sylvestris.
#' @param Ecf_Hz The frequency measured by the probe for estimating stem water
#'   content.
#' @param Tref0 The temperature (C) measured by the reference probe inside the
#'   trunk.
#' @param species "Fagus" or "Pinus". Equations for calculating the Stem Water
#'   Content are species specific, so you must specify the species to use the
#'   right equation. Equations are available only for Fagus sylvatica and Pinus
#'   sylvestris at the moment. If an invalid string is entered the function
#'   raises an error.
#' @return A numeric vector with the % volumetric water content, estimated
#'   according to the equations in the TT+ manual.
#'
#'@export

calculateSTWC <- function(Ecf_Hz, Tref0, species = "Fagus"){
  # assertthat::assert_that(species %in% c("Fagus", "Pinus"),
  #                         msg = "There is no equation for this species")
  Ec_cor <- cleanSensorSTWC(Ecf_Hz) - 73.4 * (Tref0 - 29)
  fagus_STWC <- -0.0034*Ec_cor + 54.897
  pinus_STWC <- ifelse(Ec_cor > 14500,
                       -0.01 * Ec_cor + 173.53,
                       -0.0008 * Ec_cor + 30.023)
  print(glue::glue("Stem water content calculated using equation for {species}"))
  switch (species,
    Fagus = return(fagus_STWC),
    Pinus = return(pinus_STWC)
  )
}
