% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanTAir.R
\name{cleanTAir}
\alias{cleanTAir}
\title{Clean Air Temperature}
\usage{
cleanTAir(x, lower.TAir = -15, higher.TAir = 50)
}
\arguments{
\item{x}{An air temperature vector of class numeric.}

\item{lower.TAir}{A value below which temperature is regarded as non plausible,
and thus replaced with NA.}

\item{higher.TAir}{A value above which temperature is regarded as non plausible,
and thus replaced with NA.}
}
\value{
A numeric vector, with non plausible temperature values replaced by NA's.
}
\description{
Cleans the air temperatures recorded by the Tree Talker, replacing
  with NA's the values outside the range specified by lower.TAir and higher.TAir.
}
