% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growthConversion.R
\name{growthConversion}
\alias{growthConversion}
\title{Growth conversion}
\usage{
growthConversion(growthDN)
}
\arguments{
\item{growthDN}{The raw value from the growth sensor (the DN). A numeric.}
}
\description{
Converts the growth DN to a measurment in cm according to the 
  equation in the Tree Talkers manual: 
  distanceCM <- (237908.4541-(growthDN*1.1171))/(199.4330+growthDN).
  The sign is reversed, as it indicates the distance of the sensor 
  from the tree, that decreases in time.
}
\examples{
\dontrun{
growthConversion(34567)
growthConversion(55609)
}


}
