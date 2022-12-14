% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h24Clean.R
\name{h24Clean}
\alias{h24Clean}
\title{Removes days with NA's recorded during the day}
\usage{
h24Clean(data, cols_to_clean = "do_sap_flow", complete_required = 24)
}
\arguments{
\item{cols_to_clean}{A character vector with the names of the columns to clean.}

\item{complete_required}{Complete observations during the day, to retain the
values recorded during that day. Defaults to 24 (all the records during
 that day must be complete to retain the data of that day)}
}
\description{
Removes the data for all the days where there are only few
  complete records. The number of not NA records to retain the data of that
  day is to be specified in the complete_required argument (defaults to 24).
  Recommended to run this function on the Sap Flow Data.
  This because the Sap Flow estimation depends on the delta t max recorded
  during all the day by the probes. Invalid values, or not complete observation
  Might lead to a bad estimation of the Sap Flow.
  Can be applied also to the other columns.
}
