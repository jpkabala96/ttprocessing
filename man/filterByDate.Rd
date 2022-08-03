% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filterByDate.R
\name{filterByDate}
\alias{filterByDate}
\title{Filtering TT data by date}
\usage{
filterByDate(data, start = "2020-01-01", end = Sys.Date())
}
\arguments{
\item{data}{A dataframe with clean TT data, as for example the
output of \code{clean4DData} function.}

\item{start}{The first date of the period to consider. A Date object
or an object that is coercible to Date}

\item{end}{The last date to be considered. A Date object, or an object
that is coercible to Date.}
}
\description{
This function makes it easy to filter data by dates
}
\details{
This function makes use of the \code{dplyr} function \code{filter}
  to filter the data, and returns the filtered data.frame.
}