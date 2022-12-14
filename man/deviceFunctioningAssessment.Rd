% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deviceFunctioningAssesment.R
\name{deviceFunctioningAssessment}
\alias{deviceFunctioningAssessment}
\title{Graphical report of TT+ technical funtioning}
\usage{
deviceFunctioningAssessment(
  TTdata4d,
  id,
  start_date = "2021-01-01",
  end_date = "2021-12-31",
  min_valid_obs = 24
)
}
\arguments{
\item{TTdata4d}{Tree talker 4D data, as the dataframe produced by the
\code{clean4DData}) function.}

\item{id}{Device id as string. Must be one of the values stored in the 
4d data column id.}

\item{start_date}{Date of start of the time frame of interest.}

\item{end_date}{Date of end of the time frame of interest.}
}
\value{
A ggplot object, the plot with the graphical assesment of the functioning
  of the device corresponding to that ID.
}
\description{
This function creates a graphical report of dayly probe state and
  device battery voltage for a specific Tree talker during a time specified by
  the user.
}
