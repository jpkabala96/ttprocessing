% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removeOutliers.R
\name{removeOutliers}
\alias{removeOutliers}
\title{Remove flagged Outliers}
\usage{
removeOutliers(TTFlaggedData)
}
\arguments{
\item{TTFlaggedData}{A data frame of TT+ data flagged with the before mentioned 
flagging function.}
}
\description{
Function to be applied to the output of flagOutliers. 
  This function removes outliers replacing them with NA from the variables flagged,
  by using the variables that end with _o, containing the flag. Then drops the 
  flag variables from the data.frame.
}
