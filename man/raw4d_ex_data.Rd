% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{raw4d_ex_data}
\alias{raw4d_ex_data}
\alias{raw59_ex_data}
\alias{raw45_ex_data}
\alias{ttsoil_example}
\title{Example TT+ data}
\format{
An object of class \code{data.table} (inherits from \code{data.frame}) with 6582 rows and 30 columns.

An object of class \code{data.table} (inherits from \code{data.frame}) with 1787 rows and 30 columns.

An object of class \code{data.table} (inherits from \code{data.frame}) with 1317 rows and 30 columns.

An object of class \code{data.table} (inherits from \code{data.frame}) with 827 rows and 30 columns.
}
\usage{
raw4d_ex_data

raw59_ex_data

raw45_ex_data

ttsoil_example
}
\description{
These datasets contain example TT+ data, for trying the functionalities of 
this R package. 
raw4d_ex_data contains example data with 4D, 49, 4C and 4B data, 
and is an example of a dataset produced by the 3.0 TT+ setup.
This dataset can be used with the functions \code{clean4DData} and
 \code{string49Handling}, and then furhterly processed.
}
\details{
ttsoil_example contains example data of the ttsoil, with 4D string, 
and is an example of a dataset produced by the 3.0 TT+ setup.
This dataset can be used with the function \code{ttSoilDataProcessing}.
 
raw4d_ex_data contains example data with 45, 49, 4C and 4B data, 
and is an example of a dataset produced by the 2.0 TT+ setup.
This dataset can be used with the functions \code{string45Handling} and
 \code{string49Handling}, and then furhterly processed.

raw4d_ex_data contains example data with 59, 49, 4C and 4B data, 
and is an example of a dataset produced by the 3.0 TT+ setup.
This dataset can be used with the functions \code{clean59Data} and
 \code{string49Handling}, and then furhterly processed.
}
\keyword{datasets}
