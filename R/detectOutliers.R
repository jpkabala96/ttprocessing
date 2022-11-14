#' Flag ligth data outliers
#' 
#' @description This function flags light values as outliers according to the 
#'   "boxplot rule". In fact it flags as outliers all values that fall outside
#'   of the box and whisker of a box and whisker plot. The function computes the 
#'   0.75 and 0.25 quantiles of the data. A value is considered an outlier if it 
#'   falls outside the interval: \code{(Q25-1.5*IQR, Q75+1.5IQR)} and so is 
#'   flagged as outlier. This function takes a numeric vector as input and 
#'   returns a logical vector containing that flags (T means a value is an 
#'   outlier, F means it's not an outlier).
#' @param x A numeric vector
#' @return A logical vector containing the flags.


# create detect outlier function
detectOutliers <- function(x) {
  
  # calculate first quantile
  Quantile1 <- quantile(x, probs=.25)
  
  # calculate third quantile
  Quantile3 <- quantile(x, probs=.75)
  
  # calculate inter quartile range
  IQR = Quantile3-Quantile1
  
  # return true or false
  x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
}