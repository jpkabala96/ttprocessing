#' Remove flagged Outliers
#' @description Function to be applied to the output of flagOutliers. 
#'   This function removes outliers replacing them with NA from the variables flagged,
#'   by using the variables that end with _o, containing the flag. Then drops the 
#'   flag variables from the data.frame.
#' @param TTFlaggedData A data frame of TT+ data flagged with the before mentioned 
#'   flagging function.
#'   
#' @examples
#' \dontrun{
#' data("raw4d_ex_data")
#' data49 <- string49Handling(raw4d_ex_data)
#' data49_flagged <- flagOutliers(data49, timeperiod = "week")#flag the data
#' colnames(data49_flagged)
#' summary(data49_flagged)
#' data49_removed <- removeOutliers(data49_flagged)
#' colnames(data49_flagged)
#' summary(data49_flagged)
#' }
#' 
#'   
#' @export
#'   

removeOutliers <- function(TTFlaggedData){
  flagged_variables <- TTFlaggedData %>% 
    dplyr::select(tidyselect::ends_with("_o")) %>% colnames()
  original_variables <- flagged_variables %>%
    substr(start = 1, stop = nchar(flagged_variables)-2)
  print(original_variables)
  print(flagged_variables)
  
  for(i in 1:length(original_variables)){
    TTFlaggedData <- TTFlaggedData %>%
      dplyr::mutate("{original_variables[[i]]}" := ifelse(eval(parse(text = flagged_variables[[i]])) == T, 
                    NA, eval(parse(text = original_variables[[i]]))),
                    "{flagged_variables[[i]]}" := NULL)
  }
  return(TTFlaggedData)
}

