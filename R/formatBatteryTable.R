#' Format battery table
#'
#' @description Formats the battery table returned by \code{fastBatteryCheck}
#'   for properly displaying it in the GUI. The columns are converted to
#'   characters.
#' @param battery_table The battery table (the output of fastBatteryCheck)
#' @return The battery table properly formatted, with all columns converted
#'   to character.
#'
#' @export

formatBatteryTable <- function(battery_table){
  battery_table_formatted <- apply(battery_table, MARGIN = 2, as.character)
  return(battery_table_formatted)
}
