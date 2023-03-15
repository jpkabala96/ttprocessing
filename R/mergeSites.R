#' Merge sites
#' @description This function allows the user to merge the clean TT data
#'   of some sites together. The function is a wrapper around the dplyr
#'    \code{bind_rows} function.
#' @param TT_data_frames list of TT data frames to be merged.
#' @param site_names Names of the sites to which the data.frames belong. 
#'   If not provided, by default, they will be named with progressive numbers in 
#'   the order they were supplied.
#'
#' @export

mergeSites <- function(TT_data_frames, site_names = as.character(c(1:length(TT_data_frames)))){
  assertthat::assert_that(is.list(TT_data_frames),
                          msg = "TT_data_frames must be a list
                          of dataframes containing the TT data")
  assertthat::assert_that(is.character(site_names),
                          msg = "Sites must be a character containing
                          the site names.")
  assertthat::assert_that(length(TT_data_frames) == length(site_names),
                          msg = "site_names must be the same length as
                          TT_data_frames")

  for(i in 1:length(TT_data_frames)){
    TT_data_frames[[i]]$site <- site_names[[i]]
  }
  binded <- dplyr::bind_rows(TT_data_frames, .id = NULL)
  return(binded)
}
