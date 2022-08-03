#' Plot elaborated growth data
#' @description Function for plotting the growth data obtained with the
#'   \code{growthElaboration} function.
#' @param growth_data The growth data obtained with the \code{growthElaboration}
#'   function.
#' @param variable a string with the name of the variable to be plotted among
#'   the column names in the growth_data object. One of: "growth_mode_r",
#'   "growth_s0", "growth_mode_cm".
#' @param theme The theme to be used for the plot. One of: "bw", "classic",
#'   "grey" or "light" to select the corresponding ggplot theme.
#' @export
#'



plotGrowth <- function(growth_data,
                       variable = "growth_s0",
                       theme = "bw"){
  gplt <- ggplot2::ggplot()+
    ggplot2::geom_line(data = growth_data,
                       ggplot2::aes( x = .data$central_date,
                                     y = eval(parse(text = variable)),
                                     color = .data$id))+
    ggplot2::geom_point(data = growth_data,
                        ggplot2::aes( x = .data$central_date,
                                      y = eval(parse(text = variable)),
                                      color = .data$id))+
    ggplot2::geom_hline(yintercept = 0, color = "black")
  switch(theme,
         "bw" = {
           gplt <- gplt + ggplot2::theme_bw()
           print(gplt)
           return(gplt)
         },
         "classic" = {
           gplt <- gplt + ggplot2::theme_classic()
           print(gplt)
           return(gplt)
         },
         "grey" = {
           gplt <- gplt + ggplot2::theme_grey()
           print(gplt)
           return(gplt)
         },
         "light" = {
           gplt <- gplt + ggplot2::theme_light()
           print(gplt)
           return(gplt)
         }
  )
}
