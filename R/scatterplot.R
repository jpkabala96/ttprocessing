#' Scatterplot of the TT variables
#' @description Wrapper around ggplot, that allows the user to make a
#'   scatterplot of the variables recorded by the tree talkers.
#' @param data The TT+ data cleaned
#' @param variable1 The variable to be represented on the x axis
#' @param variable2 The variable to be represented on the y axis
#' @param theme The theme to be used (one of "bw", "grey", "classic" and "light")
#' @param plot_title The title of the plot
#' @param x_label The label to be put on the x axis
#' @param y_label The label to be put on the y axis
#'
#'
#' @export


scatterplot <- function(data,
                        variable1 =  "Tair",
                        variable2 =  "do_sap_flow",
                        theme = "bw",
                        plot_title = "Plot",
                        x_label = variable1,
                        y_label = variable2){

  m1 <- median(eval(parse(text = paste0("data$", variable1))),na.rm = T)
  m2 <- median(eval(parse(text = paste0("data$", variable2))),na.rm = T)
  scplot <- ggplot2::ggplot(data = data)+
    ggplot2::geom_point(ggplot2::aes(x = eval(parse(text = variable1)),
                                     y = eval(parse(text = variable2))))+
    ggplot2::xlab(x_label)+
    ggplot2::ylab(y_label)+
    ggplot2::ggtitle(plot_title)

  switch (theme,
          "bw" = {
            scplot <- scplot+ggplot2::theme_bw()
          },
          "grey" = {
            scplot <- scplot+ggplot2::theme_grey()
          },
          "classic" = {
            scplot <- scplot + ggplot2::theme_classic()
          },
          "light" = {
            scplot <- scplot + ggplot2::theme_light()
          })

  print(scplot)
  return(scplot)

}
