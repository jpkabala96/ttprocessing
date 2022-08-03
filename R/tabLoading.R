#' Create the UI loading Tab
#' @description Creates the loading tab for the UI
#' @export
#' 

tabLoading <- function(){
  tab <- shiny::tabPanel("Loading",
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      
                      shiny::radioButtons(inputId = "nDB",
                                          label = "Number of databases",
                                          choices = c("1","2"),
                                          selected = "1"),
                      
                      shiny::textInput("LinkServer1",
                                       label = "Link al server",
                                       value = "http://ittn.altervista.org/C0200085/ttcloud.txt"),
                      
                      shiny::textInput("LinkServer2",
                                       label = "link al vecchio server",
                                       value = "http://naturetalkers.altervista.org/C0200085/ttcloud.txt"),
                      
                      shiny::actionButton("carica", "carica i dati"),
                      
                      shiny::textInput("raw_data_name", "Dataset name"),
                      
                      shiny::downloadButton("raw_data", label = "Download raw data")
                    ),
                    shiny::mainPanel(shiny::dataTableOutput("dati"))
                  )
                  
  )
  return(tab)
}