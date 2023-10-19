#' Launch the GUI for the package
#'
#' @description This function launches a shiny GUI for the package, that
#'   allows the user to clean, explore, plot and download the TT data.
#' @examples
#' \dontrun{
#'  # example code
#'  # The GUI can read TT+ data from a remote or local txt file. 
#'  #the txt file has to be formatted the same way as the TT+ data in the server.
#'  # to try it, the txt files with TT+ can be used from the github repository of 
#'  # this package.
#'  # If not, the example data can be saved to a local csv file 
#'  library(readr)
#'  data(raw4d_ex_data)
#'  #create a local file with data
#'  write_csv2(raw4d_ex_data, file = "filename.txt", col_names = F)
#'  # after launching the GUI, the path to the file created with write_csv2
#'  #has to be put as text input in the first panel, and then the data loading
#'  #button has to be pressed
#'  #after the data appear, they can be processed in the data cleaning tab
#'  #they can be saved to CSV, or plotted within the plotting tabs.
#'  launchGUI()
#'  
#' 
#' }
#'
#' 
#' @export




launchGUI <- function(){
  #choices for input
  summaries <- c("median",
                 "mean",
                 "none")
  themes <- c("bw", "grey", "classic", "light")
  #function for theme change
  theme_switch <- function(plt, theme){
    switch(theme,
           "bw" = {
             plt <- plt + ggplot2::theme_bw()
             print(plt)
             return(plt)
           },
           "grey" = {
             plt <- plt + ggplot2::theme_grey()
             print(plt)
             return(plt)
           },
           "classic" = {
             plt <- plt + ggplot2::theme_classic()
             print(plt)
             return(plt)
           },
           "light" = {
             plt <- plt + ggplot2::theme_light()
             print(plt)
             return(plt)
           }
    )
  }
  #user interface
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme(theme = "cyborg"),
    ###mette logo
    shiny::titlePanel("TreeTalker data processing",
                      windowTitle = "TT+ data processing"),
    shiny::tabsetPanel(
      shiny::tabPanel("Loading",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(

                          shiny::radioButtons(inputId = "nDB",
                                              label = "Number of databases",
                                              choices = c("1","2"),
                                              selected = "1"),

                          shiny::textInput("LinkServer1",
                                           label = "Link to the server or local file",
                                           value = "link to your tt-cloud or filepath"),

                          shiny::textInput("LinkServer2",
                                           label = "Link to another server or local file",
                                           value = "link to another tt-cloud or filepath"),

                          shiny::actionButton("carica", "load the data"),

                          shiny::textInput("raw_data_name", "Dataset name"),

                          shiny::downloadButton("raw_data", label = "Download raw data")
                        ),
                        shiny::mainPanel(shiny::dataTableOutput("dati"))
                        
                      )

      ),
      
      shiny::tabPanel("Data cleaning",
                      shiny::fluidRow(
                        shiny::column(width = 2, shiny::actionButton("pulisci", "Clean the data")),
                        shiny::column(width = 5, shiny::dateInput("datasetstartdate",
                                                                  label = "Start Date",
                                                                  value = "2020-01-01")),
                        shiny::column(width = 5, shiny::dateInput("datasetenddate",
                                                                  label = "End Date",
                                                                  value = Sys.Date())),

                      ),
                      shiny::fluidRow(
                        shiny::column(width = 12, shiny::uiOutput("selectids"))
                      ),
                      shiny::fluidRow(
                        shiny::column(width = 3, shiny::numericInput("l.TTree",
                                                                     label = "Min T inside the tree accepted (°C)",
                                                                     min = -100,
                                                                     max = 60,
                                                                     step = 1,
                                                                     value = 0)),
                        shiny::column(width = 3, shiny::numericInput("h.TTree",
                                                                     label = "Max T inside the tree accepted (°C)",
                                                                     min = 10,
                                                                     max = 60,
                                                                     step = 1,
                                                                     value = 40)),
                        shiny::column(width = 3, shiny::numericInput("l.Tair",
                                                                     label = "Min T air accepted (°C)",
                                                                     min = -100,
                                                                     max = 70,
                                                                     step = 1,
                                                                     value = -15)),
                        shiny::column(width = 3, shiny::numericInput("h.Tair",
                                                                     label = "Max T air accepted (°C)",
                                                                     min = 0,
                                                                     max = 70,
                                                                     step = 1,
                                                                     value = 55))
                      ),
                      shiny::fluidRow(
                        shiny::column(width = 3, shiny::numericInput("l.sap",
                                                                     label = "Min Sap flow accepted [g/(m^2*s)]",
                                                                     min = -20,
                                                                     max = 50,
                                                                     step = 1,
                                                                     value = 0)),
                        shiny::column(width = 3, shiny::numericInput("h.sap",
                                                                     label = "Max Sap flow accepted [g/(m^2*s)]",
                                                                     min = 0,
                                                                     max = 10000,
                                                                     step = 1,
                                                                     value = 1000)),
                        shiny::column(width = 3, shiny::numericInput("l.RH",
                                                                     label = "Min RH value accepted (%)",
                                                                     min = 0,
                                                                     max = 100,
                                                                     step = 1,
                                                                     value = 35)),
                        shiny::column(width = 3, shiny::numericInput("h.RH",
                                                                     label = "Max RH value accepted (%)",
                                                                     min = 0,
                                                                     max = 100,
                                                                     step = 1,
                                                                     value = 100))
                      ),
                      shiny::fluidRow(
                        shiny::column(width = 6, shiny::selectInput("species_input",
                                                                     label = "Species analyzed",
                                                                     choices = c("Fagus",
                                                                                 "Oak",
                                                                                 "Unknown"))),
                        shiny::column(width = 6, shiny::selectInput("timezone_input",
                                                                    label = "Timezone of the study site",
                                                                    choices = OlsonNames(),
                                                                    selected = Sys.timezone()))
                      ),
                      shiny::fluidRow(
                        shiny::column(width = 12, shiny::uiOutput("hoursselection"))),
                      shiny::fluidRow(
                        shiny::column(width = 4,
                                      shiny::textInput("data_name",
                                                       label = "Dataset filename")),
                        shiny::column(width = 4,
                                      shiny::downloadButton("clean_data",
                                                            label = "Download clean data"))
                      )
      ),

      shiny::tabPanel("Clean data",
                      dataTableOutput("data4d")),
      shiny::tabPanel("Battery",
                      shiny::splitLayout(cellWidths = c("30%","70%"),
                                         shiny::verticalLayout(shiny::textOutput("btt"),
                                                               shiny::tableOutput("v_table"),
                                                               shiny::textInput("outputTableName",
                                                                                label = "Filename for the table to be saved",
                                                                                value = "voltage_table"),
                                                               shiny::downloadButton("dv_table",
                                                                                     label = "Download battery table")),
                                         shiny::verticalLayout(shiny::textOutput("bpt"),
                                                               shiny::fluidRow(
                                                                 shiny::column(width = 3,
                                                                               shiny::dateInput("svp",
                                                                                                label = "Start date",
                                                                                                value = Sys.Date()- as.difftime(30,
                                                                                                                                units = "days"))),
                                                                 shiny::column(width = 3,
                                                                               shiny::dateInput("evp",
                                                                                                label = "End date",
                                                                                                value = Sys.Date()))
                                                               ),
                                                               shiny::plotOutput("bp"),
                                                               shiny::plotOutput("cbp"))#add battery voltage plot
                      )
      ),
      
      shiny::tabPanel("Functioning",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::dateInput("startdateAss",
                                           label = "Start Date",
                                           value = "2020-01-01"),
                          shiny::dateInput("enddateAss",
                                           label = "End Date",
                                           value = Sys.Date()),
                          shiny::uiOutput("idChoice"),
                          shiny::numericInput("nossAss", 
                                              label = "Minimum number of valid records",
                                              value = 24,
                                              min = 1, max = 24, step = 1),
                          
                          shiny::actionButton("plotAss",
                                              label = "Assess and plot"),
                          shiny::numericInput("assWidth", label = "Plot width (cm)",
                                              value = 8, min = 1, max = 49, step = 0.01),
                          shiny::numericInput("assHeight", label = "Plot height (cm)",
                                              value = 5, min = 1, max = 49, step = 0.01),
                          shiny::textInput("assFilename", label = "Plot filename",
                                           value = "plot"),
                          shiny::downloadButton("assDownload"),
                          width = 3
                        ),
                        
                        shiny::mainPanel(
                          shiny::plotOutput("assPlot")
                        )
                      )
      ),
      
      
      shiny::tabPanel("MH plots",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::uiOutput("MHchoice"),
                          shiny::selectInput("MHstat",
                                             label = "Statistic",
                                             choices = c("mean", "median"),
                                             selected = "median"),
                          shiny::dateInput("startdateMH",
                                           label = "Start Date",
                                           value = "2020-01-01"),
                          shiny::dateInput("enddateMH",
                                           label = "End Date",
                                           value = Sys.Date()),
                          shiny::textInput("MHtitle",
                                           label = "Plot title"),
                          shiny::selectInput("MHtheme",
                                             label = "Theme",
                                             choices = themes),
                          shiny::actionButton("plotMH",
                                              label  = "Plot"),
                          shiny::numericInput("MHwidth",
                                              label = "Plot width (cm)",
                                              value = 8,
                                              min = 1,
                                              max = 50,
                                              step = 0.1),
                          shiny::numericInput("MHheight",
                                              label = "Plot height (cm)",
                                              value = 5,
                                              min = 1,
                                              max = 50,
                                              step = 0.1),
                          shiny::textInput("outputMHName",
                                           label  = "Plot filename"),
                          shiny::downloadButton("downloadMHplot"),
                          width = 3
                        ),
                        mainPanel = shiny::mainPanel(shiny::plotOutput("MHplot"))
                      )

      ),
      shiny::tabPanel(title = "Plot by Quarters",
                      shiny::sidebarLayout(
                        sidebarPanel = shiny::sidebarPanel(
                          shiny::uiOutput("Qchoice"),
                          shiny::selectInput("Qsummary",
                                             label = "Summary statistic",
                                             choices = c("median", "mean"),
                                             selected = "median"),
                          shiny::dateInput("Qsdate",
                                           label = "Start date",
                                           value = "2020-01-01"),
                          shiny::dateInput("Qedate",
                                           label = "End date",
                                           value = Sys.Date()),
                          shiny::textInput("Qylab",
                                           label = "y axis label",
                                           value = "y"),
                          shiny::selectInput("Qtheme",
                                             label = "Theme",
                                             choices = themes),
                          shiny::actionButton("Qplot",
                                              label = "Plot"),
                          shiny::numericInput("Qwidth",
                                              label = "Plot width (cm)",
                                              value = 8,
                                              min = 1,
                                              max = 50,
                                              step = 0.1),
                          shiny::numericInput("Qheight",
                                              label = "Plot height (cm)",
                                              value = 5,
                                              min = 1,
                                              max = 50,
                                              step = 0.1),
                          shiny::textInput("outputQName",
                                           label  = "Plot filename"),
                          shiny::downloadButton("downloadQplot",
                                                label = "Download plot"),
                          width = 3
                        ),
                        mainPanel = shiny::mainPanel(shiny::plotOutput("Qplot"))
                      )
      ),
      shiny::tabPanel("TS plot",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::uiOutput("TSchoice"),
                          shiny::selectInput("summary",
                                             label = "Summary",
                                             choices = summaries),
                          shiny::dateInput("startdateTS",
                                           label = "Start Date",
                                           value = "2020-01-01"),
                          shiny::dateInput("enddateTS",
                                           label = "End Date",
                                           value = Sys.Date()),
                          shiny::textInput("TStitle",
                                           label = "Plot Title",
                                           value = "title"),
                          shiny::textInput("TSylab",
                                           label = "Y lab",
                                           value = "ylab"),
                          shiny::numericInput("TSymax",
                                              label = "Y axis max value",
                                              value = 100),
                          shiny::numericInput("TSymin",
                                              label = "Y axis min value",
                                              value = 0),
                          shiny::selectInput("TStheme",
                                             label = "Theme",
                                             choices = themes),
                          shiny::actionButton("TSplot",
                                              label = "Plot"),
                          shiny::textInput("outputTSName",
                                           label = "Output plot filename",
                                           value = "plot"),
                          shiny::numericInput("TSPlotWidth",
                                              label = "Plot width (cm)",
                                              value = 8, min = 1, max = 50,
                                              step = 0.1),
                          shiny::numericInput("TSPlotHeight",
                                              label = "Plot height (cm)",
                                              value = 5, min = 1, max = 50,
                                              step = 0.1),
                          shiny::downloadButton("downloadTSplot"),
                          width = 3
                        ),
                        mainPanel = shiny::mainPanel(shiny::plotOutput("TSplot"))
                      )
      ),
      shiny::tabPanel(title = "Plot With shade",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::uiOutput("shadevar"),
                          shiny::selectInput("shadestat",
                                             label = "Summary",
                                             choices = summaries),
                          shiny::dateInput("shadesdate",
                                           label = "Start date",
                                           value = Sys.Date()-as.difftime(7, units = "days")),
                          shiny::dateInput("shadeedate",
                                           label = "End date",
                                           value = Sys.Date()),
                          shiny::numericInput("shadelat",
                                              label = "Site latitude",
                                              value = 40,
                                              min = -90,
                                              max = 90,
                                              step = 0.001),
                          shiny::numericInput("shadelon",
                                              label = "Site longitude",
                                              value = 14,
                                              min = -180,
                                              max = 180,
                                              step = 0.001),
                          shiny::textInput("shadeylab",
                                           label = "Y axis label"),
                          shiny::textInput("shadetitle",
                                           label  = "Plot title",
                                           value = ""),
                          shiny::actionButton("plotshade",
                                              label = "Plot"),
                          shiny::numericInput("shadeh", label = "Plot height",
                                              value = 5,
                                              min = 1, max = 50),
                          shiny::numericInput("shadew", label = "Plot width",
                                              value = 8,
                                              min = 1, max = 50),
                          shiny::textInput("shadename",
                                           label = "Plot filename"),
                          shiny::downloadButton("downloadShadePlot"),
                          width = 3
                        ),
                        shiny::mainPanel(shiny::plotOutput("shadeplot"))
                      )),
      shiny::tabPanel(title = "Growth",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::uiOutput("gvarselect"),
                          shiny::numericInput("gnweeks",
                                              label = "Number of weeks to be aggregated",
                                              value = 2,
                                              min = 1,
                                              max = 8),
                          shiny::selectInput("growththeme",
                                             label = "Plot theme",
                                             choices = themes),
                          shiny::dateInput("gsdate",
                                           label = "Start date",
                                           value = "2020-01-01"),
                          shiny::dateInput("gedate",
                                           label = "End date",
                                           value = Sys.Date()),
                          shiny::numericInput("gymin",
                                              label = "Min y axis value",
                                              value = -5,
                                              step = 0.1),
                          shiny::numericInput("gymax",
                                              label = "Max y axis value",
                                              value = 100000,
                                              step = 0.1),
                          shiny::textInput("growthylab",
                                           label = "Y axis label"),
                          shiny::textInput("growthtitle",
                                           label = "Plot title"),
                          shiny::actionButton("plotgrowth",
                                              label = "Plot"),
                          shiny::numericInput("gpw",
                                              label = "Plot width",
                                              value = 8,
                                              min = 1,
                                              max = 50),
                          shiny::numericInput("gph",
                                              label = "Plot height",
                                              value = 5,
                                              min = 1,
                                              max = 50),
                          shiny::textInput("growthPlotName",
                                           label = "Plot filename"),
                          shiny::downloadButton("downloadGrowthPlot",
                                                label = "Download growth plot"),
                          shiny::textInput("growthDataName",
                                           label = "Growth dataset name"),
                          shiny::downloadButton("downloadGrowthData",
                                                label = "Download growth data"),
                          width = 3
                        ),
                        shiny::mainPanel(shiny::verticalLayout(shiny::plotOutput("growthPlt"),
                                                               shiny::dataTableOutput("growthdata"))
                        )
                      )),

      tabsc <- shiny::tabPanel(title = "Scatterplot",
                               shiny::sidebarLayout(
                                 shiny::sidebarPanel(
                                   shiny::uiOutput("var1choice"),
                                   shiny::uiOutput("var2choice"),
                                   shiny::selectInput("sctheme",
                                                      label = "theme",
                                                      choices = themes,
                                                      selected = "bw"),
                                   shiny::textInput("scxlab",
                                                    label = "X axis label"),


                                   shiny::numericInput("scxmin",
                                                       label = "X min value",
                                                       value = 0,
                                                       step = 0.01),

                                   shiny::numericInput("scxmax",
                                                       label = "X max value",
                                                       value = 100,
                                                       step = 0.01),

                                   shiny::textInput("scylab",
                                                    label = "Y axis label"),


                                   shiny::numericInput("scymin",
                                                       label = "Y min value",
                                                       value = 0,
                                                       step = 0.01),

                                   shiny::numericInput("scymax",
                                                       label = "Y max value",
                                                       value = 100,
                                                       step = 0.01),

                                   shiny::textInput("sctitle",
                                                    label = "Plot title"),
                                   shiny::actionButton("scupdate",
                                                       label = "Plot"),
                                   shiny::textInput("scfilename",
                                                    label = "Filename",
                                                    value = "plot"),
                                   shiny::numericInput("scw",
                                                       label = "Plot width (cm)",
                                                       value = 8,
                                                       max = 50,
                                                       min = 1,
                                                       step = 0.1),
                                   shiny::numericInput("sch",
                                                       label = "Plot height (cm)",
                                                       value = 5, max = 50, min = 1,
                                                       step = 0.1),
                                   shiny::downloadButton("downloadscplot",
                                                         label = "Download plot"),
                                   width = 3
                                 ),
                                 mainPanel = shiny::mainPanel(

                                   shiny::plotOutput("scplot"),
                                 ))
      )
    )


  )

  server <- function(input, output){

  
    #DataSource <- eventReactive(input$carica, url(input$LinkServer))
    dati1 <- shiny::eventReactive(input$carica,
                                  {readTTData(input$LinkServer1)})
    dati2 <- shiny::eventReactive(input$carica,
                                  {readTTData(input$LinkServer2)})
    dati <- shiny::reactive(
      switch (input$nDB,
              "1" = dati1(),
              "2" = mergeTTDB(dati1(),dati2()))
    )
    #obtain cloud data 4B
    cloud_data <- shiny::reactive(cloudData4B(dati()))
    output$dati <- shiny::renderDataTable(dati())
    dati_clean <- shiny::eventReactive(input$pulisci,
                                       filterByDate(h24Clean(clean4DData(dati(),
                                                                         lower.TTree = input$l.TTree,
                                                                         higher.TTree = input$h.TTree,
                                                                         lower.TAir = input$l.Tair,
                                                                         higher.TAir = input$h.Tair,
                                                                         lower.RH = input$l.RH,
                                                                         higher.RH = input$h.RH,
                                                                         lower.sap.flow = input$l.sap,
                                                                         higher.sap.flow = input$h.sap,
                                                                         species = input$species_input,
                                                                         tz = input$timezone_input)),
                                                    start = input$datasetstartdate,
                                                    end = input$datasetenddate))
    ids <- shiny::reactive(unique(dati_clean()$id))
    dati_filtered <- shiny::reactive(filterIDs(dati_clean(), ids = input$idstotake))

    light_data <- shiny::eventReactive(input$pulisci,
                                       dati() %>%
                                         string49Handling() %>%
                                         filterByDate(start = input$datasetstartdate,
                                                      end = input$datasetenddate))
    joined_data <- shiny::reactive(join4D49(dati_filtered(),
                                            light_data()))
    output$hoursselection <- shiny::renderUI(checkboxGroupInput("selecthours",
                                                                label = "Select the hours of interest",
                                                                choices = hours(),
                                                                selected = hours(),
                                                                inline = T))

    jf_data <- shiny::reactive(joined_data() %>% filterByHour(hours = input$selecthours))
    jf_variables <- shiny::reactive(colnames(jf_data()))
    #all variables
    variables <- shiny::reactive(colnames(dati_filtered()))
    light_variables <- shiny::reactive(colnames(light_data()))
    jvariables <- shiny::reactive(colnames(joined_data()))
    hours <- shiny::reactive(unique(joined_data()$f_hour))
    #numeric variables for plotting
    numeric_variables <- shiny::reactive(colnames(dplyr::select_if(jf_data(),
                                                                   is.numeric)))

    output$selectids <- shiny::renderUI(shiny::checkboxGroupInput("idstotake",
                                                                  label = "Ids to be considered",
                                                                  choices = ids(),
                                                                  selected = ids(),
                                                                  inline = T))

    output$data4d <- shiny::renderDataTable(jf_data())
    #creo la tabellina dei voltaggi


    tabella_voltaggi <- shiny::reactive(formatBatteryTable(batteryCheck(dati())))
    output$v_table <- shiny::renderTable(tabella_voltaggi())
    output$dv_table <- shiny::downloadHandler(
      filename = function() { paste0(input$outputTableName, '.pdf') },
      content = function(file) {

        grDevices::pdf(file, paper = "a4")
        graphics::plot.new()
        gridExtra::grid.table(tabella_voltaggi())
        graphics::title(main = "Tree Talkers voltage table",
                        sub = paste(input$LinkServer1,
                                    Sys.time()))
        grDevices::dev.off()

      }
    )
    #devices battery plot
    battery_plot <- shiny::reactive(plotTS(dati_clean() %>%
                                             filterByDate(start = input$svp,
                                                          end = input$evp),
                                           "voltage",
                                           statistic = "none")+
                                      ggplot2::ggtitle("Battery voltage plot")+
                                      ggplot2::ylab("Battery voltage (mV)"))
    #cloud battery plot
    cb_plot <- shiny::reactive(ggplot2::ggplot(cloud_data() %>%
                                                 dplyr::filter(.data$date_hour > input$svp) %>%
                                                 dplyr::filter(.data$date_hour < input$evp))+
                                 ggplot2::geom_line(ggplot2::aes(x = .data$date_hour,
                                                                 y = as.numeric(.data$voltage)))+
                                 ggplot2::theme_bw()+
                                 ggplot2::ggtitle("Cloud battery voltage")+
                                 ggplot2::ylab("Voltage (mV)"))

    output$bp <- shiny::renderPlot(battery_plot())
    output$cbp <- shiny::renderPlot(cb_plot())
    output$bpt <- shiny::renderText("Battery voltage plot")
    output$btt <- shiny::renderText("Battery voltage table")
    
    ## CHOOSE ID TO ASSESS
    output$idChoice <- shiny::renderUI(
      shiny::selectInput("ID_to_assess",
                         label = "ID to be assessed",
                         choices = ids())
    )
    
    
    ##Device assessment plot
    assessment_plot <- shiny::eventReactive(input$plotAss, 
                         deviceFunctioningAssessment(jf_data(),
                                                     id = input$ID_to_assess,
                                                     start_date = input$startdateAss,
                                                     end_date = input$enddateAss, 
                                                     min_valid_obs = input$nossAss)) 
    
    output$assPlot <- shiny::renderPlot(assessment_plot())
    
    output$assDownload <- shiny::downloadHandler(
      filename = function() { paste0(input$assFilename, '.png') },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = assessment_plot(),
                        device = "png",
                        units = "cm",
                        width = input$assWidth,
                        height = input$assHeight)
      }
    )
    
    
    #PLOT BY QUARTER
    output$Qchoice <- output$TSchoice <- shiny::renderUI(selectInput("vartoplotQ",
                                                                     label = "Variable to be plotted",
                                                                     choices = numeric_variables()))
    output$qylab <- shiny::reactive(input$vartoplotQ)
    Qplot <- shiny::eventReactive(input$Qplot,
                                  theme_switch(plotByQuarter(filterByDate(jf_data(),
                                                                          start = input$Qsdate,
                                                                          end = input$Qedate),
                                                             variable = input$vartoplotQ,
                                                             statistic = input$Qsummary)+
                                                 ggplot2::ggtitle(input$Qtitle)+
                                                 ggplot2::ylab(input$Qylab),
                                               input$Qtheme)
    )
    output$Qplot <- shiny::renderPlot(Qplot())
    output$downloadQplot <- shiny::downloadHandler(
      filename = function() { paste(input$outputQName, '.png', sep='') },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = Qplot(),
                        device = "png",
                        units = "cm",
                        width = input$Qwidth,
                        height = input$Qheight)
      }
    )

    #TS PLOT
    output$TSchoice <- shiny::renderUI(selectInput("vartoplotTS",
                                                   label = "Variable to be plotted",
                                                   choices = numeric_variables()))

    TSplot <- shiny::eventReactive(input$TSplot,
                                   theme_switch(plotTS(filterByDate(jf_data(),
                                                                    start = input$startdateTS,
                                                                    end = input$enddateTS),
                                                       variable = input$vartoplotTS,
                                                       statistic = input$summary)+
                                                  ggplot2::ggtitle(input$TStitle)+
                                                  ggplot2::ylab(input$TSylab)+
                                                  ggplot2::ylim(c(input$TSymin, input$TSymax)),
                                                input$TStheme))
    output$TSplot <- shiny::renderPlot(TSplot())
    output$downloadTSplot <- shiny::downloadHandler(
      filename = function() { paste(input$outputTSName, '.png', sep='') },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = TSplot(),
                        device = "png",
                        units = "cm",
                        width = input$TSPlotWidth,
                        height = input$TSPlotHeight)
      }
    )
    #PLOT WITH SHADE
    output$shadevar <- shiny::renderUI(shiny::selectInput("shadevariable",
                                                          label = "Variable to plot",
                                                          choices = numeric_variables()))
    shadeplot <- shiny::eventReactive(input$plotshade,
                                      plotWithShade(jf_data() %>%
                                                      filterByDate(start = input$shadesdate,
                                                                   end = input$shadeedate),
                                                    variable = input$shadevariable,
                                                    statistic = input$shadestat,
                                                    lat = input$shadelat,
                                                    lon = input$shadelon)+
                                        ggplot2::ggtitle(input$shadetitle)+
                                        ggplot2::ylab(input$shadeylab))
    output$shadeplot <- shiny::renderPlot(shadeplot())
    output$downloadShadePlot <- shiny::downloadHandler(
      filename = function() { paste0(input$shadename, '.png') },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = shadeplot(),
                        device = "png",
                        units = "cm",
                        width = input$shadew,
                        height = input$shadeh)
      }
    )

    #scatter plot
    output$var1choice <- shiny::renderUI(selectInput("scv1",
                                                     label = "Variable on x axis",
                                                     choices = numeric_variables()))
    output$var2choice <- shiny::renderUI(selectInput("scv2",
                                                     label = "Variable on y axis",
                                                     choices = numeric_variables()))
    scplot <- shiny::eventReactive(input$scupdate,
                                   scatterplot(jf_data(),
                                               variable1 = input$scv1,
                                               variable2 = input$scv2,
                                               theme = input$sctheme,
                                               x_label = input$scxlab,
                                               y_label = input$scylab,
                                               plot_title = input$sctitle)+
                                     ggplot2::xlim(c(input$scxmin, input$scxmax))+
                                     ggplot2::ylim(c(input$scymin, input$scymax)))
    output$scplot <- shiny::renderPlot(scplot())
    output$downloadscplot <- shiny::downloadHandler(
      filename = function() { paste(input$scfilename, '.png', sep='') },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = scplot(),
                        device = "png",
                        units = "cm",
                        width = input$scw,
                        height = input$sch)
      }
    )
    #plotByMH
    output$MHchoice <- shiny::renderUI(shiny::selectInput("vartoplotMH",
                                                          label = "Variable to be plotted",
                                                          choices = numeric_variables()))
    MHplot <- shiny::eventReactive(input$plotMH,
                                   theme_switch(plotByMH(clean_data = filterByDate(jf_data(),
                                                                                   start = input$startdateMH,
                                                                                   end = input$enddateMH),
                                                         variable = input$vartoplotMH,
                                                         statistic = input$MHstat)+
                                                  ggplot2::ggtitle(input$MHtitle),
                                                input$MHtheme))

    output$MHplot <- shiny::renderPlot(MHplot())

    output$downloadMHplot <- shiny::downloadHandler(
      filename = function() { paste0(input$outputMHName, '.png') },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = MHplot(),
                        device = "png",
                        units = "cm",
                        width = input$MHwidth,
                        height = input$MHheight)
      }
    )


    #ELABORATE AND PLOT GROWTH
    growth <- shiny::reactive(growthElaboration(dati_filtered() %>%
                                                  filterByDate(start = input$gsdate,
                                                               end = input$gedate),

                                                nweeks = input$gnweeks))

    growthPlot <- shiny::eventReactive(input$plotgrowth,
                                       plotGrowth(growth(),
                                                  variable = input$growthvar,
                                                  theme = input$growththeme)+
                                         ggplot2::ylab(input$growthylab)+
                                         ggplot2::ggtitle(input$growthtitle)+
                                         ggplot2::ylim(c(input$gymin, input$gymax)))

    growthVariables <- shiny::reactive(colnames(growth()))

    output$gvarselect <- shiny::renderUI(
      shiny::selectInput("growthvar",
                         label = "Variable to plot",
                         choices = growthVariables())
    )

    output$growthPlt <- shiny::renderPlot(growthPlot())
    output$growthdata <- shiny::renderDataTable(growth())

    output$downloadGrowthPlot <- shiny::downloadHandler(
      filename = function() { paste0(input$growthPlotName, '.png') },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = growthPlot(),
                        device = "png",
                        units = "cm",
                        width = input$gpw,
                        height = input$gph)
      }
    )

    output$downloadGrowthData <- shiny::downloadHandler(
      filename = function() {
        paste0(input$growthDataName, ".csv")
      },
      content = function(file) {
        write.csv(growth(), file)
      }
    )



    #data download
    output$raw_data <- shiny::downloadHandler(
      filename = function() {
        paste0(input$raw_data_name, ".csv")
      },
      content = function(file) {
        write.csv(dati(), file)
      }
    )
    output$clean_data <- shiny::downloadHandler(
      filename = function() {
        paste0(input$data_name, ".csv")
      },
      content = function(file) {
        write.csv(joined_data(), file)
      }
    )

  }

  shiny::shinyApp(ui = ui, server = server)


}


