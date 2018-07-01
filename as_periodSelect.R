
periodSelectUI <- function(id) {
  ns <- NS(id)
  
  tagList(  

      uiOutput(ns("uiSelectPeriodeButton"))
    , uiOutput(ns("uiSelectPeriodeMessage"))
    , verbatimTextOutput(ns("uiSelectPeriodeResultMessage"))
  
    , dateRangeInput(ns("inDateRange"), "Periode:")
  
    , DTOutput(ns('uiDataTable'))
  
  )
}

periodSelect <- function(input, output, session, param1) {
  ns<-session$ns 
  print('periodSelect start')
  
  # list of reactive values to return to calling module
  moduleResults <- reactiveValues(periodeSelection=NULL)
  ## list to store reactive values
  values <- reactiveValues()
  
  print('init sensorDatalist')
  periodeDatalist<-NULL
  
  print('init local periode selection')
  localPeriodeSelection<-globalPeriodeSelection # copy empty tibble from global variable

  ##### get data reactive functions ########
  
  ##### end of get data reactive functions ########
  
  
  ##### observe section #######  

  ##### end of observe section #######  
  
  ##### output section #######  
  
  ##### end of output section #######  

  ##### event section #######  

  ##### end of event section #######  

  ##### function section #######  

  # Date range input =========================================
  # Only label and value can be set for date range input
  updateDateRangeInput(session, "inDateRange",
                       label = paste("Date range"),
                       start = "2018-05-16", # format(Sys.time(), "%Y-%M-%D"),
                       end = "2018-05-17",  #paste("2013-12-",  sep=""),
                       min = paste("2017-01-01",  sep=""),
                       max = paste("2030-12-31", sep="")
  )
  
  period<- reactive({
    print('period')
    print(input$inDateRange)
    validate(
      need(input$inDateRange)
    )
  })
  
  ##### end of function section #######  
  
  return (moduleResults)
}
#########################################################
