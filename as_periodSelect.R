
periodSelectUI <- function(id) {
  ns <- NS(id)
  
  tagList(  

    br()
    ,  verbatimTextOutput(ns("uiPeriodSelectResultMessage"))
  
    , dateRangeInput(ns("inDateRange"), "Periode:")
    # Input: Selector for choosing project ----
    , selectInput(inputId = ns("timeSeriesCode"),
                label = "Time series:",
                choices=timeSeriesCodeList$timeSeriesDesc
    )
    

    , actionButton(ns("addToPeriodSelectionButton"), "Selecteer periode")
    , uiOutput(ns("uiPeriodSelectMessage"))
    
    , DTOutput(ns('uiDataTable2'))
  
  )
}

periodSelect <- function(input, output, session, param1) {
  ns<-session$ns 
  print('periodSelect start')
  
  # list of reactive values to return to calling module
  moduleResults <- reactiveValues(periodeSelection=NULL)
  ## list to store reactive values
  values <- reactiveValues(
    refreshTable=1
  )
  
  print('init sensorDatalist')
  periodeDatalist<-NULL
  
  
  print('init local period selection')
  localPeriodSelection<-globalPeriodSelection # copy empty tibble from global variable
  

  ##### get data reactive functions ########
  
  ##### end of get data reactive functions ########
  
  
  ##### observe section #######  

  ##### end of observe section #######  
  
  ##### output section #######  
  
  output$uiPeriodSelectButton <- renderUI({
    print('output select period button')
#    if (is.null(values$opId)) return(NULL)
#    if (is.na(values$opId)) return(NULL)
    values$refreshValue
    values$periodSelectionMessage<-NULL
    values$periodSelectionResultMessage<-NULL
    
  })
  output$uiPeriodSelectMessage <- renderText({
    print('output select period message')
    print(values$addToPeriodSelectionButtonMessage)
    values$addToPeriodSelectionButtonMessage 
  })
  output$uiPeriodSelectResultMessage <- renderText({
    print('output select period result message')
    t<-er()
    values$addToPeriodSelectionButtonResultMessage
  })
  output$uiDataTable2 <- DT::renderDataTable({
    values$refreshTable
    DT::datatable(localPeriodSelection)
  })
  proxy = dataTableProxy('uiDataTable2')
  observe({
    values$refreshTable
    replaceData(proxy, localPeriodSelection)
  })
  
  ##### end of output section #######  

  ##### event section #######  

  ##### end of event section #######  

  ##### function section #######  

  # Date range input =========================================
  # Only label and value can be set for date range input
#  updateDateRangeInput(session, "inDateRange",
#                       label = paste("Date range"),
#                       start = "2018-05-16", # format(Sys.time(), "%Y-%M-%D"),
#                       end = "2018-05-17",  #paste("2013-12-",  sep=""),
#                       min = paste("2017-01-01",  sep=""),
#                       max = paste("2030-12-31", sep="")
#  )
  
#  period<- reactive({
#    print('period')
#    print(input$inDateRange)
#    validate(
#      need(input$inDateRange)
#    )
#  })
  
  ##### end of function section #######  
  
  return (moduleResults)
}
#########################################################
