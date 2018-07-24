
periodSelectUI <- function(id) {
  ns <- NS(id)
  
  tagList(  

    br()
    ,  verbatimTextOutput(ns("uiPeriodSelectResultMessage"))
  
    , selectInput(inputId=ns("environment"),
                    label = "Omgeving"
#                  , choices=setNames(environmentList[2-6],environmentList[2-6])
                  , choices=setNames(
                      c(
                        #'wrkEnvMain',
                        'wrkEnvA'
                        ,'wrkEnvB'
                        ,'wrkEnvC'
                        ,'wrkEnvD')
                      ,c(
                        #'Algemeen',
                         '1-A'
                        ,'1-B'
                        ,'1-C'
                        ,'1-D'))
                  , selected='wrkEnvMain'
    )              # Environment 'wrkEnvDefault' is not selectable
    , dateRangeInput(ns("inDateRange"), "Periode:"
                     , start = Sys.Date()-1
                     , end = Sys.Date()
    )
    # Input: Selector for choosing project ----
    , selectInput(inputId = ns("timeSeriesCode"),
                  label = "Time series:"
                  #choices=timeSeriesCodeList$timeSeriesDesc
                , choices=setNames(timeSeriesCodeList$timeSeriesCode,timeSeriesCodeList$timeSeriesDesc)
                , selected='H'
    )
    

    , actionButton(ns("addToPeriodSelectionButton"), "Selecteer periode")
    , uiOutput(ns("uiPeriodSelectMessage"))
    , br()
    , DTOutput(ns('uiDataTable2'))
    , wellPanel(
      verbatimTextOutput(ns("selectedPeriod"))
    )
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
  
  set_activeEnvironment(wrkEnvMain)
  
  
  print('init local period selection')
  localPeriodSelection<-globalPeriodSelection # copy empty tibble from global variable
  

  ##### get data reactive functions ########
  
  ##### end of get data reactive functions ########
  
  
  ##### observe section #######  
  observe({
    input$environment
    set_activeEnvironment(input$environment)
    updateDateRangeInput(session=session, inputId='inDateRange', label = NULL, start = get_wrkPeriod()[1],
                         end = get_wrkPeriod()[2], min = NULL, max = NULL)
    updateSelectInput(inputId='timeSeriesCode',selected=get_wrkTimeSeries(),session=session)
  })

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
    # t<-er()
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
  
  output$selectedPeriod <-renderText({
    print(get_active_wrkEnv_envName())
    input$environment
    wrkPeriod<-get_wrkPeriod()
    print('wrkPeriod')
    print(wrkPeriod)
    wrkTimeSeries<-get_wrkTimeSeries()
    paste(as.character(wrkPeriod[1]),as.character(wrkPeriod[2]),' time series: ', wrkTimeSeries)
  })
  
  observeEvent(input$addToPeriodSelectionButton, {
    print('button pressed')
    print(get_active_wrkEnv_envName())
    print(lnkEnvActive$envName)
    set_wrkPeriod(input$inDateRange)
    set_wrkTimeSeries(input$timeSeriesCode)
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
  
  observe({
    print('Date changed')
    
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
