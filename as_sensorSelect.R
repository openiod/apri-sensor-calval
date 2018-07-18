
sensorSelectUI <- function(id) {
  ns <- NS(id)
  
  
#  fluidRow(
#    column(6, plotOutput(ns("plot1"), brush = ns("brush"))),
#    column(6, plotOutput(ns("plot2"), brush = ns("brush")))
#  )
  br()
  tagList(  
    br(),
    # Input: Selector for choosing project ----
    selectInput(inputId = ns("projectId"),
               label = "Project:",
               choices=projectList$projectName
    ),
    uiOutput(ns("uiProjectDetails")),
    br(),
  
    uiOutput(ns("uiFois")),
    uiOutput(ns("uiFoiDetails")),
    br(),
  
    uiOutput(ns("uiOps")),
    uiOutput(ns("uiOpDetails")),
    br(),

    uiOutput(ns("uiSelectSensorButton")),
    uiOutput(ns("uiSelectSensorMessage")),
    verbatimTextOutput(ns("uiSelectSensorResultMessage")),
    br(),
  
    DTOutput(ns('uiDataTable'))
  
  )
}

sensorSelect <- function(input, output, session, param1) {
  ns<-session$ns 
  print('sensorSelect start')
  
  # list of reactive values to return to calling module
  moduleResults <- reactiveValues(sensorSelection=NULL)
  ## list to store reactive values
  values <- reactiveValues(
    refreshTable=1
  )
  
#  sensorDatalist<-tribble(
#    ~foi, ~sensorType, ~date, ~treshold, ~sensorValue, ~tmp2, ~foiLocation, ~type
#  )
  print('init sensorDatalist')
  sensorDatalist<-NULL
  
  
  print('init local sensor selection')
#  localSensorSelection<-globalSensorSelection # copy empty tibble from global variable

  ##### get data reactive functions ########
  
  # get project data
  getProject<-reactive({
    print('getProject')
    projectList %>%
      filter(projectId==input$projectId)
  })
  # get project related feature of interests (sensor units) 
  getProjectFois<-reactive({
    print('getProjectFois')
    projectFoiList %>% filter(projectId==input$projectId) %>% inner_join(foiList, by = "foiId")
  })
  # get feature of interest data ( sensor unit)
  getFoi<-reactive({
    print('getFoi')
    print(values$foiId)
    t<-NA
    if (!is.null(values$foiId)) {
      if (!is.na(values$foiId)) {
        t<-filter(getProjectFois(),foiId==values$foiId)
      }
    } 
    #print(t)
    t
  })
  # get observable properties related to a feature of interest
  getFoiOps<-reactive({  
    print('getFoiOps')
#    print(values$foiId)
#    print(values$opId)
    t<-NULL
    if (!is.null(values$foiId)) {
      if (!is.na(values$foiId)) {
        t<-filter(foiList,foiId==values$foiId) %>%
          inner_join(foiOpList, by = "foiId")
      }  
    }  
    print(t)
    t
  })
  getOp<-reactive({
    print('getOp')
    print(values$opId)
    t<-NA
    if (!is.null(values$opId)) {
      if (!is.na(values$opId)) {
        t<-filter(getFoiOps(),foiId==values$foiId & opId==values$opId)
      }
    } 
    print(t)
    t
  })
  ##### end of get data reactive functions ########
  
  
  ##### observe section #######  

  ## Observe input projectId and display projectdetails and render input fois
  observe({
    print('observe input projectId')
    print(input$projectId)
    output$uiProjectDetails <- renderText({
      print('output uiProjectDetails')
      tmpProject<-getProject()
      paste('', if (input$projectId == '...') 'Selecteer een project ...' else paste(tmpProject$projectDesc,' (',tmpProject$projectAlias,')',sep=''))
    })
    values$projectId<-input$projectId
    
  })
  # input feature of interest
  observe({
    print('observe input$foiId')
    if (length(getProjectFois()$foiId)!=0) {
      values$foiId <- input$foiId
    } else {
      values$foiId <- NA
    } 
    # print(input$foiId)  # latest input selected value
    # print(values$foiId) # latest input selected value or NA when not available (disabled)
  })
  # input observable property
  observe({
    print('observe input$opId')
    if (length(getFoiOps()$foiId)!=0) {
      values$opId <- input$opId
    } else {
      values$opId <- NA
    } 
    #print(input$opId)  # latest input selected value
    #print(values$opId) # latest input selected value or NA when not available (disabled)
  })
  ##### end of observe section #######  
  
  ##### output section #######  
  
  output$uiFois = renderUI({
    print('output uiFois')
    t<-NA
    if (length(getProjectFois()$foiId)!=0) {
      t<-selectInput(ns("foiId"),"Unit",getProjectFois()$foiId) 
      print('project geselecteerd2')
      return(t)
    } else {
      print("Geen project geselecteerd dus fois input moet disabled worden")
      return(NULL)
    }
  })
  output$uiFoiDetails <- renderText({
    print('output uiFoiDetails')
    d<-getFoi()
    t<-NULL
    if (!is.na(d[1])) {
      t<-paste('', d$foiName,' (',d$foiIdShort,')'
               , if (length(getFoiOps()$opId)==0) ". Deze unit heeft geen sensoren geconfigureerd! "
               , sep='')
    }
    t
  })
  output$uiOps = renderUI({
    print('output uiOps: ')
    t<-NA
    if (length(getFoiOps()$opId)!=0) {
      t<-selectInput(ns("opId"),"Sensor",getFoiOps()$opId) 
      print('foiOps input select')
      return(t)
    } else {
      print("Geen fois met ops geselecteerd dus foiOps input moet disabled worden")
      return(NULL)
    }
  })
  output$uiOpDetails <- renderText({
    print('output opDetails')
    d<-getOp()
    if (length(d)==0) return(NULL)
    t<-NULL
    if (!is.null(d)) {
      if(!is.na(d[1])) {
        t<-paste('', d$opIdPrefix,d$opId,' (',d$opAlias,',',d$opUnit,')',sep='')
      } 
    }
    t
  })
  output$uiSelectSensorButton <- renderUI({
    print('output select sensor button')
    if (is.null(values$opId)) return(NULL)
    if (is.na(values$opId)) return(NULL)
    values$sensorSelectionMessage<-NULL
    values$sensorSelectionResultMessage<-NULL
    actionButton(ns("addToSensorSelectionButton"), "Selecteer sensor")
  })
  output$uiSelectSensorMessage <- renderText({
    print('output select sensor message')
    print(values$addToSensorSelectionButtonMessage)
    values$addToSensorSelectionButtonMessage 
  })
  output$uiSelectSensorResultMessage <- renderText({
    print('output select sensor result message')
    t<-er()
    values$addToSensorSelectionButtonResultMessage
  })
  output$uiDataTable <- DT::renderDataTable({
      values$refreshTable
      DT::datatable(get_wrkSensors())
  })
  proxy = dataTableProxy('uiDataTable')
  observe({
    values$refreshTable
    replaceData(proxy, get_wrkSensors())
  })
  
  #  , dataTableOutput(ns('uiDataTable'), {
  #    DT::renderDataTable({
  #      DT::datatable(localSensorSelection)
  #    })
  #  })
  ##### end of output section #######  

  ##### event section #######  
  er<-eventReactive(input$addToSensorSelectionButton, {
    print('sensor selection button event')
    print(get_wrkSensors())
    values$newSensorSelection<-getOp()
    values$addToSensorSelectionButtonResultMessage<-paste('Sensor ',isolate(values$opId),' toegevoegd aan de sensor selectie',sep="")
  })  
  
  ##### end of event section #######  

  ##### function section #######  
  

  observe({
    print('observe values newSensorSelection')
    if (is.null(values$newSensorSelection)) return(NULL) 
    if (length(values$newSensorSelection)==0) return(NULL)
    
    
    # tijdelijk hier?
    result <- callModule(sensorGetData,idList["name"=="sensorGetData_A"]$id, values$newSensorSelection, values$periodSelected, get_wrkTimeSeries());
    print(result$result())
    if (is.null(get_wrkData())) {
      print("init sensorDatalist")
      set_wrkData(result$result())
    }
    else {
      print("add to sensorDatalist")
      t<-full_join(get_wrkData(),result$result()
                   , by = c("foiName","opName","date"   
                            ,"opValue","opIdPrefix","opIdSep"
                            , "opId", "opTreshold", "foiIdImport", "type"))
      set_wrkData(t)
    }  
    print(summary(get_wrkData()))

    if (is.null(get_wrkSensors())) {
      print("init wrkSensors")
      set_wrkSensors(values$newSensorSelection)
    }
    else {
      #    print(tmpSensorData1)
      t<-full_join(get_wrkSensors(),values$newSensorSelection, by = c("foiId","foiName","foiIdShort","opId","opIdPrefix","foiIdSep","opIdSep","opAlias","opUnit"))
      set_wrkSensors(t)
    }
    values$refreshTable<<-values$refreshTable+1
    values$newSensorSelection<-NULL
    print('refresh table value is:')
    print(values$refreshTable)
    
    moduleResults$sensorSelection <-get_wrkSensors()
  })
  
  AddToSensorSelection<-reactive({
    print('AddToSensorSelection')
    
      values$newSensorSelection<-getOp()
      print(values$newSensorSelection)
      values$addToSensorSelectionButtonMessage<-'test5'

  })

  ##### end of function section #######  
  
#  return (moduleResults$sensorSelection())
  return (moduleResults)
  
}
#########################################################
