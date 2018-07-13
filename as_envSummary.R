
envSummaryUI <- function(id) {
  ns <- NS(id)
  
  tagList(  

      br()
    , verbatimTextOutput(ns("uiEnvSummaryResultMessage"))
  
    , div(
        h1('Samenvatting van geselecteerde gegevens')
      , h2('Omgeving Algemeen')  
      , h3('Periode')
      , uiOutput(ns("summaryWrkPeriodMain"))
      , h3('Sensor(en)')
      , uiOutput(ns("summaryWrkSensorsMain"))
      , h3('Sensor data')
      , DTOutput(ns("summaryWrkDataMainDT"))
      , h2('Omgeving A')  
      , h3('Periode')
      , uiOutput(ns("summaryWrkPeriodA"))
      , h3('Sensor(en)')
      , uiOutput(ns("summaryWrkSensorsA"))
      , h3('Sensor data')
      , DTOutput(ns("summaryWrkDataADT"))
      , h2('Omgeving B')  
      , h3('Periode')
      , uiOutput(ns("summaryWrkPeriodB"))
      , h3('Sensor(en)')
      , uiOutput(ns("summaryWrkSensorsB"))
      , h3('Sensor data')
      , DTOutput(ns("summaryWrkDataBDT"))
      , h2('Omgeving C')  
      , h3('Periode')
      , uiOutput(ns("summaryWrkPeriodC"))
      , h3('Sensor(en)')
      , uiOutput(ns("summaryWrkSensorsC"))
      , h3('Sensor data')
      , DTOutput(ns("summaryWrkDataCDT"))
      , h2('Omgeving D')  
      , h3('Periode')
      , uiOutput(ns("summaryWrkPeriodD"))
      , h3('Sensor(en)')
      , uiOutput(ns("summaryWrkSensorsD"))
      , h3('Sensor data')
      , DTOutput(ns("summaryWrkDataDDT"))
    )

  )
}

envSummary <- function(input, output, session) {
  ns<-session$ns 
  print('envSummary start')
  
  # list of reactive values to return to calling module
  moduleResults <- reactiveValues()
  ## list to store reactive values
  values <- reactiveValues(
  )
  

  ##### get data reactive functions ########
  
  ##### end of get data reactive functions ########
  
  
  ##### observe section #######  
  observe({
    print("##### observe envSummary Main")
    #values$gWrkPeriodUpdate()
    print('observe get_wrkPeriod()')
    wrkPeriod<-get_wrkPeriod(wrkEnvMain)
    wrkTimeSeries<-get_wrkTimeSeries(wrkEnvMain)
    wrkData<-get_wrkData(wrkEnvMain)
    print(wrkPeriod)
    #    if (is.null(wrkPeriod$value)) return
    #    output$summaryWrkPeriod<-renderText({
    #      wrkPeriod
    #    })  
    output$summaryWrkPeriodMain<-renderUI({
      tagList(
        tags$p(paste("van tot:",wrkPeriod[1],'/',wrkPeriod[2]))
        ,tags$p(paste("time series: ",wrkTimeSeries,'/',timeSeriesCodeList$timeSeriesDesc[timeSeriesCodeList$timeSeriesCode==wrkTimeSeries]))
        , br()
      )
    })
  })
  
  observe({
    get_wrkDataChanged(wrkEnvMain)
    output$summaryWrkDataMainDT <- DT::renderDataTable({
      DT::datatable(get_wrkData(wrkEnvMain))
    })
  })
  observe({
    get_wrkDataChanged(wrkEnvA)
    output$summaryWrkDataADT <- DT::renderDataTable({
      DT::datatable(get_wrkData(wrkEnvA))
    })
  })
  observe({
    get_wrkDataChanged(wrkEnvB)
    output$summaryWrkDataBDT <- DT::renderDataTable({
      DT::datatable(get_wrkData(wrkEnvB))
    })
  })
  observe({
    get_wrkDataChanged(wrkEnvC)
    output$summaryWrkDataCDT <- DT::renderDataTable({
      DT::datatable(get_wrkData(wrkEnvC))
    })
  })
  observe({
    get_wrkDataChanged(wrkEnvD)
    output$summaryWrkDataDDT <- DT::renderDataTable({
      DT::datatable(get_wrkData(wrkEnvD))
    })
  })
  
  
    observe({
    print("##### observe envSummary A")
    #values$gWrkPeriodUpdate()
#    print('observe get_wrkPeriod()')
    wrkEnvA$values$wrkPeriod
    wrkEnvMain$lnkEnvActive$envName

    wrkPeriod<-get_wrkPeriod(wrkEnvA)
    wrkTimeSeries<-get_wrkTimeSeries(wrkEnvA)
    print(wrkPeriod)
    #    if (is.null(wrkPeriod$value)) return
    #    output$summaryWrkPeriod<-renderText({
    #      wrkPeriod
    #    })  
    output$summaryWrkPeriodA<-renderUI({
      tagList(
        tags$p(paste("van tot:",wrkPeriod[1],'/',wrkPeriod[2]))
        ,tags$p(paste("time series: ",wrkTimeSeries,'/',timeSeriesCodeList$timeSeriesDesc[timeSeriesCodeList$timeSeriesCode==wrkTimeSeries]))
      )
    })  
  })
  
  observe({
    print("##### observe envSummary B")
    #values$gWrkPeriodUpdate()
    #    print('observe get_wrkPeriod()')
    wrkEnvA$values$wrkPeriod
    wrkEnvMain$lnkEnvActive$envName
    
    wrkPeriod<-get_wrkPeriod(wrkEnvB)
    wrkTimeSeries<-get_wrkTimeSeries(wrkEnvB)
    print(wrkPeriod)
    #    if (is.null(wrkPeriod$value)) return
    #    output$summaryWrkPeriod<-renderText({
    #      wrkPeriod
    #    })  
    output$summaryWrkPeriodB<-renderUI({
      tagList(
        tags$p(paste("van tot:",wrkPeriod[1],'/',wrkPeriod[2]))
        ,tags$p(paste("time series: ",wrkTimeSeries,'/',timeSeriesCodeList$timeSeriesDesc[timeSeriesCodeList$timeSeriesCode==wrkTimeSeries]))
      )
    })  
  })
  
  observe({
    print("##### observe envSummary C")
    #values$gWrkPeriodUpdate()
    #    print('observe get_wrkPeriod()')
    wrkEnvA$values$wrkPeriod
    wrkEnvMain$lnkEnvActive$envName
    
    wrkPeriod<-get_wrkPeriod(wrkEnvC)
    wrkTimeSeries<-get_wrkTimeSeries(wrkEnvC)
    print(wrkPeriod)
    #    if (is.null(wrkPeriod$value)) return
    #    output$summaryWrkPeriod<-renderText({
    #      wrkPeriod
    #    })  
    output$summaryWrkPeriodC<-renderUI({
      tagList(
        tags$p(paste("van tot:",wrkPeriod[1],'/',wrkPeriod[2]))
        ,tags$p(paste("time series: ",wrkTimeSeries,'/',timeSeriesCodeList$timeSeriesDesc[timeSeriesCodeList$timeSeriesCode==wrkTimeSeries]))
      )
    })  
  })
  
  
  observe({
    print("##### observe envSummary D")
    #values$gWrkPeriodUpdate()
    #    print('observe get_wrkPeriod()')
    wrkEnvA$values$wrkPeriod
    wrkEnvMain$lnkEnvActive$envName
    
    wrkPeriod<-get_wrkPeriod(wrkEnvD)
    wrkTimeSeries<-get_wrkTimeSeries(wrkEnvD)
    print(wrkPeriod)
    #    if (is.null(wrkPeriod$value)) return
    #    output$summaryWrkPeriod<-renderText({
    #      wrkPeriod
    #    })  
    output$summaryWrkPeriodD<-renderUI({
      tagList(
        tags$p(paste("van tot:",wrkPeriod[1],'/',wrkPeriod[2]))
        ,tags$p(paste("time series: ",wrkTimeSeries,'/',timeSeriesCodeList$timeSeriesDesc[timeSeriesCodeList$timeSeriesCode==wrkTimeSeries]))
      )
    })  
  })
  
  
    #  observe({
  #    values$gWrkSensorsUpdate()
  #    output$summaryWrkSensors<-summary(wrkSensors)
  #  })
  
  #  observe({
  #    values$gWrkDataUpdate()
  #    output$summaryWrkData<-summary(wrkData)
  #  })
  
  
  ##### end of observe section #######  
  
  ##### output section #######  
  

  ##### end of output section #######  

  ##### event section #######  

  ##### end of event section #######  

  ##### function section #######  

  ##### end of function section #######  
  
  return (moduleResults)
}
#########################################################
