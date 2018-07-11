
# Define server logic to summarize and view selected project ----
function(input, output,session) {
  
  output$titlePanelActiveEnv <- renderText({
    print('$$$$$$titlePanelActiveEnv')
    wrkEnvMain$lnkEnvActive$values$active
#    print(environmentList)
#    print(setNames(environmentList[2],environmentList[1]))
    print(get_active_wrkEnv_envName())
#    t<-filter(environmentList, envId==get_active_wrkEnv_envName())
#    print(t)
    paste('Actuele omgeving:',get_active_wrkEnv_envName())
  #  c('wrkEnvMain','wrkEnvA','wrkEnvB','wrkEnvC','wrkEnvD'),c('Algemeen','1-A','1-B','1-C','1-D')
  })
    
  sensorPlot1<-function(plotData){
    print('sensorPlot1')
    print(plotData)
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Making plot", value = 0)
    
#p<-ggplot(data=plotData,mapping = aes(x = date, y = sensorValue2))+geom_point() #,aes(x=date, y=sensorValue2))
    p<-ggplot(data=plotData, mapping = aes(x=sensorValue1, y=sensorValue2)) + #geom_point()
      geom_point(shape=1) +     # Use hollow circles
      geom_smooth(method=lm,   # Add linear regression line
                  se=FALSE)    # Don't add shaded confidence region
 # )  
    # Increment the progress bar, and update the detail text.
    progress$inc(1/2, detail = paste("Doing part"))
    
    lm_eqn <- function(df){
      m <- lm(y ~ x, df);
      eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                       list(a = format(coef(m)[1], digits = 2), 
                            b = format(coef(m)[2], digits = 2), 
                            r2 = format(summary(m)$r.squared, digits = 3)))
      as.character(as.expression(eq));                 
    }
#    lm_eqn(plotData)
    p <- p + geom_text(x = 15, y = 15, label = lm_eqn(plotData), parse = TRUE)
#    print(p1)
#    p
  renderPlot({p})
  }
  
  # builds a reactive expression that only invalidates 
  # when the value of input$goButton becomes out of date 
  # (i.e., when the button is pressed)
  output$processButtonText <- eventReactive(input$processButton, {
    print('output processButtonText')

#    tmpFois <- wrkProjectFois()
#    print(tmpFois)
#    output$wrkSensorTable <- DT::renderDataTable({
#      DT::datatable(tmpFois)
#    })

#    tmpSensorData1 <-getSensorDataSos("apri-sensor-pmsa003_SCWM68C63A809492")() # (fois)
#    print(tmpSensorData1)
#    tmpSensorData2 <-getSensorDataSos("apri-sensor-pmsa003_SCWM68C63A808F33")() # (fois)
#    print(tmpSensorData2)
#    tmpSensorData <- data.frame( date=tmpSensorData1$date,sensorValue1=tmpSensorData1$sensorValue, sensorValue2=tmpSensorData2$sensorValue,x=tmpSensorData1$sensorValue, y=tmpSensorData2$sensorValue)
#    print(tmpSensorData)
    
#    output$wrkSensorDataTable <- DT::renderDataTable({
#      DT::datatable(tmpSensorData)
#    })
#    output$sensorPlot1 <-sensorPlot1(tmpSensorData)
#    paste('Verwerkknop is gedrukt. Periode van ', input$inDateRange[1],' tot ',input$inDateRange[2], sep='')
    'testje'
  })
  
## panels section
  reactive({
    print('panels')
    #output$mytable1 <- DT::renderDataTable({
    #  DT::datatable(tmpFois)
    #})
  })
  
#  print(idList)
#  print(
#  list.filter(idList, name=="serverSensorUI_ssp")
#  print(idList[name=="serverSensorUI_ssp"])
#  print(idList[name=="serverSensorUI_ssp"]$id)
  periodSelectionResult <- callModule(periodSelect,idList["name"=="periodSensorUI_ssp"]$id);
  print(periodSelectionResult)
  sensorSelectionResult <- callModule(sensorSelect,idList["name"=="serverSensorUI_ssp"]$id);
  print(sensorSelectionResult)
  envSummaryResult <- callModule(envSummary,idList["name"=="envSummaryUI_ssp"]$id);
  print(envSummaryResult)
  
  observe({
    print("####################################### sensorSelectionResult")
    #print(sensorSelectionResult)
    print(reactiveValuesToList(sensorSelectionResult, all.names=TRUE) )
    validate(need(!is.reactivevalues(sensorSelectionResult),"Dataframe not found")) # changed as well
    #      sensorSelectionResult$toList())
    #sensorSelectionResult
    #values$sensorSelectionResult <- sensorSelectionResult$sensorSelection()
    #    t2<-sensorSelectionResult()
  })
  
  output$sensorMap <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      #fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
      fitBounds(4.0, 50.0, 4.3, 53.0)
    #      addProviderTiles(providers$Stamen.TonerLite,
#                       options = providerTileOptions(noWrap = TRUE)
#      ) # %>%
      #addMarkers(data = points())
  })
  
  ## client messaging about timezone
  timeZoneInfo<-NULL #global var for timeZone info $serverPosix,$serverTimeZone,$clientPosix,$clientTimeZone,
  triggerClientTime <- function(session=shiny::getDefaultReactiveDomain()){
    serverTime <- Sys.time()
    serverTimeZone <- as.integer(strftime(serverTime,"%z"))/100
    session$sendCustomMessage(
      type="getClientTime",
      message=list(
        serverPosix = as.numeric(serverTime),
        serverTimeZone = serverTimeZone
      )
    )
  }
  
  # Observe and print time (zone) from client and server
  observe({ 
    #print(input$clientTime)
    timeZoneInfo<<-input$clientTime
    #print(timeZoneInfo$serverPosix)
    #print(timeZoneInfo$serverTimeZone)
    #print(timeZoneInfo$clientPosix)
    #print(timeZoneInfo$clientTimeZone)
    tzDiff<-timeZoneInfo$serverPosix-timeZoneInfo$clientPosix
    lag<- timeZoneInfo$serverTimeZone-timeZoneInfo$clientTimeZone
    print(paste('clientlag: ',tzDiff))
    print(paste('timzone diff server vs client: ',lag))
  })
  # Ask the client for current time and time zone (hours from UTC)
  triggerClientTime()
  
  print('end of server')
  "end of server"
  
}
