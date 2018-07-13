

sensorGetData <- function(input, output, session, selectedSensor, selectedPeriod, selectedTimeSeries) {
  ns<-session$ns 
  print('sensorGetData start')
  
  print('input selected sensor:')
  print(selectedSensor)
  print('input selected period:')
  print(selectedPeriod)
  
  # list of reactive values to return to calling module
  moduleResults <- reactiveValues()
  # list to store reactive values
  values <- reactiveValues()
  
  
  getSensorDataSos<-function(sensorSystem,fois,ops,timeSeries) {
    data <- callModule(get_ApriSensoR_data_SOS,"ApriSensorData"
                       , sensorSystem
                       , fois 
                       , ops
                       , timeSeries
                       , period="");
    
  }  
  
  moduleResults$result <- getSensorDataSos(selectedSensor$opIdPrefix
                                ,paste(selectedSensor$opIdPrefix,selectedSensor$foiIdSep,selectedSensor$foiId,sep='')
                                ,paste(selectedSensor$opIdPrefix,selectedSensor$opIdSep,selectedSensor$opId,sep='')
                                ,selectedTimeSeries
                                )
  observe({
    print('get data module returned results')
    print(summary(moduleResults$result()))
#    t<-values$result()
#    moduleResults$result <<- t
#    moduleResults$test<-'test'
 #   print(summary(moduleResults$result()))
  }) 
    
  #}
  
  # tmpSensorData1 <-getSensorDataSos("apri-sensor-pmsa003_SCWM68C63A809492")() # (fois)
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
  

  return(moduleResults) 
  
}  