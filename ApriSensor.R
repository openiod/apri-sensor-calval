
get_ApriSensoR_data_SOS <- function(input, output, session, sensorsystem, offering, fois, ops, period, timeSeries
                                    , startDate=NULL
                                    , endDate=NULL) {
  # fois <- "apri-sensor-pmsa003_SCWM68C63A80923C,apri-sensor-pmsa003_SCWM68C63A23401D";
  # periode <- ''
  # periode <- "&date_start=2018-02-27T00:00:00+01:00&date_end=2018-04-01T00:00:00+01:00";
  # periode <- "&date_start=3d";

#  validate(
#    need(sensorsystem != "", "Please select a sensorsystem")
#  )
#  validate(
#    need(fois != "", "Please select a feature of interest")
#  )
  startEnd<-NULL
  print(startDate)
  print(endDate)
  if (!is.null(startDate)&!is.null(endDate)) {
    startEnd<-paste('&date_start=',startDate,'&date_end=',endDate,sep='');
    print(startEnd)
  }
  url <- paste("https://openiod.org/SCAPE604/openiod?SERVICE=WPS&REQUEST=Execute&identifier=transform_observation&action=getobservation",
      "&sensorsystem=",sensorsystem,
      "&offering=",offering,
      "&foi=",fois,
      "&op=",ops,
      startEnd,
      "&timeSeries=",timeSeries,
      "&format=csv",
      sep="");
  #    "&date_start=",
  print(url)
  result<-tryCatch(read.csv(url, header = FALSE, sep = ";", quote = "\""), error=function(e) NULL)
  if (is.null(result)) return(NULL)
  if(nrow(result)==0) {
    print('read csv no records found')
    return()
  }
  data <- as.tibble(read.csv(url, header = FALSE, sep = ";", quote = "\""))
  data$date <- as.POSIXct(data$V5, format="%Y-%m-%dT%H:%M")
  data$opValue <- data$V6
  data$opName <- as.character(data$V3)
  data$foiName <- as.character(data$V4)
  plushours <- 2*60*60
  
  data$minute <- sapply(format(data$date, "%M"), as.numeric)
  data$hour <- sapply(format(data$date, "%H"), as.numeric)
  
  #data <- subset(data, (data$sensorValue <100) );
  
  data$date <- data$date - ( data$minute %% 5)*60  # gemiddelde per 5 min
  #data$date <- data$date - data$minute*60  # gemiddelde per 60 min
  #data$date <- data$date - data$hour*60*60 - data$minute*60  # gemiddelde per dag
  #data$date2 <- data$date

  #  dfTresholds <- callModule(get_ApriSensoR_tresholds,"ApriSensorTresholds");
  
  #tmp_data<-merge(data,dfTresholds(),by="sensorType",all.x = FALSE)
  #dfdata <- aggregate(cbind(treshold, sensorValue)~foi+sensorType+date, data=tmp_data, mean, na.rm=TRUE)
  #dfdata <- aggregate(tmp_data~foiName+opName+date, data=tmp_data, mean, na.rm=TRUE)
  
  tmp_data<-data %>%
    group_by(foiName,opName,date) %>%
    summarize(opValue = mean(opValue, na.rm = TRUE))
  tmp_data2<-left_join(tmp_data,opTresholdList,by=c("opName"))
  
#  totalData <- dfdata
#  totalData$tmp2 = as.character(totalData$foi);
  tmp_data2$foiIdImport=substr(tmp_data2$foiName,regexpr('[SCRPL][SCWMU].*$',tmp_data2$foiName),50);

  #result <- callModule(factorSensorType,"ApriSensorType",totalData$sensorType);
  #totalData$sensorType <- result()
  
  period <- range(tmp_data2$date);
  periodetext1 <- strftime(period[1], format = "%Y-%m-%d %H:%M uur" )
  periodetext2 <- strftime(period[2], format = "%Y-%m-%d %H:%M uur")
  
  result <- callModule(labelsSensorType,"ApriSensorTypeLabels");
  labels <- result()
  result <- callModule(breaksAndLabels,"ApriSensorBreaksAndLabels");
  cBreaksAndLabels <- result()
  
  foitext <- paste('Fijnstof en meteo',sep="");
  
  #totalData <- totalData[order(totalData$foiLocation, totalData$sensorType, totalData$date),]
  #totalDatas<- totalData[order(totalData$date),]
  
  tmp_data2$type = c(0, cumsum(diff(tmp_data2$date) >36000))  # tijdsduur in seconden als minumum waarde voor onderbrekingen van grafieklijn
  #totalData$type = c(0, cumsum(diff(totalData$date) >60))  # tijdsduur in seconden als minumum waarde voor onderbrekingen van grafieklijn
  #totalData$type = c(0, cumsum(diff(totalData$date) >300))  # tijdsduur in seconden als minumum waarde voor onderbrekingen van grafieklijn
  
  reactive({data=tmp_data2})
}

factorSensorType <- function(input, output, session, sensorType) {
  t_sensorType = factor(sensorType,
    levels=c(
      'apri-sensor-am2320-rHum',
      'apri-sensor-am2320-temperature',
      'apri-sensor-ds18b20-temperature',
      'apri-sensor-bme280-pressure',
      'apri-sensor-bme280-temperature',
      'apri-sensor-bme280-rHum',
      'apri-sensor-pms7003-concPM1_0_CF1',
      'apri-sensor-pms7003-concPM2_5_CF1',
      'apri-sensor-pms7003-concPM10_0_CF1',
      'apri-sensor-pms7003-concPM1_0_amb',
      'apri-sensor-pms7003-concPM2_5_amb',
      'apri-sensor-pms7003-concPM10_0_amb',
      'apri-sensor-pms7003-rawGt0_3um',
      'apri-sensor-pms7003-rawGt0_5um',
      'apri-sensor-pms7003-rawGt1_0um',
      'apri-sensor-pms7003-rawGt2_5um',
      'apri-sensor-pms7003-rawGt5_0um',
      'apri-sensor-pms7003-rawGt10_0um',
      'apri-sensor-pmsa003-concPM1_0_CF1',
      'apri-sensor-pmsa003-concPM2_5_CF1',
      'apri-sensor-pmsa003-concPM10_0_CF1',
      'apri-sensor-pmsa003-concPM1_0_amb',
      'apri-sensor-pmsa003-concPM2_5_amb',
      'apri-sensor-pmsa003-concPM10_0_amb',
      'apri-sensor-pmsa003-rawGt0_3um',
      'apri-sensor-pmsa003-rawGt0_5um',
      'apri-sensor-pmsa003-rawGt1_0um',
      'apri-sensor-pmsa003-rawGt2_5um',
      'apri-sensor-pmsa003-rawGt5_0um',
      'apri-sensor-pmsa003-rawGt10_0um',
      "apri-sensor-luchtmeetnet-PM25",
      "apri-sensor-luchtmeetnet-PM10"
    ))
  reactive({result=t_sensorType})
}

labelsSensorType <- function(input, output, session) {
  t_labels <- c(
    'apri-sensor-am2320-temperature' = "temp.C",
    'apri-sensor-am2320-rHum' = "AM2320 rHum\n%RH",
    'apri-sensor-ds18b20-temperature' = "DS18B20 temp.\n°C",
    'apri-sensor-bme280-pressure' = "BME280\n Luchtdruk\nhPa",
    'apri-sensor-bme280-temperature' = "BME280\n Temperatuur\nCelc",
    'apri-sensor-bme280-rHum' = "BME280\n rLuchtvochtigheid\npercent.",
    'apri-sensor-pms7003-concPM1_0_amb'="PMS7003\n PM 1 amb",
    'apri-sensor-pms7003-concPM1_0_CF1'="PMS7003\n PM 1",
    'apri-sensor-pms7003-concPM2_5_amb'="PMS7003\n PM 2.5 amb",
    'apri-sensor-pms7003-concPM2_5_CF1'="PMS7003\n PM 2.5",
    'apri-sensor-pms7003-concPM10_0_CF1'="PMS7003\n PM 10",
    'apri-sensor-pms7003-concPM10_0_amb'="PMS7003\n PM 10 amb",
    'apri-sensor-pms7003-rawGt0_3um'="PMS7003\n PM 0.3 part.",
    'apri-sensor-pms7003-rawGt0_5um'="PMS7003\n PM 0.5 part.",
    'apri-sensor-pms7003-rawGt1_0um'="PMS7003\n PM 1.0 part.",
    'apri-sensor-pms7003-rawGt2_5um'="PMS7003\n PM 2.5 part.",
    'apri-sensor-pms7003-rawGt5_0um'="PMS7003\n PM 5.0 part.",
    'apri-sensor-pms7003-rawGt10_0um'="PMS7003\n PM 10.0 part.",
    'apri-sensor-pmsa003-concPM1_0_amb'="PMSA003\n PM 1 amb",
    'apri-sensor-pmsa003-concPM1_0_CF1'="PMSA003\n PM 1",
    'apri-sensor-pmsa003-concPM2_5_amb'="PMSA003\n PM 2.5 amb",
    'apri-sensor-pmsa003-concPM2_5_CF1'="PMSA003\n PM 2.5",
    'apri-sensor-pmsa003-concPM10_0_CF1'="PMSA003\n PM 10",
    'apri-sensor-pmsa003-concPM10_0_amb'="PMSA003\n PM 10 amb",
    'apri-sensor-pmsa003-rawGt0_3um'="PMSA003\n PM 0.3 part.",
    'apri-sensor-pmsa003-rawGt0_5um'="PMSA003\n PM 0.5 part.",
    'apri-sensor-pmsa003-rawGt1_0um'="PMSA003\n PM 1.0 part.",
    'apri-sensor-pmsa003-rawGt2_5um'="PMSA003\n PM 2.5 part.",
    'apri-sensor-pmsa003-rawGt5_0um'="PMSA003\n PM 5.0 part.",
    'apri-sensor-pmsa003-rawGt10_0um'="PMSA003\n PM 10.0 part.",
    "apri-sensor-luchtmeetnet-PM25"="LMN\n PM2.5 ug/m3",
    "apri-sensor-luchtmeetnet-PM10"="LMN\n PM10 ug/m3"
  )
  reactive({result=t_labels})
}

breaksAndLabels <- function(input, output, session) {
  t_BenL <- c(
    'SCRP00000000082fba1b' = "Purmerend (ba1b)",
    'SCRP00000000082fba1b*RF7*1' = "Purmerend (ba1b) unit RF7/1",
    'SCRP000000004123e145' = "Purmerend (E145)",
    'SCRP000000004659c5bc' = "Zutphen (c5bc)",
    'SCWMA020A62C8201' = "Zutphen L-Oost(8201)",
    'SCWMA020A62DC1CB' = "WemosD1 (C1CB pm+meteo)",
    'SCWM68C63A808F33' = "WemosD1 (8F33 pm)",
    'SCWM68C63A80923C' = "Apeldoorn straat (923C)",
    'LUCHTMEETNETNL01496' = "LML/BAM HvH/Berghaven (1496)",
    'grenswaarde' = "Grenswaarde indicatief"
  )
  reactive({result=t_BenL})
}

