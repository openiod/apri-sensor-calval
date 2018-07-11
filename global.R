
#install.packages("tidyverse", dependencies = TRUE)
library(stats) # vóór tidyverse
library(tidyverse)

#install.packages("shinydashboard")
library(shiny)
library(shinyjs)
library(shinydashboard)

library(httr)
library(jsonlite)
library(magrittr)
library(ggplot2)
#install.packages("DT")
library(DT)
#library(reshape2)
library(scales)
#install.packages("rlist")
library(rlist)
#library(plyr)
library(pryr)
#library(modules)
#install.packages("future")
library(future)
plan(multiprocess) # future plan for multicore (forked R processes (on current machine)), if supported, otherwise multisession: all	background R sessions (on current machine)
library(leaflet)
source("config.R")
source("ApriSensor.R")
source("as_sensorSelect.R")
source("as_periodSelect.R")
source("as_envSummary.R")
source("as_sensorGetData.R")

# https://shiny.rstudio.com/reference/shiny/1.0.1/shiny-options.html
options(shiny.trace=FALSE)
options(shiny.fullstacktrace=TRUE)
options(shiny.testmode=TRUE)
#showMethods()

mycss <- "
#sensorPlot1 {
position: relative;
z-index:2
}
#loading-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: 1;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
z-index: 0;
}
"

#projects <- list("ApriSensor Dylos"="asDylos","ApriSensor Zutphen"="asZutphen","ApriSensor Apeldoorn"="asApeldoorn","ApriSensor D&S (DCMR, invoorvereiding)"="asDS")

######## create environments for package data and functions
##### work objects

# wrkEnv : Work environment for 'global' package level storage of:

# emptyenv()-> wrkEnvDefault -> wrkEnvMain -> wrkEnvA / wrkEnvB / wrkEnvC / wrkEnvD
# objects per environment or NA for parent level 
#   values : as reactiveValues
#     wrkPeriod: start- and enddate 
#     wrkTimeSeries: timeSeries indicator/code
#     wrkSensors: sensor meta data
#     wrkData: sensor data (messaurements)

print('create default environment')

wrkEnvDefault <- new.env(parent = emptyenv())
wrkEnvDefault$values<-reactiveValues(wrkPeriod=c(Sys.Date(),Sys.Date(),wrkTimeSeries='H'))
wrkEnvMain <- new.env(parent = wrkEnvDefault)
wrkEnvA <- new.env(parent = wrkEnvMain)
wrkEnvB <- new.env(parent = wrkEnvMain)
wrkEnvC <- new.env(parent = wrkEnvMain)
wrkEnvD <- new.env(parent = wrkEnvMain)

wrkEnvMain$values<-reactiveValues()
wrkEnvA$values<-reactiveValues()
wrkEnvB$values<-reactiveValues()
wrkEnvC$values<-reactiveValues()
wrkEnvD$values<-reactiveValues()

wrkEnvDefault$envName<-'default env'
wrkEnvMain$envName<-'main env'
wrkEnvA$envName<-'A env'
wrkEnvB$envName<-'B env'
wrkEnvC$envName<-'C env'
wrkEnvD$envName<-'D env'

get_active_wrkEnv_envName<-function(){
  print('get active wrkEnv envName')
#  print(wrkEnvMain$lnkEnvActive)
#  print(wrkEnvMain$lnkEnvActive$envName)
#  print(lnkEnvActive$envName)
#  print(wrkEnvMain$envName)
#  print(ls(wrkEnvMain))
#  print(ls(lnkEnvActive))
  wrkEnvMain$lnkEnvActive$envName
}
set_activeEnvironment<- function(newEnv){
  print('set active environment')
  #print(newEnv)
  tmpEnv<-newEnv
  if (!is.environment(tmpEnv)) {
    choices<-list(wrkEnvDefault=wrkEnvDefault,wrkEnvMain=wrkEnvMain,wrkEnvA=wrkEnvA,wrkEnvB=wrkEnvB,wrkEnvC=wrkEnvC,wrkEnvD=wrkEnvD)
    tmpEnv<-choices[[newEnv]]
    #print(choices)
  }
  if (!is.environment(tmpEnv)) {
    print('set active environment error, param is not an existing environment')
    print(newEnv)
    return
  }  
  #print(where("lnkEnvActive"))
  old<-wrkEnvMain$lnkEnvActive
  wrkEnvMain$lnkEnvActive<<-tmpEnv
  lnkEnvActive<<-tmpEnv
  old$values$active<-FALSE
  wrkEnvMain$lnkEnvActive$values$active<-TRUE
  invisible(old)
}
get_wrkPeriod <- function(envir=NULL) {
  print('get_wrkPeriod')
  if (is.environment(envir)) {
    envir$values$wrkPeriod
  } else wrkEnvMain$lnkEnvActive$values$wrkPeriod
#  print(environment())
#  print(ls(environment()))
#  print(environmentName(wrkEnvMain))
#  print(environmentName(wrkEnvMain$lnkEnvActive))
}
set_wrkPeriod <- function(value) {
  print(paste("set wrkPeriod",wrkEnvMain$lnkEnvActive$envName))
  print(value)
  old <- wrkEnvMain$lnkEnvActive$values$wrkPeriod
  wrkEnvMain$lnkEnvActive$values$wrkPeriod <<- value
  invisible(old)
}
get_wrkTimeSeries <- function(envir=NULL) {
  print('get_wrkTimeSeries')
  if (is.environment(envir)) {
    envir$values$wrkTimeSeries
  } else wrkEnvMain$lnkEnvActive$values$wrkTimeSeries
}
set_wrkTimeSeries <- function(value) {
  old <- wrkEnvMain$lnkEnvActive$values$wrkTimeSeries
  wrkEnvMain$lnkEnvActive$values$wrkTimeSeries <- value
  invisible(old)
}
get_wrkSensors <- function() {
  wrkEnvMain$lnkEnvActive$wrkSensors
}
set_wrkSensors <- function(value) {
  old <- wrkEnvMain$lnkEnvActive$wrkSensors
  wrkEnvMain$lnkEnvActive$wrkSensors <- value
  invisible(old)
}
get_wrkData <- function() {
  wrkEnvMain$lnkEnvActive$wrkData
}
set_wrkData <- function(value) {
  old <- wrkEnvMain$lnkEnvActive$wrkData
  wrkEnvMain$lnkEnvActive$wrkData <- value
  invisible(old)
}

wrkEnvMain$lnkEnvActive<-wrkEnvMain
wrkEnvMain$values<-reactiveValues(wrkPeriod=NULL,wrkTimeSeries=NULL)
set_activeEnvironment(wrkEnvMain)


wrkEnvDefault$x <-1
wrkEnvMain$x <-2
wrkEnvA$x <-3
print(ls(wrkEnvDefault))
print(environment(wrkEnvDefault)) # NULL
print(environment(wrkEnvMain)) # NULL
print(parent.env(wrkEnvDefault)) # empty env
print(parent.env(wrkEnvMain))
print(parent.env(wrkEnvA))
print(parent.env(wrkEnvB))
print(parent.env(wrkEnvC))
print(parent.env(wrkEnvD))
print(environment(parent.env(wrkEnvA)))
print(parent.frame())
print(get("x", envir = wrkEnvDefault))
print('x found in default env')
print(get("x", envir = wrkEnvMain))
print('x found in main')
print(get("x", envir = wrkEnvA))
print('x found in A')
print(get("envName", envir = lnkEnvActive))
print('x found in A')
print(get("envName", envir = lnkEnvActive))
print('x found in lnkEnvActive')
print(search())
print(environment())




print('test_config.R')
print(environment(wrkEnvMain))
print(environment(wrkEnvMain$lnkEnvActive))



######## create environment for package data and functions





projectList<-tribble(
  ~projectId, ~projectName, ~projectAlias,  ~projectDesc
  , 'visibilis','visibilis','Pientka & Scapeler','Sensorkast op DCMR luchtmeetnet meetstation Hoek van Holland / Berghaven'
  , '...','...','...','...'
)
projectFoiList<-tribble(
  ~projectId, ~foiId
  , 'visibilis','SCWM68C63A809492'
  , 'visibilis','SCWM68C63A809290'
  , 'visibilis','SCWM68C63A808F33'
  , 'visibilis','LUCHTMEETNETNL01496'
)
foiList <-tribble( # feature of interest
  ~foiId, ~foiName, ~foiIdShort
  , 'SCWM68C63A809492','HvH PMSA003-1','9492'
  , 'SCWM68C63A809290','HvH PMSA003-2','9290'
  , 'SCWM68C63A808F33','HvH PMSA003-3','8F33'
  , 'LUCHTMEETNETNL01496','DCMR HvH/Berghaven','1496'
)
foiOpList<-tribble( # observable properties related to feature of interest
  ~foiId, ~opId, ~opIdPrefix, ~foiIdSep, ~opIdSep, ~opAlias, ~opUnit
  , 'SCWM68C63A809290','concPM1_0_CF1','apri-sensor-pmsa003','_','-', 'PM1','µg/m3'
  , 'SCWM68C63A809290','concPM2_5_CF1','apri-sensor-pmsa003','_','-', 'PM2.5','µg/m3'
  , 'SCWM68C63A809290','concPM10_0_CF1','apri-sensor-pmsa003','_','-', 'PM10','µg/m3'
  , 'SCWM68C63A809290','rawGt0_3um','apri-sensor-pmsa003','_','-', 'raw<0.3µm','part.'
  , 'SCWM68C63A809290','rawGt0_5um','apri-sensor-pmsa003','_','-', 'raw<0.5µm','part.'
  , 'SCWM68C63A809290','rawGt1_0um','apri-sensor-pmsa003','_','-', 'raw<1µm','part.'
  , 'SCWM68C63A809290','rawGt2_5um','apri-sensor-pmsa003','_','-', 'raw<2.5µm','part.'
  , 'SCWM68C63A809290','rawGt5_0um','apri-sensor-pmsa003','_','-', 'raw<5µm','part.'
  , 'SCWM68C63A809290','rawGt10_0um','apri-sensor-pmsa003','_','-', 'raw<10µm','part.'
  , 'SCWM68C63A809492','concPM1_0_CF1','apri-sensor-pmsa003','_','-', 'PM1','µg/m3'
  , 'SCWM68C63A809492','concPM2_5_CF1','apri-sensor-pmsa003','_','-', 'PM2.5','µg/m3'
  , 'SCWM68C63A809492','concPM10_0_CF1','apri-sensor-pmsa003','_','-', 'PM10','µg/m3'
  , 'SCWM68C63A809492','rawGt0_3um','apri-sensor-pmsa003','_','-', 'raw<0.3µm','part.'
  , 'SCWM68C63A809492','rawGt0_5um','apri-sensor-pmsa003','_','-', 'raw<0.5µm','part.'
  , 'SCWM68C63A809492','rawGt1_0um','apri-sensor-pmsa003','_','-', 'raw<1µm','part.'
  , 'SCWM68C63A809492','rawGt2_5um','apri-sensor-pmsa003','_','-', 'raw<2.5µm','part.'
  , 'SCWM68C63A809492','rawGt5_0um','apri-sensor-pmsa003','_','-', 'raw<5µm','part.'
  , 'SCWM68C63A809492','rawGt10_0um','apri-sensor-pmsa003','_','-', 'raw<10µm','part.'
  , 'SCWM68C63A808F33','concPM1_0_CF1','apri-sensor-pmsa003','_','-', 'PM1','µg/m3'
  , 'SCWM68C63A808F33','concPM2_5_CF1','apri-sensor-pmsa003','_','-', 'PM2.5','µg/m3'
  , 'SCWM68C63A808F33','concPM10_0_CF1','apri-sensor-pmsa003','_','-', 'PM10','µg/m3'
  , 'SCWM68C63A808F33','rawGt0_3um','apri-sensor-pmsa003','_','-', 'raw<0.3µm','part.'
  , 'SCWM68C63A808F33','rawGt0_5um','apri-sensor-pmsa003','_','-', 'raw<0.5µm','part.'
  , 'SCWM68C63A808F33','rawGt1_0um','apri-sensor-pmsa003','_','-', 'raw<1µm','part.'
  , 'SCWM68C63A808F33','rawGt2_5um','apri-sensor-pmsa003','_','-', 'raw<2.5µm','part.'
  , 'SCWM68C63A808F33','rawGt5_0um','apri-sensor-pmsa003','_','-', 'raw<5µm','part.'
  , 'SCWM68C63A808F33','rawGt10_0um','apri-sensor-pmsa003','_','-', 'raw<10µm','part.'
  , 'LUCHTMEETNETNL01496','PM25','apri-sensor-luchtmeetnet','_','-', 'PM2.5','µg/m3'
  , 'LUCHTMEETNETNL01496','PM10','apri-sensor-luchtmeetnet','_','-', 'PM10','µg/m3'
)
opTresholdList <- tribble(
  ~opIdPrefix, ~opIdSep, ~opId, ~opTreshold,
  "apri-sensor-pmsa003","-","rawGt0_3um",1000,
  "apri-sensor-pmsa003","-","rawGt0_5um",200,
  "apri-sensor-pmsa003","-","rawGt1_0um",50,
  "apri-sensor-pmsa003","-","rawGt2_5um",5,
  "apri-sensor-pmsa003","-","rawGt5_0um",5,
  "apri-sensor-pmsa003","-","rawGt10_0um",15,
  "apri-sensor-pmsa003","-","concPM1_0_CF1",15,
  "apri-sensor-pmsa003","-","concPM2_5_CF1",15,
  "apri-sensor-pmsa003","-","concPM10_0_CF1",15,
  "apri-sensor-pmsa003","-","concPM1_0_amb",15,
  "apri-sensor-pmsa003","-","concPM2_5_amb",15,
  "apri-sensor-pmsa003","-","concPM10_0_amb",15,
  "apri-sensor-luchtmeetnet","-","PM25",15,
  "apri-sensor-luchtmeetnet","-","PM10",15
)
opTresholdList$opName <- paste(opTresholdList$opIdPrefix
                               ,opTresholdList$opIdSep
                               ,opTresholdList$opId
                               ,sep="")
# tibble to hold selection periods
globalPeriodSelection<-tibble(
  periodStartDate=date()
  ,periodEndDate=date()
  ,timeSeriesCode=character()
  ,timeSeriesDesc=character()
)
# tibble to hold selection sensors
globalSensorSelection<-tibble(
  foiId=character()
  ,opId=character()
  ,opIdPrefix=character()
  ,foiIdSep=character()
  ,opIdSep=character()
  ,opAlias=character()
  ,opUnit=character()
  ,projectId=character()
  ,foiName=character()
  ,foiIdShort=character()
)
timeSeriesCodeList<-tribble(
  ~timeSeriesCode, ~timeSeriesDesc
  , 'H','Per uur'
  , '5m','Per 5 minuten'
  , 'D','Per dag'
  , '...','Geen time series'
)


wrkProjectList<-NULL
wrkProject <- NULL
wrkProjectFoiList <- NULL

# names(projects[1:5])<-c("name","id_short","id","op_name","op_id")
# project_list <-unique( projects[1] )
# projectFois_list <-unique(projects[3])
# project_list2<-levels( factor( projects[1] ) )

#, "LUFTDATENNL12326"
#, "SCRP00000000082fba1b_SDS011"
#, "SCRP00000000082fba1b_DS18B20"
#, "SCRP00000000082fba1b"
#, "LUCHTMEETNETNL49017"

#ProjectPientka <- dataframe("name"="Pientka & Scapeler")
#ProjectPientka$fois<-list("name"="PMSA003_83FF",id="83FF",ops=c(
#  "PM10.0","apri-sensor-pmsa003-concPM10_0_CF1"
#  ,"PM2.5","apri-sensor-pmsa003-concPM2_5_CF1"
#  )
#)
#observableProperty <- list(name="PM2.5",id="apri-sensor-pmsa003-concPM2_5_CF1")
##ProjectPientka$fois&observableproperties<-observableProperty # rbind(observableProperty)

#print(projects)

#observableProperty <- list(name="PM2.5",id="apri-sensor-pmsa003-concPM2_5_CF1")
#ProjectPientka$fois&ops<-rbind(observableProperty)



meetstations <- list(
  "asDylos"=c("part25"="part25","part10"="part10")
, "asZutphen"=c("Zutphen LW"="ZutphenLW","Zutphen LO1"="ZutphenLO1","Zutphen LO2"="ZutphenLO2","Eefden"="Eefden")
, "asApeldoorn"=c("Apeldoorn str"="Apeldoorn str","Apeldoorn achter"="ApeldoornAchter")
, "asDS"=c("temperature"="apri-sensor-ds18b20-temperature")
)
sensors <- list(
  "..."=c("..."="...")
, "ZutphenLW"=c("apri-sensor-pmsa003-concPM2_5_CF1"="pm25","apri-sensor-pmsa003-concPM10_0_CF1"="pm10")
, "apri-sensor-pmsa003-concPM2_5_CF1"=c("pm25"="pm25")
, "apri-sensor-pmsa003-concPM10_0_CF1"=c("pm10"="pm10")
, "apri-sensor-pmsa003-concPM10_0_CF1"=c("pm10"="pm10")
, "part25"=c("een"="een")
, "part10"=c("een"="een")
, "apri-sensor-ds18b20-temperature"=c("een"="een")
)

