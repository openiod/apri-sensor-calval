
envPlotUI <- function(id) {
  ns <- NS(id)
  
  
#  conditionalPanel("get_wrkDataChanged(wrkEnvA) > 0",
#                   plotOutput(ns("plotWrkDataAPlot"))
#                   ,ns #, NS(id)               
#  )
  
  tagList(  

      br()
    , verbatimTextOutput(ns("uiEnvPlotResultMessage"))
  
      , div(
          uiOutput(ns("showPlotPanelMain"))
        , conditionalPanel("output.showPlotPanelMain == 'Main'",
                          { 
                            plotOutput(ns("plotWrkDataMainPlot"))
                          }  
          ,ns=ns                
        )
        , uiOutput(ns("showPlotPanelA"))
        , conditionalPanel("output.showPlotPanelA == 'A'",
                           { 
                             plotOutput(ns("plotWrkDataAPlot"))
                           }  
                           ,ns=ns                
        )
        , uiOutput(ns("showPlotPanelB"))
        , conditionalPanel("output.showPlotPanelB == 'B'",
                           { 
                             plotOutput(ns("plotWrkDataBPlot"))
                           }  
                           ,ns=ns                
        )
        , uiOutput(ns("showPlotPanelC"))
        , conditionalPanel("output.showPlotPanelC == 'C'",
                           { 
                             plotOutput(ns("plotWrkDataCPlot"))
                           }  
                           ,ns=ns                
        )
        , uiOutput(ns("showPlotPanelD"))
        , conditionalPanel("output.showPlotPanelD == 'D'",
                           { 
                             plotOutput(ns("plotWrkDataDPlot"))
                           }  
                           ,ns=ns                
        )
    )

  )
}

envPlot <- function(input, output, session) {
  ns<-session$ns 
  print('envPlot start')
  
  # list of reactive values to return to calling module
  moduleResults <- reactiveValues()
  ## list to store reactive values
  values <- reactiveValues(
  )
  

  ##### get data reactive functions ########
  
  ##### end of get data reactive functions ########
  
  
  ##### observe section #######  

  observe({
    if(is.null(get_wrkDataChanged(wrkEnvMain))) return()
    print('?????????????????????????????????????????????????????????????????')
    print(get_wrkDataChanged(wrkEnvMain))
    output$showPlotPanelMain <- renderText('Main')
    t<-get_wrkData(wrkEnvMain)
    print('plot data for environment Main')
    print(t)
    output$plotWrkDataMainPlot<-renderPlot({
      #plot(t$date, t$opValue)
      #p<-ggplot(data=plotData,mapping = aes(x = date, y = sensorValue2))+geom_point() #,aes(x=date, y=sensorValue2))
      p<-ggplot(data=t, map=aes(x=date, y=opValue)) + #geom_point()
        geom_point(shape=1) #+     # Use hollow circles
      #  geom_smooth(method=lm,   # Add linear regression line
      #              se=FALSE)    # Don't add shaded confidence region
      # )  
      p
      #plot(mtcars$wt, mtcars$mpg)
    })
  })
  observe({
    if(is.null(get_wrkDataChanged(wrkEnvA))) return()
    if(get_wrkDataChanged(wrkEnvA)==0) return()
    print('?????????????????????????????????????????????????????????????????')
    print(get_wrkDataChanged(wrkEnvA))
    output$showPlotPanelA <- renderText('A')
    t<-get_wrkData(wrkEnvA)
    print('plot data for environment A')
    print(t)
    output$plotWrkDataAPlot<-renderPlot({
      #plot(t$date, t$opValue)
      #p<-ggplot(data=plotData,mapping = aes(x = date, y = sensorValue2))+geom_point() #,aes(x=date, y=sensorValue2))
      p<-ggplot(data=t, map=aes(x=date, y=opValue)) + #geom_point()
        geom_point(shape=1) #+     # Use hollow circles
      #  geom_smooth(method=lm,   # Add linear regression line
      #              se=FALSE)    # Don't add shaded confidence region
      # )  
      p
      #plot(mtcars$wt, mtcars$mpg)
    })
  })
  observe({
    if(is.null(get_wrkDataChanged(wrkEnvB))) return()
    if(get_wrkDataChanged(wrkEnvB)==0) return()
    print('?????????????????????????????????????????????????????????????????')
    print(get_wrkDataChanged(wrkEnvB))
    output$showPlotPanelB <- renderText('B')
    t<-get_wrkData(wrkEnvB)
    print('plot data for environment B')
    print(t)
    output$plotWrkDataBPlot<-renderPlot({
      #plot(t$date, t$opValue)
      #p<-ggplot(data=plotData,mapping = aes(x = date, y = sensorValue2))+geom_point() #,aes(x=date, y=sensorValue2))
      p<-ggplot(data=t, map=aes(x=date, y=opValue)) + #geom_point()
        geom_point(shape=1) #+     # Use hollow circles
      #  geom_smooth(method=lm,   # Add linear regression line
      #              se=FALSE)    # Don't add shaded confidence region
      # )  
      p
      #plot(mtcars$wt, mtcars$mpg)
    })
  })
  observe({
    if(is.null(get_wrkDataChanged(wrkEnvC))) return()
    if(get_wrkDataChanged(wrkEnvC)==0) return()
    print('?????????????????????????????????????????????????????????????????')
    print(get_wrkDataChanged(wrkEnvC))
    output$showPlotPanelC <- renderText('C')
    t<-get_wrkData(wrkEnvC)
    print('plot data for environment C')
    print(t)
    output$plotWrkDataCPlot<-renderPlot({
      #plot(t$date, t$opValue)
      #p<-ggplot(data=plotData,mapping = aes(x = date, y = sensorValue2))+geom_point() #,aes(x=date, y=sensorValue2))
      p<-ggplot(data=t, map=aes(x=date, y=opValue)) + #geom_point()
        geom_point(shape=1) #+     # Use hollow circles
      #  geom_smooth(method=lm,   # Add linear regression line
      #              se=FALSE)    # Don't add shaded confidence region
      # )  
      p
      #plot(mtcars$wt, mtcars$mpg)
    })
  })
  observe({
    if(is.null(get_wrkDataChanged(wrkEnvD))) return()
    if(get_wrkDataChanged(wrkEnvD)==0) return()
    print('?????????????????????????????????????????????????????????????????')
    print(get_wrkDataChanged(wrkEnvD))
    output$showPlotPanelD <- renderText('D')
    t<-get_wrkData(wrkEnvD)
    print('plot data for environment D')
    print(t)
    output$plotWrkDataDPlot<-renderPlot({
      #plot(t$date, t$opValue)
      #p<-ggplot(data=plotData,mapping = aes(x = date, y = sensorValue2))+geom_point() #,aes(x=date, y=sensorValue2))
      p<-ggplot(data=t, map=aes(x=date, y=opValue)) + #geom_point()
        geom_point(shape=1) #+     # Use hollow circles
      #  geom_smooth(method=lm,   # Add linear regression line
      #              se=FALSE)    # Don't add shaded confidence region
      # )  
      p
      #plot(mtcars$wt, mtcars$mpg)
    })
  })
  

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
