
envPlotUI <- function(id) {
  ns <- NS(id)
  
  
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
                             tagList(
                             # Input: Selector for choosing type of graph/plot
                              selectInput(inputId = ns("plotTypeA"),
                                         label = "Plot type:",
                                         choices=c('Standard','Regression')
                                        )
                             ,
                             plotOutput(ns("plotWrkDataAPlot"))
                             ,
                             verbatimTextOutput(ns("plotWrkDataASummary"))
                             )
                           }  
                           ,ns=ns                
        )
        , uiOutput(ns("showPlotPanelB"))
        , conditionalPanel("output.showPlotPanelB == 'B'",
                           { 
                             tagList(
                               # Input: Selector for choosing type of graph/plot
                               selectInput(inputId = ns("plotTypeB"),
                                           label = "Plot type:",
                                           choices=c('Standard','Regression')
                               )
                               ,
                               plotOutput(ns("plotWrkDataBPlot"))
                             )
                           }  
                           ,ns=ns                
        )
        , uiOutput(ns("showPlotPanelC"))
        , conditionalPanel("output.showPlotPanelC == 'C'",
                           { 
                             tagList(
                               # Input: Selector for choosing type of graph/plot
                               selectInput(inputId = ns("plotTypeC"),
                                           label = "Plot type:",
                                           choices=c('Standard','Regression')
                               )
                               ,
                               plotOutput(ns("plotWrkDataCPlot"))
                             )
                           }  
                           ,ns=ns                
        )
        , uiOutput(ns("showPlotPanelD"))
        , conditionalPanel("output.showPlotPanelD == 'D'",
                           { 
                             tagList(
                               # Input: Selector for choosing type of graph/plot
                               selectInput(inputId = ns("plotTypeD"),
                                           label = "Plot type:",
                                           choices=c('Standard','Regression')
                               )
                               , plotOutput(ns("plotWrkDataDPlot"))
                             )
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
    print('observe wrkDataChanged')
#    print(get_wrkDataChanged(wrkEnvA))
    output$showPlotPanelA <- renderText('A')
    wrkEnvA$plotDataStandard<-get_wrkData(wrkEnvA)

    isolate({
      plotType<-input$plotTypeA
      s<-get_wrkSensors(wrkEnvA)
    })
    
    if (nrow(data.frame(s))>1) { #} & plotTyp=='Regression') {
      t<-wrkEnvA$plotDataStandard
      t1 <- t %>% filter(foiIdImport==s$foiId[1])
      t2 <- t %>% filter(foiIdImport==s$foiId[2])
      #total<-full_join(t1,t2)
      total<-bind_cols(t1,t2)
      total$x<-total$opValue
      total$y<-total$opValue1
      print(total)
      wrkEnvA$plotDataRegression<-total
    }  
    output$plotWrkDataAPlot<-NULL
    #output$plotWrkDataAPlot<-renderPlot({plotDataStandard(envir=wrkEnvA)})
  })
  
  observe({
    wrkEnvA$values$plotType<-input$plotTypeA
    r<-createPlot(envir=wrkEnvA)
    output$plotWrkDataAPlot<-renderPlot({r$p})
    output$plotWrkDataASummary<-renderPrint({
      print(wrkEnvA$values$dataSummary$call)
      print(wrkEnvA$values$dataSummary$r.squared)
      print(wrkEnvA$values$dataSummary$residuals)
      print(wrkEnvA$values$dataSummary[1-4])
      print(wrkEnvA$values$dataSummary)
    }
    )  
  })
  observe({
    wrkEnvB$values$plotType<-input$plotTypeB
    r<-createPlot(envir=wrkEnvB)
    output$plotWrkDataBPlot<-renderPlot({r$p})
  })
  observe({
    wrkEnvC$values$plotType<-input$plotTypeC
    r<-createPlot(envir=wrkEnvC)
    output$plotWrkDataCPlot<-renderPlot({r$p})
  })
  observe({
    wrkEnvD$values$plotType<-input$plotTypeD
    r<-createPlot(envir=wrkEnvD)
    output$plotWrkDataDPlot<-renderPlot({r$p})
  })
  
  createPlot<- function(envir=NULL) {
    s<-get_wrkSensors(envir=envir)
    print(s)
    print(envir$values$plotType)
    if (is.null(s)) return(NULL)
    if (is.null(envir$values$plotType)) return(NULL)
    
    isolate({
    s<-get_wrkSensors(envir=envir)
    if (nrow(data.frame(s))>1 & envir$values$plotType=='Regression') {
      r<-plotDataRegression(envir=envir)
    } else {
      r<-plotDataStandard(envir=envir)
    }
    })
  }
  
  plotDataRegression <- function (envir=NULL) {
    results<-NULL
    data<-envir$plotDataRegression
    p<-ggplot(data=data, aes(x=opValue, y=opValue1)) +
      geom_point(shape=1) +    # Use hollow circles
      geom_smooth(method=lm,   # Add linear regression line
                  se=FALSE)    # Don't add shaded confidence region
    
    lm_eqn <- function(df){
      m <- lm(y ~ x, df);
      envir$values$dataSummary<-summary(m)
      #eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
      eq <- substitute(y == a + b %.% x*","~~r^2~"="~r2, 
                       list(a = format(coef(m)[1], digits = 2), 
                            b = format(coef(m)[2], digits = 2), 
                            r2 = format(summary(m)$r.squared, digits = 3)))
      print(as.character(as.expression(eq)))
      as.character(as.expression(eq));
      
    }
    
    # lm_eqn(total)
    
    p + geom_text(x = 1, y = 3, label = lm_eqn(data), parse = TRUE, size=6)
    results$p<-p
    return(results)
  }
  
  plotDataStandard <- function(envir=NULL) {
    results<-NULL
    data<-envir$plotDataStandard
    p<-ggplot(data=data, map=aes(x=date, y=opValue)) + 
      geom_point(shape=1) +  # Use hollow circles
      geom_line(aes(colour=foiIdImport
                    ,group=interaction(foiIdImport,opId,type)
      ),size=0.8)
    results$p<-p
    envir$values$dataSummary<-summary(data)
    return(results)
  }
  
  
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
