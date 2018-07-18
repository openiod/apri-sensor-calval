
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
                             )
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
    print('observe wrkDataChanged')
#    print(get_wrkDataChanged(wrkEnvA))
    output$showPlotPanelA <- renderText('A')
    wrkEnvA$plotDataStandard<-get_wrkData(wrkEnvA)

    isolate({
      plotType<-input$plotTypeA
      s<-get_wrkSensors(wrkEnvA)
    })
    
    if (nrow(data.frame(s))>1) { #} && plotTyp=='Regression') {
      print(t) 
      print('ok')
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

    output$plotWrkDataAPlot<-renderPlot({plotDataStandard(envir=wrkEnvA)})
    return()
    
    print('plot data for environment A')
    print(s)
    print ('after print(s)')
    output$plotWrkDataAPlot<-renderPlot({
      print('output plotWrkDataAPlot')
      print(s)
      print(t)
#      print(nrow(data.frame(s)))
      if (nrow(data.frame(s))>1 && plotTyp=='Regression') {
        print(t) 
        print('ok')
        
        t1 <- t %>% filter(foiIdImport==s$foiId[1])
        t2 <- t %>% filter(foiIdImport==s$foiId[2])
        #total<-full_join(t1,t2)
        total<-bind_cols(t1,t2)
        total$x<-total$opValue
        total$y<-total$opValue1
        print(total)
        wrkEnvA$plotDataRegression<-total

        p<-ggplot(data=total, aes(x=opValue, y=opValue1)) +
          geom_point(shape=1) +    # Use hollow circles
          geom_smooth(method=lm,   # Add linear regression line
                      se=FALSE)    # Don't add shaded confidence region
      
        lm_eqn <- function(df){
          m <- lm(y ~ x, df);
          #eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
          eq <- substitute(y == a + b %.% x*","~~r^2~"="~r2, 
                       list(a = format(coef(m)[1], digits = 2), 
                            b = format(coef(m)[2], digits = 2), 
                            r2 = format(summary(m)$r.squared, digits = 3)))
          print(as.character(as.expression(eq)))
          as.character(as.expression(eq));
          
        }
        
       # lm_eqn(total)
        p1 <- p + geom_text(x = 1, y = 3, label = lm_eqn(total), parse = TRUE, size=8)
        return (p1)      
      
      }
      
      output$plotWrkDataAPlot<-renderPlot({plotDataStandard(envir=wrkEnvA)})
#      p<-ggplot(data=t, map=aes(x=date, y=opValue)) + #geom_point()
#        geom_point(shape=1)  # Use hollow circles
#      p
      
    })
  })
  
  observe({
    plotType<-input$plotTypeA
    s<-isolate(get_wrkSensors(wrkEnvA))
    if (nrow(data.frame(s))>1 && plotType=='Regression') {
      p<-plotDataRegression(envir=wrkEnvA)
    } else {
      p<-plotDataStandard(envir=wrkEnvA)
    }
    output$plotWrkDataAPlot<-renderPlot({p})
    
  })
  
  plotDataRegression <- function (envir=NULL) {
    data<-envir$plotDataRegression
    p<-ggplot(data=data, aes(x=opValue, y=opValue1)) +
      geom_point(shape=1) +    # Use hollow circles
      geom_smooth(method=lm,   # Add linear regression line
                  se=FALSE)    # Don't add shaded confidence region
    
    lm_eqn <- function(df){
      m <- lm(y ~ x, df);
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
  }
  
  plotDataStandard <- function(envir=NULL) {
    data<-envir$plotDataStandard
    p<-ggplot(data=data, map=aes(x=date, y=opValue)) + #geom_point()
      geom_point(shape=1)  # Use hollow circles
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
