
envPlotUI <- function(id) {
  ns <- NS(id)
  
  
  tagList(  

      br()
    , verbatimTextOutput(ns("uiEnvPlotResultMessage"))
  
      , div(
          uiOutput(ns("showPlotPanelMain"))
        , conditionalPanel("output.showPlotPanelMain == 'Main'",
                          { 
                            pdf(NULL)
                            plotOutput(ns("plotWrkDataMainPlot"))
                          }  
          ,ns=ns                
        )
        , uiOutput(ns("showPlotPanelA"))
        , conditionalPanel("output.showPlotPanelA == 'A'",
                           { 
                             pdf(NULL)
                             tagList(
                             # Input: Selector for choosing type of graph/plot
                              selectInput(inputId = ns("plotTypeA"),
                                         label = "Plot type:",
                                         choices=c('Standard','Regression','Wind')
                                        )
                             , plotOutput(ns("plotWrkDataAPlot"))
                             , plotlyOutput(ns("plotWrkDataAPlotly"))
                             , verbatimTextOutput(ns("plotWrkDataASummary"))
                             )
                           }  
                           ,ns=ns                
        )
        , uiOutput(ns("showPlotPanelB"))
        , conditionalPanel("output.showPlotPanelB == 'B'",
                           { 
                             pdf(NULL)
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
                             pdf(NULL)
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
                             pdf(NULL)
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

  ## observe plottype per environment
  observe({
    print('observe: input plotType ')
    #print(input$plotTypeA)
    wrkEnvA$values$plotType<-input$plotTypeA
  })  
  observe({
    print('observe: input plotType ')
    #print(input$plotTypeB)
    wrkEnvB$values$plotType<-input$plotTypeB
  })  
  observe({
    print('observe: input plotType ')
    #print(input$plotTypeC)
    wrkEnvC$values$plotType<-input$plotTypeC
  })  
  observe({
    print('observe: input plotType ')
    #print(input$plotTypeD)
    wrkEnvD$values$plotType<-input$plotTypeD
  })  
  
  # when data changed, (re)bind data
  observe({
    envir<-wrkEnvA
    if(is.null(get_wrkDataChanged(envir))) return()
    if(get_wrkDataChanged(envir)==0) return()
    print('observe wrkDataChanged')
    output$showPlotPanelA <- renderText('A')
    bindOpValues(envir)
    output$plotWrkDataAPlot<-renderPlot({NULL})
  })
  observe({
    envir<-wrkEnvB
    if(is.null(get_wrkDataChanged(envir))) return()
    if(get_wrkDataChanged(envir)==0) return()
    print('observe wrkDataChanged')
    output$showPlotPanelB <- renderText('B')
    bindOpValues(envir)
    output$plotWrkDataBPlot<-renderPlot({NULL})
  })
  observe({
    envir<-wrkEnvC
    if(is.null(get_wrkDataChanged(envir))) return()
    if(get_wrkDataChanged(envir)==0) return()
    print('observe wrkDataChanged')
    output$showPlotPanelC <- renderText('C')
    bindOpValues(envir)
    output$plotWrkDataCPlot<-renderPlot({NULL})
  })
  observe({
    envir<-wrkEnvD
    if(is.null(get_wrkDataChanged(envir))) return()
    if(get_wrkDataChanged(envir)==0) return()
    print('observe wrkDataChanged')
    output$showPlotPanelD <- renderText('D')
    bindOpValues(envir)
    output$plotWrkDataDPlot<-renderPlot({NULL})
  })
  
  # bind data function 
  bindOpValues <- function(envir) {
    print('bindOpvalues')
    envir$plotDataStandard<-get_wrkData(envir)
    s<-get_wrkSensors(envir)

    # Wind data binding
    if (nrow(data.frame(s))>1) { 
      t<-envir$plotDataStandard
      envir$plotDataWind<-NULL
      windForceOp<-'ff'
      windDirection<-'dd'
      print(t)
      
      tF <- t %>% filter(opId==windForceOp)
      print(summary(tF))
      print(tF)
      print(length(tF$opId))
      if (length(tF$opId>0)) {
        if (nrow(data.frame(tF)>0)) {
          tD <- t %>% filter(opId==windDirection)
          if (length(tD$opId>0)) {
            if (nrow(data.frame(tD)>0)) {
              tD$windAngle<-(270-tD$opValue)*pi/180
              tF$windForce<-tF$opValue
              envir$plotDataWind<-bind_cols(tD,tF)
              envir$plotDataWind$date <- as.POSIXct(envir$plotDataWind$date) #make sure x are POSIXct and not just characters
              print(envir$plotDataWind)
            }
          }  
        }
      }
    }
    

#    p<-plotSticks(total$date,rep(0,3),total$wforce*cos(total$angle),
#                  total$wforce*sin(total$angle)
#                  #,yscale=0.4 
#                  ,ylim=c(-4.0, 12.0)
#    )    
    
    
    # Regression data binding
    if (nrow(data.frame(s))>1) { 
      envir$plotDataRegression<-NULL
      t<-envir$plotDataStandard
      t1 <- t %>% filter(foiIdImport==s$foiId[1]&opId==s$opId[1])
      t2 <- t %>% filter(foiIdImport==s$foiId[2]&opId==s$opId[2])
      if (nrow(data.frame(s))>2) {
        t3 <- t %>% filter(foiIdImport==s$foiId[3]&opId==s$opId[3])
      }  
      if (nrow(data.frame(s))>3) {
        t4 <- t %>% filter(foiIdImport==s$foiId[4]&opId==s$opId[4])
      }  
      if (nrow(data.frame(s))==2) {
        if (nrow(data.frame(t2))==0) return()
        print(summary(t1))
        print(summary(t2))
        total<-bind_cols(t1,t2)
        total$y<-total$opValue
        total$x<-total$opValue1
      }
      if (nrow(data.frame(s))==3) {
        if (nrow(data.frame(t2))==0) return()
        if (nrow(data.frame(t3))==0) return()
        total<-bind_cols(t1,t2,t3)
        total$y<-total$opValue
        total$x1<-total$opValue1
        total$x2<-total$opValue2
      }
      if (nrow(data.frame(s))==4) {
        if (nrow(data.frame(t2))==0) return()
        if (nrow(data.frame(t3))==0) return()
        if (nrow(data.frame(t4))==0) return()
        print(summary(t1))
        print(summary(t2))
        print(summary(t3))
        print(summary(t4))
        total<-bind_cols(t1,t2,t3,t4)
        total$y<-total$opValue
        total$x1<-total$opValue1
        total$x2<-total$opValue2
        total$x3<-total$opValue3
      }
      print(total)
      envir$plotDataRegression<-total
    }  
    
  }
  
  observe({
    print('observe: input plotType ')
    tmp_envir<-wrkEnvA
#    tmp_envir$values$plotType<-input$plotTypeA
    print(tmp_envir$values$plotType)
    r<-NULL
    r$p<-NULL
    print(r)
    print(r$p)
    r<-createPlot(envir=tmp_envir)
    print(r)
    print(r$p)
    
    if(is.ggplot(r$p)) {
      print('r$p is a ggplot')
      if (is.installed('plotly') ) {
        output$plotWrkDataAPlotly<-renderPlotly({ggplotly(r$p)})
      } else {
        output$plotWrkDataAPlot<-renderPlot({r$p})
      }
      
      if (!is.null(r)&!is.null(r$p)&tmp_envir$values$plotType=='Regression'){
        output$plotWrkDataASummary<-renderPrint({
          print(tmp_envir$values$dataSummary$call)
          print(tmp_envir$values$dataSummary$r.squared)
          print(tmp_envir$values$dataSummary$residuals)
          #print(tmp_envir$values$dataSummary[1-4])
          print(tmp_envir$values$dataSummary)
        })
      }
    } else {
      print(paste('r$p is not a ggplot','for plottype',tmp_envir$values$plotType))
      print(paste('plotly installed: ',is.installed('plotly')) )
      if (is.installed('plotly') ) {
        output$plotWrkDataAPlotly<-renderPlotly({r$p})
      } else {
        output$plotWrkDataAPlot<-renderPlot({r$p})
      }
    }
    if (tmp_envir$values$plotType=='Standard'){
      output$plotWrkDataASummary<-renderPrint({
        print(tmp_envir$values$dataSummary)
      })
    }
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
    r<-NULL
    s<-get_wrkSensors(envir=envir)
    print(s)
    print(envir$values$plotType)
    if (is.null(s)) return(NULL)
    if (is.null(envir$values$plotType)) return(NULL)
    
    print(nrow(data.frame(s)))

    isolate({
      #s<-get_wrkSensors(envir=envir)
      print(nrow(data.frame(s)))
      if (envir$values$plotType=='Regression') {
        if(nrow(data.frame(s))>1) {
          r<-plot_DataRegression(envir=envir)
        }
      } else {
        if (envir$values$plotType=='Wind' & !is.null(envir$plotDataWind)) {
          r<-plot_DataWind(envir=envir)
        } else {
          r<-plot_DataStandard(envir=envir)
        }  
      }
    })
  }
  
  plot_DataWind <- function (envir=NULL) {
    print('plot_DataWind')
    results<-NULL
    data<-envir$plotDataWind
    print(data)
    print(data$windAngle)
    print(data$windForce)
    print(data$date)
    #p<-plotSticks(data$date,rep(0,3),data$windForce*cos(data$windAngle),
#    p<-oce.plot.sticks(data$date,rep(0,3),data$windForce*cos(data$windAngle),
#                data$windForce*sin(data$windAngle)
#                #,yscale=0.4 
#                #,ylim=c(-4.0, 12.0)
#    )
    u <- rnorm(100)
    v <- rnorm(100)
    t <- seq(0, 100, length.out=100)
    p<-plotSticks(t, 0, u, v, yscale=5)    
    
#    p<-ggplot(data=data, map=aes(x=date, y=windForce)) +
#              geom_line(aes(colour=foiIdImport,
#                            group=interaction(foiIdImport,opId,type)) ) +
#              geom_line(aes(colour=foiIdImport,y=windAngle,
#                 group=interaction(foiIdImport,opId,type)) )          
    results$p<-p
    return(results)
  }
  
  plot_DataRegression <- function (envir=NULL) {
    print('plot_DataRegression')
    results<-NULL
    data<-envir$plotDataRegression
    if (is.null(data)) return()
    p<-ggplot(data=data, aes(x=opValue, y=opValue1)) +
      geom_point(shape=1) +    # Use hollow circles
      geom_smooth(method=lm,   # Add linear regression line
                  se=FALSE)    # Don't add shaded confidence region
    
    lm_eqn <- function(df){
      print(df)
      if(all(c("x1", "x2", "x3") %in% colnames(df)) ) {  
        m <- lm(y ~ x1+x2+x3, df);
      } else if(all(c("x1", "x2") %in% colnames(df))){
        m <- lm(y ~ x1+x2, df);
      } else if("x1" %in% colnames(df)){
        m <- lm(y ~ x1, df);
      } else if("x" %in% colnames(df)){
        m <- lm(y ~ x, df);
      }
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
  
  plot_DataStandard <- function(envir=NULL) {
    print('plot_DataStandard')
    results<-NULL
    data<-envir$plotDataStandard
    p<-ggplot(data=data, map=aes(x=date, y=opValue)) + 
      geom_point(shape=1) +  # Use hollow circles
      geom_line(aes(colour=foiIdImport,linetype=opId,
                    group=interaction(foiIdImport,opId,type)
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
