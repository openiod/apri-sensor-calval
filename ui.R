#fluidPage(

  # App title ----
# titlePanel("Project viewer"),

  # Sidebar layout with input and output definitions ----
#  sidebarLayout(

    # Sidebar panel for inputs ----
#    sidebarPanel(
      # Input: Selector for choosing project ----
   #   selectInput(inputId = "projectId",
  #      label = "Project:",
  #      choices=projectList$projectName
  #    ),
  #    uiOutput("projectDetails"),
  #    br(),

  #    uiOutput("projectFois"),
  #    uiOutput("foiDetails"),
  #    br(),

#      uiOutput("sensors"),
      
#      dateRangeInput("inDateRange", "Date range input:")
      
 #   ),  # end of sidebarPanel

    # Main panel for displaying outputs ----
#    mainPanel(

#    )
# )
#)
useShinyjs()
header <- dashboardHeader(
  title = "Project Visibilis"
)
sidebar <- dashboardSidebar(
  tags$head(tags$style(HTML(mycss)))
  # include the message.js script so the JavaScript knows the custom message handler we have written
#  , singleton(
#    tags$head(tags$script(src = "js/message-handler.js"))
#  )
  , singleton(
    tags$head(tags$script(src = "message-handler.js"))
  )
  , includeScript(paste(getwd(),"/www/js/ApriSensor.js",sep=''))
#  , includeScript(paste(getwd(),"/www/js/message-handler.js",sep=''))
  , actionButton("processButton", "Verwerk")
  , p("Klik op de knop om de selectie te verwerken.")
  , verbatimTextOutput("processButtonText")
  
  
)
sensorBody <- dashboardBody(
  tabsetPanel(
    id = 'panels',
      tabPanel("Periode selectie", {
      div(id = "periodSelect-container",
          DT::dataTableOutput("periodeSelectieTable")
          ,periodSelectUI(idList["name"=="serverPeriodUI_ssp"]$id)
      )
    })
    , tabPanel("Sensor selectie", {
        div(id = "sensorSelect-container",
            DT::dataTableOutput("sensorSelectieTable")
          , sensorSelectUI(idList["name"=="serverSensorUI_ssp"]$id)
        )
      })
    , tabPanel("Data samenvatting", {
        div(id = "envSummary-container",
          envSummaryUI(idList["name"=="envSummaryUI_ssp"]$id)
        )    
      })
    , tabPanel("sensor data", DT::dataTableOutput("wrkSensorDataTable"))
    , tabPanel("plot", {
        div(id = "plot-container",
          tags$img(src = "spinner.gif",
            id = "loading-spinner"),
          envPlotUI(idList["name"=="envPlotUI_ssp"]$id)
        )
      })
    , tabPanel("Kaart", { 
        div(id = "kaart-container",
          box(leafletOutput("sensorMap"))
        )
      })
    , tabPanel(title=uiOutput("titlePanelActiveEnv"), { 
    })
    )
)
#dashboardPage(header, sidebar, body)
navbarPage("OpenIoD.org"
  , tabPanel("Introductie",
      tabsetPanel(
        id = 'panels',
          tabPanel("Introductie", {
            includeMarkdown(paste(getwd(),"/www/md/introduction.Rmd",sep=''))
            })
          , tabPanel("Instructie", {
          includeMarkdown(paste(getwd(),"/www/md/instruction.Rmd",sep=''))
        })
      ))
  , tabPanel("sensor", sensorBody)
  , tabPanel("Kalibratie")
  , tabPanel("Validatie")
  , navbarMenu("Sensordata"
    , tabPanel("Projecten")
    , tabPanel("Sensoren", DT::dataTableOutput("wrkSensorTable"))
    , tabPanel("Meetdata")
  )  
)

