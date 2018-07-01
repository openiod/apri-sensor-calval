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
  , includeScript(paste(getwd(),"/www/js/ApriSensor.js",sep=''))
    
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
          #,sensorSelectUI('periodSelect')
      )
    })
    , tabPanel("Sensor selectie", {
        div(id = "sensorSelect-container",
            DT::dataTableOutput("sensorSelectieTable")
          , sensorSelectUI(idList["name"=="serverSensorUI_ssp"]$id)
        )
      })
    , tabPanel("sensor data", DT::dataTableOutput("wrkSensorDataTable"))
    , tabPanel("plot", {
        div(id = "plot-container",
          tags$img(src = "spinner.gif",
            id = "loading-spinner"),
          plotOutput("sensorPlot1", height = 500)
        )
      })
    , tabPanel("Kaart", { 
        div(id = "kaart-container",
          box(leafletOutput("sensorMap"))
        )
      })
  )
)
#dashboardPage(header, sidebar, body)
navbarPage("OpenIoD.org"
  , tabPanel("Introductie")
  , tabPanel("sensorBody", sensorBody)
  , tabPanel("Kalibratie")
  , tabPanel("Validatie")
  , navbarMenu("Sensordata"
    , tabPanel("Projecten")
    , tabPanel("Sensoren", DT::dataTableOutput("wrkSensorTable"))
    , tabPanel("Meetdata")
    , tabPanel(getwd())
  )  
)

