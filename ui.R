library(leaflet)
library(shinyBS)

# Choices for drop-downs

navbarPage(
  "Outbreak Modeling",
  id = "nav",
  
  tabPanel(
    "Interactive map",
    div(
      class = "outer",
      
      tags$head(# Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")),
      
      leafletOutput("map", width = "100%", height = "100%"),
      
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 60,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = 330,
        height = "auto",
        
        h2("Data Upload"),
        fileInput(
          'data',
          'Data file',
          accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
        )
      ),
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 250,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = 330,
        height = "auto",
        h2("Modeling"),
        p("Predictors for Models"),
        bsCollapse(
          bsCollapsePanel(
            "Temperature",
            checkboxGroupInput(
              "temperature",
              label = NULL,
              choices = list(
                "Annual Avg Temp (mat)" = 1,
                "Avg Diurnal Range (mdr)" = 2,
                "Isothermality (iso)" = 3,
                "Temp Seasonality (tseas)" = 4,
                "Max Temp of Warmest Month (tmax)" = 5,
                "Min Temp of Coldest Month (tmin)" = 6,
                "Temp Annual Range (tar)" = 7,
                "Avg Temp of Wettest Quarter (twetq)" = 8,
                "Avg Temp of Driest Quarter (tdryq)" = 9,
                "Avg Temp of Warmest Quarter (twarmq)" = 10,
                "Avg Temp of Coldest Quarter (tcoldq)" = 11
              ),
              selected = 1:11
            )
          ),
          bsCollapsePanel(
            "Precipitation",
            checkboxGroupInput(
              "precipitation",
              label = NULL,
              choices = list(
                "Annual Precip (ap)" = 12,
                "Precip of Wettest Month (pwet)" = 13,
                "Precip of Driest Month (pdry)" = 14,
                "Precip Seasonality (pseas)" = 15,
                "Precip of Wettest Quarter (pwetq)" = 16,
                "Precip of Driest Quarter (pdryq)" = 17,
                "Precip of Warmest Quarter (pwarmq)" = 18,
                "Precip of Coldest Quarter (pcoldq)" = 19
              ),
              selected = 12:19
            )
          ),
          bsCollapsePanel(
            "Socioeconomic",
            checkboxGroupInput(
              "socio",
              label = NULL,
              choices = list("Population Density (glds00ag)" = 20),
              selected =
                20
            )
          )
        ),
        actionButton("run.model", "Run Models"),
        br(),
        br(),
        radioButtons("models.map", "View Models",
                     c("Maxent", "BRT"),
                     selected = "Maxent")
      ),
      absolutePanel(
        id = "viz",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 60,
        right = "auto",
        left = 20,
        bottom = "auto",
        width = 330,
        height = "auto",
        h2("Plots"),
        plotOutput("significance", height = 350),
        plotOutput("auc", height = 350)
      )
    )
  ),
  tabPanel(
    "Data explorer",
    fluidRow(DT::dataTableOutput("table"))
    ,
    conditionalPanel("false", icon("crosshair"))
  )
)