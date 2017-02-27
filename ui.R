library(leaflet)

# Choices for drop-downs
models <- c(
  "MaxEnt" = "maxent",
  "Boosted Regression Tree" = "brt"
)


navbarPage("Outbreak Modeling", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Exploration"),
                                      fileInput('data', 'Data file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                      p("Modeling"),
                                      actionButton("runModel", "Run Models"),
                                      br(),br(),
                                      radioButtons("models.map","View Models",
                                                   c("Maxent","BRT"),
                                                   selected = "Maxent")
                        ),
                        absolutePanel(id = "viz", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, right = "auto", left = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      h2("Plots"),
                                      plotOutput("significance", height = 300),
                                      plotOutput("auc", height = 300)
                        )
                    )
           ),
           tabPanel("Data explorer",
                    fluidRow(
                      DT::dataTableOutput("table")
                    )
                    ,
                    conditionalPanel("false", icon("crosshair"))
           )
)