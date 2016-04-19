library(shiny)
library(shinyBS)

shinyUI(fluidPage(
  titlePanel("Vaccine-Preventable Outbreaks"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create maximum entropy maps with information from 
               Council on Foreign Relations data on vaccine-preventable 
               outbreaks."),
      sliderInput("years", "Years of interest:",
                  min = 2007, max = 2016, value = c(2007, 2016), tick=F, sep=""),
     
      checkboxGroupInput("categories",
                         h4("Response Categories"),
                         choices = list("Measles" = "Measles",
                                        "Mumps" = "Mumps",
                                        "Polio" = "Polio",
                                        "Rubella" = "Rubella",
                                        "Whooping Cough" = "Whooping Cough"),
                         selected = c("Measles", "Mumps", "Polio", "Rubella", "Whooping Cough")),
      
      checkboxGroupInput("impact.scale",
                         h4("Impact Scale"),
                         choices = list("Epidemic" = "Epidemic",
                                        "Secondary" = "Secondary",
                                        "Cluster" = "Cluster",
                                        "Isolated" = "Isolated"),
                         selected=c("Epidemic","Secondary","Cluster","Isolated")),
      radioButtons("fatalities",
                         h4("Fatalities"),
                         choices = list("Include only fatal outbreaks" = 0,
                                        "Include only non-fatal outbreaks" = 1,
                                        "Include all outbreaks regardless of fataliites" = -1),
                   selected=-1),
      
      h4("Predictor Variables"),
      bsCollapse(
        bsCollapsePanel("Temperature",
                        checkboxGroupInput("temperature", label=NULL,
                                           choices = list("Annual Avg Temp (mat)" = 1,
                                          "Avg Diurnal Range (mdr)" = 2,
                                          "Isothermality (iso)" = 3,
                                          "Temp Seasonality (tseas)" = 4,
                                          "Max Temp of Warmest Month (tmax)" = 5,
                                          "Min Temp of Coldest Month (tmin)" = 6,
                                          "Temp Annual Range (tar)" = 7,
                                          "Avg Temp of Wettest Quarter (twetq)" = 8,
                                          "Avg Temp of Driest Quarter (tdryq)" = 9,
                                          "Avg Temp of Warmest Quarter (twarmq)" = 10,
                                          "Avg Temp of Coldest Quarter (tcoldq)" = 11),
                           selected = 1:11)
                        ),
        bsCollapsePanel("Precipitation",
                        checkboxGroupInput("precipitation", label=NULL,
                                           choices = list("Annual Precip (ap)" = 12,
                                                          "Precip of Wettest Month (pwet)" = 13,
                                                          "Precip of Driest Month (pdry)"= 14,
                                                          "Precip Seasonality (pseas)" = 15,
                                                          "Precip of Wettest Quarter (pwetq)" = 16,
                                                          "Precip of Driest Quarter (pdryq)" = 17,
                                                          "Precip of Warmest Quarter (pwarmq)" = 18,
                                                          "Precip of Coldest Quarter (pcoldq)" = 19),
                                           selected = 12:19)
                        ),
        bsCollapsePanel("Socioeconomic",
                        checkboxGroupInput("socio", label=NULL, 
                                           choices = list("Population Density (glds00ag)" = 20),
                                           selected=20)
                        )
      ),
      actionButton("submitButton", "Submit")
      ),
    mainPanel(style="position:fixed; right:0;",
      tabsetPanel(
        tabPanel("Visualize", plotOutput("visualize")),
        tabPanel("Model Map", plotOutput("map")), 
        tabPanel("Variable Significance", plotOutput("significance")),
        tabPanel("AUC", imageOutput("auc")),
        tabPanel("Thresholds", tableOutput("threshold")),
        id="outputs"
      )
    )
  )
))
