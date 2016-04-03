shinyUI(fluidPage(
  titlePanel("Vaccine-Preventable Outbreaks"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create maximum entropy maps with information from 
               Council on Foreign Relations data on vaccine-preventable 
               outbreaks."),
      sliderInput("years", "Years of interest:",
                  min = 2007, max = 2016, value = c(2007, 2016), tick=F, sep=""),
      checkboxGroupInput("categories", h3("Response Categories"),
                         choices = list("Polio" = "Polio", 
                              "Whooping Cough" = "Whooping Cough", 
                             "Measles" = "Measles",
                             "Mumps" = "Mumps",
                             "Rubella" = "Rubella",
                             "Polio" = "Polio",
                             "Violence" = "Violence")),
      checkboxGroupInput("predictors", h3("Predictors"),
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
                                        "Avg Temp of Coldest Quarter (tcoldq)" = 11,
                                        "Annual Precip (ap)" = 12,
                                        "Precip of Wettest Month (pwet)" = 13,
                                        "Precip of Driest Month (pdry)"= 14,
                                        "Precip Seasonality (pseas)" = 15,
                                        "Precip of Wettest Quarter (pwetq)" = 16,
                                        "Precip of Driest Quarter (pdryq)" = 17,
                                        "Precip of Warmest Quarter (pwarmq)" = 18,
                                        "Precip of Coldest Quarter (pcoldq)" = 19), 
                         selected=1:19),
      submitButton("Submit")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", plotOutput("map")), 
        tabPanel("Variable Significance", plotOutput("significance")),
        tabPanel("AUC", imageOutput("auc"))
      )
    )
  )
))