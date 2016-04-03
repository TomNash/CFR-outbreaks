options(warn=-1)
invisible(library(maptools))
invisible(library(gdata))
invisible(library(raster))
invisible(require(dismo))

data <- read.csv("CFR_Vaccine_Map_Corrected.csv")
load("./bioclim_10m.Rdata")

shinyServer(
  function(input, output) {
    observe({
      if (!(is.null(input$categories))) {
        year.range <- input$years[1]:input$years[2]
        filtered.cases <- which(data$Outbreak %in% input$categories & 
                                  data$Year %in% year.range)
        if (length(input$predictors) == nlayers(bioStack)) {
          maxent.model <- maxent(bioStack, data[filtered.cases,5:4])
        } else {
          bioStack@data@names <- colnames(bioStack@data@values)
          maxent.model <- maxent(dropLayer(bioStack, i = c(1:19)[-as.numeric(input$predictors)]),
                                 data[filtered.cases,5:4])
        }
        maxent.map <- predict(maxent.model, bioStack)
        output$map <- renderPlot({
          plot(maxent.map, xlab="Longitude", ylab="Latitude")
        })
        output$significance <- renderPlot({
          plot(maxent.model)
        })
    }
  })
  }
)