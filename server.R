options(warn=-1)
library(maptools)
library(gdata)
library(raster)
library(dismo)

data <- read.csv("CFR_Vaccine_Map_Corrected.csv")
load("bioclim_10m.Rdata")

pop.dens <- raster("~/Downloads/gl_gpwv3_pdens_00_bil_25/glds00ag.bil")
projection(pop.dens) <- "+proj=longlat +ellps=WGS84"
e <- extent(-180,180,-60,90)
pop.dens <- extend(pop.dens,e)
pop.dens <- aggregate(pop.dens, fac=4)


shinyServer(
  function(input, output) {
    observe({
      if (!(is.null(input$categories))) {
        progress = shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Running model. Please wait.", value = 0)
              
        year.range <- input$years[1]:input$years[2]
        filtered.cases <- which(data$Outbreak %in% input$categories & 
                                  data$Year %in% year.range)
        
        merge.socio.climate <- addLayer(bioStack, pop.dens)
#         merge.socio.climate@data@names <- c(colnames(bioStack@data@values), 
#                                            names(pop.dens@data))
        chosen.predictors <- as.numeric(c(input$predictors.clim, 
                                          input$predictors.socio))
        print(chosen.predictors)
        if (length(chosen.predictors) == nlayers(merge.socio.climate)) {
          maxent.model <- maxent(merge.socio.climate, data[filtered.cases,5:4])
        } else {
          merge.socio.climate <- dropLayer(merge.socio.climate, 
                                           i = c(1:20)[-chosen.predictors])
          maxent.model <- maxent(merge.socio.climate, data[filtered.cases,5:4])
        }
        maxent.map <- predict(maxent.model, merge.socio.climate)       
      
        output$map <- renderPlot({
          plot(maxent.map, xlab="Longitude", ylab="Latitude")
        })
        output$significance <- renderPlot({
          plot(maxent.model)
        })
        output$auc <- renderImage({
          outfile <- paste0(maxent.model@path,"/plots/species_roc.png")
          list(src = outfile,
               contentType = 'image/png')
        })
      }
    })
  }
)
