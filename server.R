options(warn=-1)

list.of.packages <- c("shiny", "shinyBS", "openxlsx", "raster", "dismo","rJava", "maps", 
"utils", "DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(shiny)
require(shinyBS)
require(raster)
require(dismo)
require(openxlsx)
require(rJava)
require(maps)
require(utils)

jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (!file.exists(jar)) {
  stop("Download the required 'maxent.jar' file and palce in the appropriate directory")
}


unzip("data.zip", overwrite = T)

# Read in file and fix typos
raw.data <- read.xlsx("data/Raw_GH_Vaccine_Map.xlsx")
raw.data[which(raw.data$Impact.Scale == "Isolated "),11] <- "Isolated"
raw.data[which(raw.data$Impact.Scale == "Seconday "),11] <- "Secondary"
raw.data[which(raw.data$Impact.Scale == "Seconday"),11] <- "Secondary"
raw.data[which(raw.data$Outbreak == "Measles\n"),2] <- "Measles"
raw.data[which(raw.data$Outbreak == "Diphtheria"),2] <- "Diptheria"
raw.data[which(raw.data$Outbreak == "Typhoid Fever\n"),2] <- "Typhoid Fever"
raw.data[grepl("Polio*",raw.data$Outbreak),2] <- "Polio"
raw.data[which(raw.data$Outbreak == "Mumps\n"),2] <- "Mumps"
raw.data[which(raw.data$Outbreak == "Whooping Cough\n"),2] <- "Whooping Cough"
raw.data[grepl("Measles*",raw.data$Outbreak),2] <- "Measles"
raw.data[which(raw.data$Outbreak == "Annoucement"),2] <- "Announcement"
raw.data[which(raw.data$Outbreak == "Announcement "),2] <- "Announcement"
raw.data[which(raw.data$Year=="2101"),9] <- "2011"
raw.data[which(raw.data$Year=="214"),9] <- "2014"
raw.data[778, "Long"] <- "-3.16017"
raw.data[1051, "Lat"] <- "44.7300"

# Remove bad data
raw.data <- raw.data[-385, ]
raw.data <- raw.data[-which(raw.data$Impact.Scale == "Announcement"), ]
raw.data <- raw.data[-which(is.na(raw.data$Impact.Scale)), ]
raw.data <- raw.data[-which(raw.data$Impact.Scale == "."), ]

# Convert Latitude, Longitude, and years to numerics
raw.data$Lat <- as.numeric(raw.data$Lat)
raw.data$Long <- as.numeric(raw.data$Long)
raw.data$Year <- as.numeric(raw.data$Year)
raw.data$Fatalities <- as.numeric(raw.data$Fatalities)
raw.data$Impact.Scale <- as.factor(raw.data$Impact.Scale)

# Subset the rows, columns desired
data <- subset(raw.data, Outbreak %in% c("Measles", "Mumps", "Polio", "Rubella",
                                         "Whooping Cough"),
               select=c(Outbreak, Year, Long, Lat, Fatalities, Impact.Scale))

# Load in the bioClim raster data
load("data/bioclim_10m.Rdata")

# Load in the population density raster data
# Has to be modified to fit dimensions of bioClim
# Padding blank spaces to cover same space
pop.dens <- raster("data/glds00ag.bil")
projection(pop.dens) <- "+proj=longlat +ellps=WGS84"
e <- extent(-180,180,-60,90)
pop.dens <- extend(pop.dens,e)
pop.dens <- aggregate(pop.dens, fac=4)

shinyServer(
  function(input, output, session) {
   
    # Reactive plot that visualizes cases before running model
    output$visualize <- renderPlot({

      # Get range of years
      year.range <- input$years[1]:input$years[2]
      
      # Narrow down to desired outbreak types in desired years based on scale
      filtered.cases <- which(data$Outbreak %in% input$categories & 
                                data$Year %in% year.range &
                                data$Impact.Scale %in% input$impact.scale)

      # Filter on fatality selection
      if (as.numeric(input$fatalities) > 0) {
        filtered.cases <- filtered.cases[which(data[filtered.cases, "Fatalities"] > 0)]
      } else if (as.numeric(input$fatalities) == 0) {
        filtered.cases <- filtered.cases[which(data[filtered.cases, "Fatalities"] == 0)]
      } else {
        filtered.cases <- filtered.cases
      }
      
      # Plot occurences reactive to user selection
      occ <- data[filtered.cases,3:4]    
      map('world',interior=FALSE,col='gray', fill=TRUE, lty=0, ylim=c(-60, 90))
      points(occ, pch=20, cex=0.4)
      title(main=paste0("Number of outbreaks selected: ", nrow(occ)))
      mtext("N.B. At least 40 outbreaks are needed for modeling")
    })
    observeEvent(input$submitButton, {
      year.range <- input$years[1]:input$years[2]

      # Narrow down to desired outbreak types in desired years based on scale
      filtered.cases <- which(data$Outbreak %in% input$categories & 
                                data$Year %in% year.range &
                                data$Impact.Scale %in% input$impact.scale)

      # Further filtering on fatalities
      if (as.numeric(input$fatalities) > 0) {
        filtered.cases <- filtered.cases[which(data[filtered.cases, "Fatalities"] > 0)]
      } else if (as.numeric(input$fatalities) == 0) {
        filtered.cases <- filtered.cases[which(data[filtered.cases, "Fatalities"] == 0)]
      } else {
        filtered.cases <- filtered.cases
      }
      
      # Make sure there are at least 10 cases
      validate(need(length(filtered.cases) >= 40, ""))
      
      # Progress message
      progress = shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Running model. Please wait.", value = 0)
      
      
      # Add the climatic and socioeconomic data to one raster brick
      merge.socio.climate <- addLayer(bioStack, pop.dens)

      # Get what indices (layers in the raster brick) to be kept
      chosen.predictors <- as.numeric(c(input$precipitation, 
                                        input$temperature,
                                        input$socio))
      # Get occurences that match filters
      occ <- data[filtered.cases,3:4]
      
      # Cross-validation, 80-20 split
      fold <- kfold(occ, k = 5)
      occtest <- occ[fold == 1, ]
      occtrain <- occ[fold != 1, ]
      
      # If all predictors are to be used
      if (length(chosen.predictors) == nlayers(merge.socio.climate)) {
        maxent.model <- maxent(merge.socio.climate, occtrain)
      } else {
        # Drop the layers which aren't to be used
        merge.socio.climate <- dropLayer(merge.socio.climate, 
                                         i = c(1:nlayers(merge.socio.climate))[-chosen.predictors])
        maxent.model <- maxent(merge.socio.climate, occtrain)
      }
      maxent.map <- predict(maxent.model, merge.socio.climate)
      
      # Evaluating model
      # Generate background points
      bg <- randomPoints(merge.socio.climate, 1000)
      e1 <- evaluate(maxent.model, p=occtest, a=bg, x=merge.socio.climate)
      # Identify thresholds
      model.thresholds <- threshold(e1)

     
      # Render the map
      output$map <- renderPlot({
        plot(maxent.map, xlab="Longitude", ylab="Latitude")
        points(occ, pch=20, cex=0.4)
      })

      # Plot variable significance
      output$significance <- renderPlot({
        plot(maxent.model)
      })

      # Plot the AUC curve
      output$auc <- renderImage({
        outfile <- paste0(maxent.model@path,"/plots/species_roc.png")
        list(src = outfile,
             contentType = 'image/png',
             height = 390,
             width = 520
        )
      })

      # Create table of thresholds
      output$threshold <- DT::renderDataTable(
        DT::datatable(t(model.thresholds), options = list(searching = FALSE, paging = FALSE, list(4, 'desc')))
      )

      updateTabsetPanel(session, "outputs", selected = "Model Map")
    })
  }
)
