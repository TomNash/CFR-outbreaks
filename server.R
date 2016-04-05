options(warn=-1)
library(raster)
library(dismo)
library(openxlsx)


unzip("data.zip", overwrite = T)

print("Downloading data file from CFR")
download.file("https://docs.google.com/spreadsheet/pub?key=0AmNfP3LEaDfkdE9Qd0o5Y01HU0xHVDF5eVpJSno3dHc&single=true&gid=0&output=xls",
              destfile="data/Raw_GH_Vaccine_Map.xlsx")

# Read in file and fix typos
raw.data <- read.xlsx("data/Raw_GH_Vaccine_Map.xlsx")
raw.data[which(raw.data$Impact.Scale == "Isolated "),11] <- "Isolated"
raw.data[which(raw.data$Impact.Scale == "Seconday "),11] <- "Secondary"
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
raw.data[617,7] <- "-3.16017"
raw.data[832,6] <- "44.7300"

# Convert Latitude, Longitude, and years to numerics
raw.data$Lat <- as.numeric(raw.data$Lat)
raw.data$Long <- as.numeric(raw.data$Long)
raw.data$Year <- as.numeric(raw.data$Year)

# Subset the columns desired
data <- subset(raw.data, select=c(Outbreak, Year, Long, Lat))

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
  function(input, output) {
    observe({
      if (!(is.null(input$categories))) {
	# Progress message
        progress = shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Running model. Please wait.", value = 0)
        
        year.range <- input$years[1]:input$years[2]

	# Narrow down to desired outbreak types in desired years
        filtered.cases <- which(data$Outbreak %in% input$categories & 
                                  data$Year %in% year.range)
        
	# Add the climatic and socioeconomic data to one raster brick
        merge.socio.climate <- addLayer(bioStack, pop.dens)

	# Get what indices (layers in the raster brick) to be kept
        chosen.predictors <- as.numeric(c(input$precipitation, 
                                          input$temperature,
                                          input$socio))
        
	# If all predictors are to be used
        if (length(chosen.predictors) == nlayers(merge.socio.climate)) {
          maxent.model <- maxent(merge.socio.climate, data[filtered.cases,])
        } else {
	  # Drop the layers which aren't to be used
          merge.socio.climate <- dropLayer(merge.socio.climate, 
                                           i = c(1:nlayers(merge.socio.climate))[-chosen.predictors])
          maxent.model <- maxent(merge.socio.climate, data[filtered.cases,3:4])
        }
        maxent.map <- predict(maxent.model, merge.socio.climate)       
        
	# Render the map
        output$map <- renderPlot({
          plot(maxent.map, xlab="Longitude", ylab="Latitude")
        })
	# Plot variable significance
        output$significance <- renderPlot({
          plot(maxent.model)
        })
	# Plot the AUC curve
        output$auc <- renderImage({
          outfile <- paste0(maxent.model@path,"/plots/species_roc.png")
          list(src = outfile,
               contentType = 'image/png')
        })
      }
    })
  }
)
