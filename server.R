# options(warn=-1)

# leaflet(options = leafletOptions(worldCopyJump = T)) %>%
#   addTiles(
#     urlTemplate = "//api.mapbox.com/styles/v1/nashtf/ciziymwzy002i2smqmvqvygv7/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibmFzaHRmIiwiYSI6ImNpeml5bTc4NzAybTIzM21pN2NoeXJseXMifQ.NwM6pg94SamDh9vVRxpmgA.png",
#     attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
#   ) %>% addRasterImage(maxent.map) %>% setView(lng = -93.85, lat = 37.45, zoom = 2)
# %>% addCircleMarkers(lng=occ$Long[1:10], lat=occ$Lat[1:10], weight=1) %>% addPopups(lng=occ$Long[1:10], lat = occ$Lat[1:10], "hi")

# leaflet() %>%
#   addTiles(
#     urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#     attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
#   ) %>%
#   setView(lng = -93.85, lat = 37.45, zoom = 4) %>% addPopups(lng=occ$Long[1:10], lat = occ$Lat[1:10], "hi")


# list.of.packages <- c("shiny", "shinyBS", "openxlsx", "raster", "dismo","rJava", "maps", 
# "utils", "DT")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinyBS)
library(raster)
library(dismo)
library(openxlsx)
library(rJava)
library(maps)
library(utils)
library(tools)
library(ggmap)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shinyjs)
library(randomForest)

# jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
# 
# if (!file.exists(jar)) {
#   stop("Download the required 'maxent.jar' file and palce in the appropriate directory")
# }


# unzip("data.zip", overwrite = T)
# 
# # Read in file and fix typos
# outbreak.data <- read.xlsx("data/Raw_GH_Vaccine_Map.xlsx")
# outbreak.data[which(outbreak.data$Impact.Scale == "Isolated "),11] <- "Isolated"
# outbreak.data[which(outbreak.data$Impact.Scale == "Seconday "),11] <- "Secondary"
# outbreak.data[which(outbreak.data$Impact.Scale == "Seconday"),11] <- "Secondary"
# outbreak.data[which(outbreak.data$Outbreak == "Measles\n"),2] <- "Measles"
# outbreak.data[which(outbreak.data$Outbreak == "Diphtheria"),2] <- "Diptheria"
# outbreak.data[which(outbreak.data$Outbreak == "Typhoid Fever\n"),2] <- "Typhoid Fever"
# outbreak.data[grepl("Polio*",outbreak.data$Outbreak),2] <- "Polio"
# outbreak.data[which(outbreak.data$Outbreak == "Mumps\n"),2] <- "Mumps"
# outbreak.data[which(outbreak.data$Outbreak == "Whooping Cough\n"),2] <- "Whooping Cough"
# outbreak.data[grepl("Measles*",outbreak.data$Outbreak),2] <- "Measles"
# outbreak.data[which(outbreak.data$Outbreak == "Annoucement"),2] <- "Announcement"
# outbreak.data[which(outbreak.data$Outbreak == "Announcement "),2] <- "Announcement"
# outbreak.data[which(outbreak.data$Year=="2101"),9] <- "2011"
# outbreak.data[which(outbreak.data$Year=="214"),9] <- "2014"
# outbreak.data[778, "Long"] <- "-3.16017"
# outbreak.data[1051, "Lat"] <- "44.7300"
# 
# # Remove bad data
# outbreak.data <- outbreak.data[-385, ]
# outbreak.data <- outbreak.data[-which(outbreak.data$Impact.Scale == "Announcement"), ]
# outbreak.data <- outbreak.data[-which(is.na(outbreak.data$Impact.Scale)), ]
# outbreak.data <- outbreak.data[-which(outbreak.data$Impact.Scale == "."), ]
# 
# # Convert Latitude, Longitude, and years to numerics
# outbreak.data$Lat <- as.numeric(outbreak.data$Lat)
# outbreak.data$Long <- as.numeric(outbreak.data$Long)
# outbreak.data$Year <- as.numeric(outbreak.data$Year)
# outbreak.data$Fatalities <- as.numeric(outbreak.data$Fatalities)
# outbreak.data$Impact.Scale <- as.factor(outbreak.data$Impact.Scale)

# # Subset the rows, columns desired
# data <- subset(outbreak.data, Outbreak %in% c("Measles", "Mumps", "Polio", "Rubella",
#                                          "Whooping Cough"),
#                select=c(Outbreak, Year, Long, Lat, Fatalities, Impact.Scale))

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
merge.socio.climate <- addLayer(bioStack, pop.dens)
models.run <<- F

shinyServer(

  function(input, output, session) {

    # Create the map
    output$map <- renderLeaflet({
          leaflet(options = leafletOptions(worldCopyJump = T)) %>%
              addTiles(
                #urlTemplate = "//api.mapbox.com/styles/v1/nashtf/ciziymwzy002i2smqmvqvygv7/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibmFzaHRmIiwiYSI6ImNpeml5bTc4NzAybTIzM21pN2NoeXJseXMifQ.NwM6pg94SamDh9vVRxpmgA.png",
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
              ) %>% setView(lng = -13.85, lat = 37.45, zoom = 2)
    })
    
    outbreaksInBounds <- reactive({
      if (is.null(input$map_bounds))
        return()
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(outbreak.data,
             Lat >= latRng[1] & Lat <= latRng[2] & 
               Long >= lngRng[1] & Long <= lngRng[2])
    })
    
    output$histSize <- renderPlot({
      # If no zipcodes are in view, don't plot
      if (nrow(outbreaksInBounds()) == 0)
        return(NULL)
      
      hist(outbreaksInBounds()$centile,
           breaks = centileBreaks,
           main = "Outbreak Size",
           xlab = "Frequency",
           xlim = range(allzips$centile),
           col = '#00DD00',
           border = 'white')
    })
    

    observeEvent(input$data, {
      data.file <- input$data

      if (is.null(data.file))
        return(NULL)

      if (file_ext(data.file$name) == "txt")
        outbreak.data <<- read.table(data.file$datapath)
      else
        outbreak.data <<- read.csv(data.file$datapath)

      names(outbreak.data)[grepl("long", names(outbreak.data), ignore.case = T)] <- "Long"
      names(outbreak.data)[grepl("lat", names(outbreak.data), ignore.case = T)] <- "Lat"
      names(outbreak.data)[grepl("cases", names(outbreak.data), ignore.case = T)] <- "Cases"

      radius <- log(outbreak.data$Cases) + 5
      
      leafletProxy("map", data = outbreak.data) %>%
        clearShapes() %>%
        addCircleMarkers(~Long, ~Lat, radius=radius,
                   stroke=FALSE, fillOpacity=0.4, fillColor="blue") #%>% addLegend("bottomleft", pal=pal, values=colorData, title=colorBy, layerId="colorLegend")
      shinyjs::hide(id = "viz")
      
    })
    
    observe({
      if (is.null(input$goto))
        return()
      isolate({
        map <- leafletProxy("map")
        map %>% clearPopups()
        dist <- 10
        lat <- input$goto$lat
        lng <- input$goto$lng
        showOutbreakInfo(lat, lng)
        map %>% fitBounds(lng + dist, lat + dist, lng - dist, lat - dist)
      })
    })
    
    output$table <- DT::renderDataTable({
      #print(outbreak.data[1,1:11]),
      #DT::datatable(outbreak.data[,1:11])
      DT::datatable(outbreak.data[,1:11])
      df <- outbreak.data %>%
      select(Outbreak,Location,Continent,Lat,Long,Year,Cases,Impact.Scale) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
      action <- DT::dataTableAjax(session, df)
      print(df[1,])
      DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    })

    # Show a popup at the given location
    showOutbreakInfo <- function(lat, lng) {

      matched.popups <- subset(outbreak.data, Lat == lat & Long == lng)

      if (nrow(matched.popups) > 0) {
        revgeo.out <- lapply(1:nrow(matched.popups), function(i) {
          revgeocode(as.numeric(matched.popups[i,c("Long","Lat")]), output = "more", messaging = F)
        })

        content <- lapply(1:length(revgeo.out), function(i) {
          as.character(tagList(
            tags$h4(matched.popups[i,"Outbreak"]),
            tags$strong(HTML(sprintf("%s, %s",
                                     revgeo.out[[i]]$administrative_area_level_1,
                                     revgeo.out[[i]]$country))),
            tags$br(),sprintf("Year: %d", matched.popups[i,"Year"]),
            tags$br(),sprintf("Number of cases: %d", matched.popups[i,"Cases"]),
            tags$br(),sprintf("Scale: %s", matched.popups[i,"Impact.Scale"]),
            tags$br()
          ))
        })
        leafletProxy("map") %>% addPopups(lng=matched.popups$Long,
                                          lat=matched.popups$Lat, content)
      }
      else {
        return()
      }
    }

    # When map is clicked, show a popup with city info
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_marker_click
      if (is.null(event))
        return()
      isolate({
        showOutbreakInfo(event$lat, event$lng)
      })
    })

    observeEvent(input$runModel, {
      
      print("why")
      models.run <<- F

      # Make sure there are at least 10 cases
      validate(need(nrow(outbreak.data) >= 40, ""))

      # Progress message
      progress = shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Running MaxEnt model. Please wait.", value=0)

      # Get occurences that match filters
      occ <- outbreak.data[,c("Long","Lat")]

      # Cross-validation, 80-20 split
      fold <- kfold(occ, k = 5)
      occtest <- occ[fold == 1, ]
      occtrain <- occ[fold != 1, ]

      maxent.model <<- maxent(merge.socio.climate, occtrain)
      maxent.map <<- predict(maxent.model, merge.socio.climate)

      progress$inc(0.5, detail = "Evaluating MaxEnt model")

      # Evaluating model
      # Generate background points
      bg <- randomPoints(merge.socio.climate, 1000)
      e1 <<- evaluate(maxent.model, p=occtest, a=bg, x=merge.socio.climate)

      # Identify thresholds
      maxent.thresholds <- threshold(e1)

      models.run <<- T
      overlayRaster()
    })
    
    overlayRaster <- function() {
      if(input$models.map == "Maxent") {
        leafletProxy("map") %>% clearImages() %>% clearControls() %>%
          addRasterImage(layerId = "maxent", maxent.map, opacity=0.5,
                         colors=brewer.pal(5, "YlOrRd")) %>%
          addLegend(values=values(maxent.map), title="Likelihood", 
                    colors=brewer.pal(5, "YlOrRd"), 
                    labels=c("0.0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1.0"),
                    position="bottomright")
        
        output$significance <- renderPlot({
          plot(maxent.model)
        })
        output$auc <- renderPlot({
          plot(e1, 'ROC')
        })
      }
      else
        leafletProxy("map") %>% clearImages()
    }
    
    observeEvent(input$models.map, {
      if(!models.run) {
        return()
      }
      overlayRaster()
    })
  }
)
