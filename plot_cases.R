library(openxlsx)
library(stringr)
library(maps)
library(sp)
library(maptools)
library(gdata)
library(raster)

setwd("~/BIOL453/project/")
data <- read.xlsx("Source_data_for_CFR_vaccine_map - Sheet1.xlsx")
data$Lat = as.numeric(data$Lat)
data$Long = as.numeric(data$Long)
# Remove citation/descriptive data
data <- data[-c(12:14)]

# Get unique impact scales
unique(data$Impact.Scale)
data[which(data$Impact.Scale == "Isolated "),11] <- "Isolated"
data[which(data$Impact.Scale == "Seconday "),11] <- "Secondary"

# Get unique category and outbreak
unique(data$Category)
unique(data$Outbreak)

data[which(data$Outbreak == "Measles\n"),2] <- "Measles"
data[which(data$Outbreak == "Diphtheria"),2] <- "Diptheria"
data[which(data$Outbreak == "Typhoid Fever\n"),2] <- "Typhoid Fever"
data[grepl("Polio*",data$Outbreak),2] <- "Polio"
data[which(data$Outbreak == "Mumps\n"),2] <- "Mumps"
data[which(data$Outbreak == "Whooping Cough\n"),2] <- "Whooping Cough"
data[grepl("Measles*",data$Outbreak),2] <- "Measles"
data[which(data$Outbreak == "Annoucement"),2] <- "Announcement"
data[which(data$Outbreak == "Announcement "),2] <- "Announcement"

unique(data$Continent)
data[which(data$Continent == "Europe "),4] <- "Europe"
data[which(data$Continent == "Asia "),4] <- "Asia"

countries <- strsplit(data$Location,"\\(")
onlyCountry <- vector(length=length(data$Location))
for (i in 1:length(countries))
  onlyCountry[i] <- countries[i][[1]][1]
dataCountry <- str_trim(onlyCountry)

dataCountry[which(dataCountry == "Cote d\'Ivoire")] <- "Ivory Coast"
dataCountry[which(dataCountry == "Côte d’Ivoire")] <- "Ivory Coast"
dataCountry[884] <- "U.S." # New York
dataCountry[709] <- "U.K." # Londond

data$Country <- dataCountry
data <- data[c(1:2,12,3:11)]

data$Year <- as.numeric(data$Year)
unique(data$Year)
data[which(data$Year=="2101"),9] <- "2011"
data[which(data$Year=="214"),9] <- "2014"

data[617,7] <- "-3.16017"
data[832,6] <- "44.7300"

data$Lat <- as.numeric(data$Lat)
data$Long <- as.numeric(data$Long)

# Fix missings
data[which(data$Country=="Kyrgyzsten"),3] <- "Kyrgyzstan"
data[which(data$Country=="Federated States of Micronesia"),3] <- "Micronesia"
data[which(data$Country=="Losotho"),3] <- "Lesotho"
data[which(data$Country=="Bosnia"),3] <- "Bosnia and Herzegovina"
data[which(data$Country=="DR Congo"),3] <- "Democratic Republic of the Congo"
data[which(data$Country=="U.K."),3] <- "UK"
data[which(data$Country=="England"),3] <- "UK"
data[which(data$Country=="U.S."),3] <- "USA"

world <- map(database = "world",regions=".",fill = T,plot=F)
IDs <- world$names
world.map <- map2SpatialPolygons(world,IDs)

data.virus <- data[-c(min(which(data$Outbreak=="Announcement" | 
                                  data$Outbreak=="Violence")):nrow(data)),]
data.virus.map <- data.virus[-which(data$Country == "Gibraltar"),]
casesByCountryYear <- matrix(nrow=length(unique(data.virus.map$Country)),
                             ncol=length(unique(data.virus.map$Year)))
row.names(casesByCountryYear) <- unique(data.virus.map$Country)
colnames(casesByCountryYear) <- sort(unique(data.virus.map$Year))
for (i in 1:nrow(casesByCountryYear)) {
  for (j in 1:ncol(casesByCountryYear)) {
    casesByCountryYear[i,j] <- sum(data.virus.map[which(
      data.virus.map$Country == row.names(casesByCountryYear)[i] 
      & data.virus.map$Year == colnames(casesByCountryYear)[j]),10])
  }
}
row.names(casesByCountryYear)[which(!row.names(casesByCountryYear) %in% world$names)]

included <- c()
for (country in 1:nrow(casesByCountryYear))
  included <- c(included,c(which(world$names %in% row.names(casesByCountryYear)[country])))
included <- sort(unique(included))

excluded <- c()
for (country in 1:length(world$names))
  if(!(world$names[country] %in% row.names(casesByCountryYear)))
    excluded <- c(excluded,country)
excluded <- sort(unique(excluded))

IDs <- c(world$names[included],world$names[excluded])

exceptions <- row.names(casesByCountryYear)[
  which(!(row.names(casesByCountryYear) %in% world$names[included]))]

cases.year <- data.frame(IDs,0,0,0,0,0,0,0,0,0,stringsAsFactors = F)
colnames(cases.year) <- c("IDs","count.2008","count.2009","count.2010",
                          "count.2011","count.2012","count.2013",
                          "count.2014","count.2015","count.2016")

IDs <- IDs[-max(which(IDs=="Vatican"))]
IDs <- IDs[-max(which(IDs=="San Marino"))]
cases.year <- cases.year[-max(which(cases.year$IDs=="Vatican")),]
cases.year <- cases.year[-max(which(cases.year$IDs=="San Marino")),]
rownames(cases.year)<-IDs

# General case
for (j in 2:ncol(cases.year)) {
  for (i in 1:nrow(cases.year)){
    if(cases.year[i,1] %in% row.names(casesByCountryYear)) {
      cases.year[i,j] <- casesByCountryYear[
        which(cases.year[i,1]==row.names(casesByCountryYear)),j-1]
    }
    else
      cases.year[i,j] <- NA
  }
}
# Exceptions
for (j in 2:ncol(cases.year)) {
  for (country in exceptions) {
    matchText <- paste0(country,":")
    cases.year[min(which(startsWith(cases.year$IDs,matchText))),j] <- 
      casesByCountryYear[which(row.names(casesByCountryYear)==country),j-1]
  }
}
worldMapEachYear = SpatialPolygonsDataFrame(world.map, cases.year)
spplot(worldMapEachYear, 'count.2008', main="Cases in 2008", col.regions=rev(heat.colors(256)))
spplot(worldMapEachYear, 'count.2009', main="Cases in 2009", col.regions=rev(heat.colors(256)))
spplot(worldMapEachYear, 'count.2010', main="Cases in 2010", col.regions=rev(heat.colors(256)))
spplot(worldMapEachYear, 'count.2011', main="Cases in 2011", col.regions=rev(heat.colors(256)))
spplot(worldMapEachYear, 'count.2012', main="Cases in 2012", col.regions=rev(heat.colors(256)))
spplot(worldMapEachYear, 'count.2013', main="Cases in 2013", col.regions=rev(heat.colors(256)))
spplot(worldMapEachYear, 'count.2014', main="Cases in 2014", col.regions=rev(heat.colors(256)))
spplot(worldMapEachYear, 'count.2015', main="Cases in 2015", col.regions=rev(heat.colors(256)))


# Raster plots

cell_length = 10
nbin_lat = 180 / cell_length
nbin_long = 360 / cell_length
latLongMat.2010 <- matrix(nrow = nbin_lat, ncol = nbin_long, 0) # Empty lat/long matrix
#rownames(latLongMat.2010) <- paste0("lowerBound",-90:89)
#colnames(latLongMat.2010) <- paste0("lowerBound",-180:179)
data.2010 <- subset(data,Year==2010) # Only 2010 data
for (i in 1:nrow(data.2010)) { # Each incident/occurence
  binLat <- floor((data.2010$Lat[i] + 91)/cell_length) # round down to nearest lat and adjustment for correct bin
  binLong <- floor((data.2010$Long[i] + 181)/cell_length) # same as above but for long
  latLongMat.2010[binLat,binLong] <- latLongMat.2010[binLat,binLong] + data.2010[i,10]
}

latLongMat.2010 = ifelse(latLongMat.2010 == 0, NA, latLongMat.2010)
test <- raster(latLongMat.2010)
bb <- extent(-180, 180, -90, 90)
extent(test) <- bb
test <- setExtent(test,bb,keepres = TRUE)
image(test)
projection(test)<- "+proj=longlat +datum=WGS84"
plot(test)
map('world', add=T)

