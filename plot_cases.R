library(openxlsx)
library(stringr)
library(maps)
library(sp)
library(maptools)
library(gdata)
library(raster)
require(dismo)# dismo has the SDM analyses we"ll need
# http://www.molecularecologist.com/2013/04/species-distribution-models-in-r/

setwd("~/BIOL453/project/")
data <- read.xlsx("Source_data_for_CFR_vaccine_map - Sheet1.xlsx")
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

cell_length = 1
nbin_lat = 180 / cell_length
nbin_long = 360 / cell_length
latLongMat.2010 <- matrix(nrow = nbin_lat, ncol = nbin_long, 0) # Empty lat/long matrix
# rownames(latLongMat.2010) <- paste0("lowerBound",-90:89)
# colnames(latLongMat.2010) <- paste0("lowerBound",-180:179)
data.2010 <- subset(data,Year==2010) # Only 2010 data
for (i in 1:nrow(data.2010)) { # Each incident/occurence
  binLat <- floor((abs(data.2010$Lat[i] - 91))/cell_length) # round down to nearest lat and adjustment for correct bin
  binLong <- floor((data.2010$Long[i] + 181)/cell_length) # same as above but for long
  latLongMat.2010[binLat,binLong] <- latLongMat.2010[binLat,binLong] + data.2010[i,10]
}
#latLongMat.2010 = ifelse(latLongMat.2010 == 0, NA, latLongMat.2010)
raster.2010 <- raster(latLongMat.2010)
bb <- extent(-180, 180, -90, 90)
extent(raster.2010) <- bb
raster.2010 <- setExtent(raster.2010,bb,keepres=F)
res(raster.2010)<- 1
# image(raster.2010)
# projection(raster.2010)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# plot(raster.2010)
# map('world', add=T)


latLongMat.2008 <- matrix(nrow = nbin_lat, ncol = nbin_long, 0) # Empty lat/long matrix
# rownames(latLongMat.2008) <- paste0("lowerBound",-90:89)
# colnames(latLongMat.2008) <- paste0("lowerBound",-180:179)
data.2008 <- subset(data,Year==2008) # Only 2008 data
for (i in 1:nrow(data.2008)) { # Each incident/occurence
  binLat <- floor((abs(data.2008$Lat[i] - 91))/cell_length) # round down to nearest lat and adjustment for correct bin
  binLong <- floor((data.2008$Long[i] + 181)/cell_length) # same as above but for long
  latLongMat.2008[binLat,binLong] <- latLongMat.2008[binLat,binLong] + data.2008[i,10]
}
raster.2008 <- raster(latLongMat.2008)
bb <- extent(-180, 180, -90, 90)
extent(raster.2008) <- bb
raster.2008 <- setExtent(raster.2008,bb,keepres=F)
res(raster.2008)<- 1
projection(raster.2008) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


latLongMat.2009 <- matrix(nrow = nbin_lat, ncol = nbin_long, 0) # Empty lat/long matrix
# rownames(latLongMat.2009) <- paste0("lowerBound",-90:89)
# colnames(latLongMat.2009) <- paste0("lowerBound",-180:179)
data.2009 <- subset(data,Year==2009) # Only 2009 data
for (i in 1:nrow(data.2009)) { # Each incident/occurence
  binLat <- floor((abs(data.2009$Lat[i] - 91))/cell_length) # round down to nearest lat and adjustment for correct bin
  binLong <- floor((data.2009$Long[i] + 181)/cell_length) # same as above but for long
  latLongMat.2009[binLat,binLong] <- latLongMat.2009[binLat,binLong] + data.2009[i,10]
}
raster.2009 <- raster(latLongMat.2009)
bb <- extent(-180, 180, -90, 90)
extent(raster.2009) <- bb
raster.2009 <- setExtent(raster.2009,bb,keepres=F)
res(raster.2009)<- 1
projection(raster.2009) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


latLongMat.2011 <- matrix(nrow = nbin_lat, ncol = nbin_long, 0) # Empty lat/long matrix
# rownames(latLongMat.2011) <- paste0("lowerBound",-90:89)
# colnames(latLongMat.2011) <- paste0("lowerBound",-180:179)
data.2011 <- subset(data,Year==2011) # Only 2011 data
for (i in 1:nrow(data.2011)) { # Each incident/occurence
  binLat <- floor((abs(data.2011$Lat[i] - 91))/cell_length) # round down to nearest lat and adjustment for correct bin
  binLong <- floor((data.2011$Long[i] + 181)/cell_length) # same as above but for long
  latLongMat.2011[binLat,binLong] <- latLongMat.2011[binLat,binLong] + data.2011[i,10]
}
raster.2011 <- raster(latLongMat.2011)
bb <- extent(-180, 180, -90, 90)
extent(raster.2011) <- bb
raster.2011 <- setExtent(raster.2011,bb,keepres=F)
res(raster.2011)<- 1
projection(raster.2011) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


latLongMat.2012 <- matrix(nrow = nbin_lat, ncol = nbin_long, 0) # Empty lat/long matrix
# rownames(latLongMat.2012) <- paste0("lowerBound",-90:89)
# colnames(latLongMat.2012) <- paste0("lowerBound",-180:179)
data.2012 <- subset(data,Year==2012) # Only 2012 data
for (i in 1:nrow(data.2012)) { # Each incident/occurence
  binLat <- floor((abs(data.2012$Lat[i] - 91))/cell_length) # round down to nearest lat and adjustment for correct bin
  binLong <- floor((data.2012$Long[i] + 181)/cell_length) # same as above but for long
  latLongMat.2012[binLat,binLong] <- latLongMat.2012[binLat,binLong] + data.2012[i,10]
}
raster.2012 <- raster(latLongMat.2012)
bb <- extent(-180, 180, -90, 90)
extent(raster.2012) <- bb
raster.2012 <- setExtent(raster.2012,bb,keepres=F)
res(raster.2012)<- 1
projection(raster.2012) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


latLongMat.2013 <- matrix(nrow = nbin_lat, ncol = nbin_long, 0) # Empty lat/long matrix
# rownames(latLongMat.2013) <- paste0("lowerBound",-90:89)
# colnames(latLongMat.2013) <- paste0("lowerBound",-180:179)
data.2013 <- subset(data,Year==2013) # Only 2013 data
for (i in 1:nrow(data.2013)) { # Each incident/occurence
  binLat <- floor((abs(data.2013$Lat[i] - 91))/cell_length) # round down to nearest lat and adjustment for correct bin
  binLong <- floor((data.2013$Long[i] + 181)/cell_length) # same as above but for long
  latLongMat.2013[binLat,binLong] <- latLongMat.2013[binLat,binLong] + data.2013[i,10]
}
raster.2013 <- raster(latLongMat.2013)
bb <- extent(-180, 180, -90, 90)
extent(raster.2013) <- bb
raster.2013 <- setExtent(raster.2013,bb,keepres=F)
res(raster.2013)<- 1
projection(raster.2013) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


latLongMat.2014 <- matrix(nrow = nbin_lat, ncol = nbin_long, 0) # Empty lat/long matrix
# rownames(latLongMat.2014) <- paste0("lowerBound",-90:89)
# colnames(latLongMat.2014) <- paste0("lowerBound",-180:179)
data.2014 <- subset(data,Year==2014) # Only 2014 data
for (i in 1:nrow(data.2014)) { # Each incident/occurence
  binLat <- floor((abs(data.2014$Lat[i] - 91))/cell_length) # round down to nearest lat and adjustment for correct bin
  binLong <- floor((data.2014$Long[i] + 181)/cell_length) # same as above but for long
  latLongMat.2014[binLat,binLong] <- latLongMat.2014[binLat,binLong] + data.2014[i,10]
}
raster.2014 <- raster(latLongMat.2014)
bb <- extent(-180, 180, -90, 90)
extent(raster.2014) <- bb
raster.2014 <- setExtent(raster.2014,bb,keepres=F)
res(raster.2014)<- 1
projection(raster.2014) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


latLongMat.2015 <- matrix(nrow = nbin_lat, ncol = nbin_long, 0) # Empty lat/long matrix
# rownames(latLongMat.2015) <- paste0("lowerBound",-90:89)
# colnames(latLongMat.2015) <- paste0("lowerBound",-180:179)
data.2015 <- subset(data,Year==2015) # Only 2015 data
for (i in 1:nrow(data.2015)) { # Each incident/occurence
  binLat <- floor((abs(data.2015$Lat[i] - 91))/cell_length) # round down to nearest lat and adjustment for correct bin
  binLong <- floor((data.2015$Long[i] + 181)/cell_length) # same as above but for long
  latLongMat.2015[binLat,binLong] <- latLongMat.2015[binLat,binLong] + data.2015[i,10]
}
raster.2015 <- raster(latLongMat.2015)
bb <- extent(-180, 180, -90, 90)
extent(raster.2015) <- bb
raster.2015 <- setExtent(raster.2015,bb,keepres=F)
res(raster.2015)<- 1
projection(raster.2015) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# maxent(raster.2010,data.2010[,7:6])

# https://r-forge.r-project.org/scm/viewvc.php/pkg/dismo/R/maxent.R?view=markup&root=dismo


jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
# checking if maxent can be run (normally not part of your script)
if (file.exists(jar) & require(rJava)) {
  # get predictor variables
  fnames <- list.files(path=paste(system.file(package="dismo"), '/ex', sep=''),
                       pattern='grd', full.names=TRUE )
  predictors <- stack(fnames)
  #plot(predictors)
  # file with presence points
  occurence <- paste(system.file(package="dismo"), '/ex/bradypus.csv', sep='')
  occ <- read.table(occurence, header=TRUE, sep=',')[,-1]
  # witholding a 20% sample for testing
  fold <- kfold(occ, k=5)
  occtest <- occ[fold == 1, ]
  occtrain <- occ[fold != 1, ]
  # fit model, biome is a categorical variable
  me <- maxent(predictors, occtrain, factors='biome')
  # see the maxent results in a browser:
  # me
  # use "args"
  # me2 <- maxent(predictors, occtrain, factors='biome', args=c("-J", "-P"))
  # plot showing importance of each variable
  plot(me)
  # response curves
  # response(me)
  # predict to entire dataset
  r <- predict(me, predictors)
  # with some options:
  # r <- predict(me, predictors, args=c("outputformat=raw"), progress='text',
  # filename='maxent_prediction.grd')
  plot(r)
  points(occ)
  #testing
  # background data
  bg <- randomPoints(predictors, 1000)
  #simplest way to use 'evaluate'
  e1 <- evaluate(me, p=occtest, a=bg, x=predictors)
  # alternative 1
  # extract values
  pvtest <- data.frame(extract(predictors, occtest))
  avtest <- data.frame(extract(predictors, bg))
  e2 <- evaluate(me, p=pvtest, a=avtest)
  # alternative 2
  # predict to testing points
  testp <- predict(me, pvtest)
  head(testp)
  testa <- predict(me, avtest)
  e3 <- evaluate(p=testp, a=testa)
  e3
  threshold(e3)
  plot(e3, 'ROC')
}