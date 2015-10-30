# maskTESWithHMS.R


library(R.matlab)
library(maps)
library(stringr)
library(sp)
library(rgdal)
library(rgeos)
library(stringr)
library(R.utils)


##############################################################################
# Handle TES data, make into useful arrays
##############################################################################
files <- list.files("TESData")
hour <- numeric(0)
dates<- numeric(0)
lon  <- numeric(0)
lat  <- numeric(0)
for(file in files){
  
  List <-  readMat(paste0("TESData/",file))

#   hour <-  append(hour, as.numeric(List[["westNA.ut.hr"]]))
#   dates <- append(dates, as.numeric(List[["westNA.date"]]))
#   lon   <- append(lon, as.numeric(List[["westNA.lons"]]))
#   lat   <- append(lat, as.numeric(List[["westNA.lats"]]))

  hour <-  append(hour, as.numeric(List[["usa.ut.hr"]]))
  dates <- append(dates, as.numeric(List[["usa.date"]]))
  lon   <- append(lon, as.numeric(List[["usa.lons"]]))
  lat   <- append(lat, as.numeric(List[["usa.lats"]]))

}

# The TES points have been read in, see which ones overlap with HMS
# smoke plumes
dateString <- as.character(dates)
TESTime <- as.POSIXct(dateString, format="%Y%m%d", tz="UTC")


##############################################################################
# Handle HMS data 
##############################################################################
  
# Location the HMS GIS smoke files in project 
smokeFiles <- list.files("/Users/sbrey/projects/PM25Ozone/FireData/HMS_smoke/")

# For TESPAN we only care about July
minMonth <- 7
maxMonth <- 7
  
# Only look at days that we have smoke data for. Determined by preference and
# HMS dataset 
smokeDayString <- unique(str_sub(smokeFiles,5,12))
smokeTime <- as.POSIXct(smokeDayString, format="%Y%m%d", tz="UTC")
  
##############################################################################
# Create spatial points dataframe of hybrid monitor locations
##############################################################################
coords    <- cbind(lon, lat)
locations <- SpatialPoints(coords, proj4string=CRS(as.character(NA)))


##############################################################################
# Create array to store information on if TES is in smoke or not 
############################################################################## 
Nretrievals <- length(locations)
inSmoke <- rep(NA, Nretrievals)


################################################################################
# loop through TES retrievals, checking each for intersection with a smoke plume
################################################################################
HMSDataDir <- "/Users/sbrey/projects/PM25Ozone/FireData/HMS_smoke/"
for (i in 1:Nretrievals){
  
  location <- locations[i]
  
  # Build the link to the correct daily HMS file 
  t <- as.POSIXlt(TESTime[i])
  year <- t$year + 1900
  mon <- t$mon+1;if(str_length(mon) < 2){mon <- paste0("0",mon)}
  day <- t$mday; if(str_length(day) < 2){day <- paste0("0",day)}
  
  # Load the GIS smoke data
  layername <- paste0("hms_",year, mon, day)
  print(paste(i, "Layername:", layername))
  try_error <- try(
    GIS <- readOGR(dsn=HMSDataDir, layer=layername, verbose=FALSE)
    , silent = FALSE
  )
  
  # If the GIS data exists carry on
  if(!class(try_error) == "try-error"){
    
    # Is this retrieval even in the boudning box? If not, call it NA
    bbox   <- GIS@bbox
    minLat <- bbox[2,1]
    maxLat <- bbox[2,2]
    minLon <- bbox[1,1]
    maxLon <- bbox[1,2]
    pLat   <- location$lat
    pLon   <- location$lon
    
    inLatRange <- minLat <= pLat & maxLat >= pLat
    inLonRange <- minLon <= pLon & maxLon >= pLon
    
    inBoundingBox <- inLatRange & inLonRange
    
    # If there is a chance that the retrieval overlaps, check to see if it
    # overlaps with a specific smoke polygon
    if(inBoundingBox){
      # What stations overlap with smoke? 
      index <- over(location, GIS)[,1] # We want where it is NOT NA
      HMSmask  <- !(is.na(index))
      
      # Co-located with smoke?
      inSmoke[i] <- HMSmask
    }
    
    # NOTE: If there is no HMS data available | the bounding box does not cover
    # NOTE: the retrieval, this will be NA by defualt. 
    
  } # End of if statement checking for GIS data completeness
  
  print(paste("progress =", round(i/Nretrievals*100,2),"%"))
  
} # End of looping through TES datapoints



################################################################################
# Save the results nicely in a csv file 
################################################################################
df <- data.frame(date=dates, 
                hour=hour, 
                Longitude=lon, 
                Latitude=lat, 
                inSmoke=inSmoke)

write.csv(df, file="DataOut/TES_SmokeSumarry.csv", row.names=FALSE)


################################################################################
# Plot the retrievals and colorcode them based on if they are in smoke 
################################################################################
df <- read.csv(file="DataOut/TES_SmokeSumarry.csv", stringsAsFactors=FALSE)

dates <- df$date
yearsString <- str_sub(dates, 1,4)
uniqueYears <- unique(yearsString)

for(year in uniqueYears){
  
  yearMask <- year == yearsString
  
  df_subset <- df[yearMask,]
  
  # Color based on in smoke | not | or not available
  col <- rep("black", dim(df_subset)[1])
  
  col[which(df_subset$inSmoke == TRUE)]  <- "red"
  col[which(df_subset$inSmoke == FALSE)] <- "lightblue"
  
  
  fileName <- paste0("figures/TESHMSOverlay_",year,".pdf")
  pdf(file=fileName, width=10, height=7)
  
  par( mar=c(0,0,1,4)  )
  
  map(database = "world", ylim = c(20,70), xlim = c(-185,-85))
  points(df_subset$Longitude, df_subset$Latitude, col=col, pch=19)
  
  
  legend("bottomleft",
         xpd=TRUE,
         inset=c(0,0.4),
         legend=c("In Smoke","Not in Smoke", "No data"),
         col=c("red", "lightblue", "black"),
         pch=19,
         cex=1.5)
  
  title(year)
  
  dev.off()
  
}




