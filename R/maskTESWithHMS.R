# maskTESWithHMS.R

#-------------------------------DESCRIPTION-----------------------------------#
# This script is designed to figure out when a TES retrieva  is overlapping a 
# HMS smoke plume polygon. Further, we want to know if the TES retrieval
# is overlapping a high, low, or mid level anthropogenic emissions NOX grid
# Box. 

# Load the required libraries
library(R.matlab)
library(maps)
library(stringr)
library(sp)
library(rgdal)
library(rgeos)
library(stringr)
library(R.utils)

# Creates daily sanity check figures and dumps them in testFigures/ directory
test <- FALSE

# What version of TES data do we want to use? 
version <- "version_1" # "version_1" | "version_2"

##############################################################################
# Handle TES data, make into useful arrays
##############################################################################
files <- list.files(paste0("TESData","/",version))
hour <- numeric(0)
dates<- numeric(0)
lon  <- numeric(0)
lat  <- numeric(0)
soundingID <- character(0)

# Loop through files reading in information of the arrays defined above
for(file in files){
  
  List <-  readMat(paste0("TESData/",version,"/",file))

  hour <-  append(hour, as.numeric(List[["usa.ut.hr"]]))
  dates <- append(dates, as.numeric(List[["usa.date"]]))
  lon   <- append(lon, as.numeric(List[["usa.lons"]]))
  lat   <- append(lat, as.numeric(List[["usa.lats"]]))
  soundingID <- append(soundingID, List[["usa.soundingID"]]) # comes as char 
  
}

# The TES points have been read in, see which ones overlap with HMS
# smoke plumes
dateString <- as.character(dates)
TESTime    <- as.POSIXct(dateString, format="%Y%m%d", tz="UTC")

##############################################################################
# Handle HMS data 
# NOTE: This is where raw plumes were previously used
##############################################################################
  
# Location the HMS GIS smoke files in project 
HMSDataDir <- "/Users/sbrey/sharedProjects/smokeSource/Data/smokeRData/daily/"
smokeFiles <- list.files(HMSDataDir)

# We are going to use the merged files, these underwent the most QC.
mergedMask <- str_detect(smokeFiles, "merged")
smokeFiles <- smokeFiles[mergedMask]

# What are the dates of these files?
smokeDayString <- str_sub(smokeFiles,1,8) # these dates are all unique
smokeTime      <- as.POSIXct(smokeDayString, format="%Y%m%d", tz="UTC")

# NOTE: smokeTime has been plotted and visually inspected for missing time 
# NOTE: periods. There are no significant gaps (missing data) in the time 
# NOTE: series. 
  
##############################################################################
# Handle NOX Spatial data 
##############################################################################
load("EPAData/NEI_2008_wkdayNOX_spdf.RData")
NOXValues <- NOX$value
maxNOX    <- max(NOXValues, na.rm=TRUE)
NOXPercentOfMax <- NOXValues / maxNOX * 100 # to make the NOX number a %

##############################################################################
# Create spatial points dataframe of TES retrieval locations 
##############################################################################
coords    <- cbind(lon, lat)
locations <- SpatialPoints(coords, proj4string=CRS(as.character(NA)))

##############################################################################
# Create array to store information on if TES is in smoke or not
# and an array that tells us what percentile of NOX emissions it is in
############################################################################## 
Nretrievals     <- length(locations)

inSmoke               <- rep(NA, Nretrievals)
NOXEmission           <- rep(NA, Nretrievals)
NOXPercentOfMaxStored <- rep(NA, Nretrievals)
GISExists             <- rep(NA, Nretrievals)

################################################################################
# loop through TES retrievals, checking each for intersection with a smoke plume
################################################################################

#HMSDataDir <- "/Users/sbrey/projects/PM25Ozone/FireData/HMS_smoke/"

for (i in 1:Nretrievals){ #Nretrievals
  
  # Working with a new location (aka retrieval) everytime we step through this 
  # loop 
  location <- locations[i]
  
  # Load the GIS smoke data
  layername <- paste0(dateString[i], "_merged_smoke.RData")
  percentComplete <- round(i/length(lon)*100, 5)
  print(paste(percentComplete, "% complete.  Layername:", layername))
  smokePlumeFile <- paste0(HMSDataDir, layername)
  
  # See if the HMS GIS file exists for this day
  try_error <- try(
     GIS <- get(load(smokePlumeFile))
     , silent = FALSE
  )

  # If the GIS data exists carry on with smoke analysis
  GISExist <- !(class(try_error) == "try-error")
  if(GISExist){
    
    # Record the existance
    GISExists[i] <- TRUE
    
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
      
      # Make sure the CRS is the same
      GIS@proj4string <- location@proj4string
      
      # What stations overlap with smoke? 
      index <- over(location, GIS)
      HMSmask  <- !(is.na(index)) # if index is NA, says false, aka not in smoke
      
      # Co-located with smoke?
      inSmoke[i] <- HMSmask
      
    } 
    
    # NOTE: If there is no HMS data available | the bounding box does not cover
    # NOTE: the retrieval, this will be NA by defualt. 
    # NOTE: insmoke == FALSE corresponds to scope of HMS covers TES location
    # NOTE: but that location is not in smoke
    
  } else {
    
    # There is no GIS data for this date
    GISExists[i] <- FALSE
    
  } # End of if statement checking for GIS data completeness
  
  ################################################################################
  # Now check to see if TES retrieval is overlapping NEI. Report % of max
  # NEI value for wkday july 2008 this retrieval is over 
  ################################################################################
  
  # Find out if location inside bounding box of NOX GIS object 
  bbox   <- NOX@bbox
  minLat <- bbox[2,1]
  maxLat <- bbox[2,2]
  minLon <- bbox[1,1]
  maxLon <- bbox[1,2]
  noxLat   <- location$lat
  noxLon   <- location$lon
  
  # Place a check for errors. pLat only defined when there is GIS data 
  if(!noxLat == pLat & GISExist){
    stop("noLat should be equal to pLat, there is an error")
  }
  
  inLatRange <- minLat <= noxLat & maxLat >= noxLat
  inLonRange <- minLon <= noxLon & maxLon >= noxLon
  
  inBoundingNOXBox <- inLatRange & inLonRange
  
  # if in the bounding NOX box then this TES retrieval is assocaited with some
  # NEI NOX value
  if(inBoundingNOXBox){
    
    NOXList <- over(location, NOX, returnList=TRUE)
    NOXdf   <- NOXList[[1]] # NOTE: Should only ever overlap one NOX box
    PolyID  <- row.names(NOXdf)
    rowNumber <- as.numeric(str_replace(PolyID, "s", "")) # "s" removed _. numeric 
    
    # Store the information of the selected NOX box
    NOXEmission[i] <- NOXValues[rowNumber]
    NOXPercentOfMaxStored[i] <- NOXPercentOfMax[rowNumber]
    
  }
  
  ################################################################################
  # Audit figure! TEST THIS CODE VISUALLY. Make sure results do what you exspect 
  ################################################################################
  # if(test){
  if(FALSE){
    
    # Save a pdf figure of the results for the test simulations
    pdf(file=paste0("testFigures/",i,".pdf"),
        width=12, height=9)
    
    # Plot the base of the map
    map("world", xlim=c(-180,-50), ylim=c(0,80))
   
    # plot the smoke if it exists for this date
    if(!class(try_error) == "try-error"){plot(GIS, add=TRUE, col="red")}
    # Always plot the NOX grid boxes
    plot(NOX, add=TRUE)
    # Shade the NOX box that the retrieval is inside of 
    if(inBoundingNOXBox){plot(NOX[rowNumber,], col="green", add=TRUE)}
    
    # Plot the retrieval location and change shape and color if it is in smoke
    if(is.na(inSmoke[i]) | inSmoke[i] == FALSE){
      # it is not in smoke
      plot(location, add=TRUE, col="blue", lwd=3)
    } else{
      # it is in smoke
      plot(location, add=TRUE, col="purple", lwd=19)
      
    }
    
    # Add titles and labels for debugging purposes
    title(paste("NOX % of max is:",NOXPercentOfMaxStored[i]))
    title(line=0, paste("inSmoke =", inSmoke[i]))
    title(sub = layername)
    title(sub=paste(noxLat, noxLon, hour[i]), line=0)
    title(sub=paste(lat[i], noxLon[i]), line=1)
    # turn off and save the current plot 
    dev.off()
    
  } # End of testing block 

} # End of looping through TES datapoints


################################################################################
# Save the results nicely in a csv file 
################################################################################
df <- data.frame(soundingID=soundingID,
                 date=dates, 
                 hour=hour, 
                 Longitude=lon, 
                 Latitude=lat, 
                 inSmoke=inSmoke,
                 GISExists=GISExists,
                 NOXEmission=NOXEmission,
                 NOXPercentOfMax=NOXPercentOfMaxStored,
                 maxNOX=rep(maxNOX, Nretrievals))

# The file save name needs the date version and direcory. 
writeFile <- paste0("DataOut/TES_SmokeSumarry_wNOX_", version, ".csv")
write.csv(df, file=writeFile, row.names=FALSE)


