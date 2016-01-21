# createPolygonFromNetCDF.R 

# TODO: Figure out why spdf is off. 

# This script will be the home of a function that should generally serve to 
# convert gridded netcdf data to R polygons for spatial overlap analysis. 


# Load required libraries when script is sourced
library(sp)
library(rgdal)
library(rgeos)
library(stringr)
library(R.utils)
library(maps)
library(ncdf4)

################################################################################
#  ARUGEMENTS
################################################################################
# ncFile     # Name of the netcdf file you want to open
# latName    # name of the lat variable in netcdf file 
# lonName    # name of the lon variable in netcdf file 
# gridType   # "center" | "NW_edge". Which do the lat/lon describe of the grid? 
# lonRange   # Array with c(west boundry, east boundry) of supplied nc data 
# latRange   # Array with c(south boundry, north boundry) of supplied nc data 
# variable   # the variable assigned to each grid box. Cannot be array. 
# average    # if time dimension is provided, take time average? TRUE | FALSE
# sumDims    # Take the sum over the a dimension. TRUE | FALSE
# dimensions # which dimensions to sum | mean over using apply(). 1:2 default. 
# fileSaveName # Name of the file to be saved 
################################################################################

createPolygonFromNetCDF <- function(ncFile="EPAData/NEI08_2010_1x1_Jul_wkday_regrid.nc",
                                    latName="lat",
                                    lonName="lon",
                                    gridType="center",
                                    lonRange=c(-126,-60),
                                    latRange=c(15,51),
                                    variable="NO",
                                    average=FALSE,
                                    sumDims=TRUE,
                                    dimensions=1:2,
                                    fileSaveName="output.RData"
                                    ){
  
  # Open the NEI netcdf file 
  nc     <- nc_open(ncFile)
  lat    <- ncvar_get(nc, latName) 
  lon    <- ncvar_get(nc, lonName) 
  
  # Get the dy and dx for building the grid
  dx <- max(diff(lon))
  dy <- max(diff(lat))
  
  # Show the user what the calculated dx and dy values are 
  warning(paste("dx =", dx, "and dy =", dy))
  
  # Get the chosen variable 
  if (average){
    v <- apply(ncvar_get(nc, variable), dimensions, mean, na.rm=TRUE) 
  } else if(sumDims){
    v <- apply(ncvar_get(nc, variable), dimensions, sum, na.rm=TRUE) 
  }
  

  #Apply lat and lon range masks to all variables
  lonMask <- lon > lonRange[1] & lon < lonRange[2]
  latMask <- lat > latRange[1] & lat < latRange[2]
  
  lat <- lat[latMask]
  lon <- lon[lonMask]
  
  v <- v[lonMask, latMask]
  
  # Create the polygons with attributes (sp and spdf objects)
  polyCount <- 0 # Keep track of how many urban polygons we have
  polyList  <- list() # list to store polygon coordinate segments
  
  # Need to store value, create an array that is dim1 x dim2 long
  # aka latxlon grid information
  uniquePoints <- length(lon) * length(lat)
  value        <- rep(NA, uniquePoints)
  
  for (i in 1:length(lat)){   # loop through lat
    for (j in 1:length(lon)){ # and for each lat go through each lon
     
      # Keep track of which polygon we are on 
      polyCount <- polyCount + 1 
      value[polyCount] <- v[j,i]
      
      # Create corners based on what type of grid lat lon describe
      if(gridType=="center"){
      
        # East-West extent    
        W <- lon[j] - dx/2 # left edge
        E <- W + dx        # right edge
        
        # North-South extent 
        N <- lat[i] + dy/2 # top edge
        S <- N-dy          # bottom edge 
        
      } else if(gridType=="NW_edge"){
        stop("This edge grid functionality not developed yet")
      }
      
      # Create coords of box starting in NW corner, going clockwise and 
      # ending in the NW corner
      plons <- c( W, E, E, W, W)
      plats <- c( N, N, S, S, N)
      
      coords <- cbind(plons, plats)
      
      # Create a polygon object based on these corners
      Sr1 = Polygon(coords)
      
      # Store this polygon object in a list 
      Srs1 = Polygons(list(Sr1), paste0("s",polyCount))
      
      polyList[[polyCount]] <- Srs1 
      
      
    }
  }
  
  # If the number of datapoints (polyCount) is not equal to product of grid
  # dimensions you have a problem! Throw a stop() and warning message 
  if(!polyCount == uniquePoints){
    stop("Something is wrong with grid logic. Data points and polys don't match")
  }
  
  # Length of polylist should be the same as number of TRUE in urbanMask
  sp <- SpatialPolygons(polyList, 1:polyCount)
  
  # Create the dataframe associated with these polygons
  df <- data.frame(value)
  # Make the data.frame row names match the names of the polygons they are 
  # associated with. This means adding an s to each name. 
  row.names(df) <- paste0("s",row.names(df))
  
  # Create the spatial polygon dataframe and return it to where the function is
  # called 
  spdf <- SpatialPolygonsDataFrame(sp, data=df)
  return(spdf)
  
  
}

################################################################################
# Function testing ground for development. This data will be used for analysis
# being done for Juliet. 
################################################################################

if(FALSE){
# MAKE NOX

# Get NO in US out of NEI
NO <- createPolygonFromNetCDF(ncFile="EPAData/NEI08_2010_1x1_Jul_wkday_regrid.nc",
                              latName="lat",
                              lonName="lon",
                              gridType="center",
                              lonRange=c(-126,-60),
                              latRange=c(15,51),
                              variable="NO",
                              average=FALSE,
                              sumDims=TRUE,
                              dimensions=1:2,
                              fileSaveName="output.RData")

NO2 <- createPolygonFromNetCDF(ncFile="EPAData/NEI08_2010_1x1_Jul_wkday_regrid.nc",
                               latName="lat",
                               lonName="lon",
                               gridType="center",
                               lonRange=c(-126,-60),
                               latRange=c(15,51),
                               variable="NO2",
                               average=FALSE,
                               sumDims=TRUE,
                               dimensions=1:2,
                               fileSaveName="output.RData")

# Combine the NO + NO2 to make NOX. Note that you cannont add objects of class
# spatialPolygonsDataframe
NOXValues <- NO$value + NO2$value

# copy the spatial component of NO2 for NOX
NOX <- NO2

# Give this the combined quantities of NO + NO2 
NOX$value <- NOXValues 

# Save this NOX quantitiy information
save(NOX, file="EPAData/NEI_2008_wkdayNOX_spdf.RData")


}








