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


# ARUGEMENTS
ncFile  <- # Name of the netcdf file you want to open
latName <- # name of the lat variable in netcdf file 
lonName <- # name of the lon variable in netcdf file 
gridType<- # "center" | "NW_edge". Which do the lat/lon describe of the grid? 
variable<- # the variable assigned to each grid box. Cannot be array. 
average <- # if time dimension is provided, take time average? TRUE | FALSE
sumDims <- # Take the sum over the a dimension. TRUE | FALSE
dimensions <- # which dimensions to sum | mean over using apply(). 1:2 default. 
fileSaveName <- # Name of the file to be saved 
    
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
  
  warning(paste("dx =", dx, "and dy =", dy))
  
  # Get the chosen variable 
  if (average){
    v <- apply(ncvar_get(nc, "NO"), dimensions, mean, na.rm=TRUE) 
  } else if(sumDims){
    v <- apply(ncvar_get(nc, "NO"), dimensions, sum, na.rm=TRUE) 
  }
  

  #Apply lat and lon range masks to all variables
  lonMask <- lon > lonRange[1] & lon < lonRange[2]
  latMask <- lat > latRange[1] & lat < latRange[2]
  
  lat <- lat[latMask]
  lon <- lon[lonMask]
  
  v <- v[lonMask, latMask]
  
  # Create the polygons with attributes
  
  polyCount <- 0 # Keep track of how many urban polygons we have
  polyList  <- list() # list to store polygon coordinate segments
  
  # Need to store value, create an array that is dim1 x dim2 long
  uniquePoints <- length(lon) * length(lat)
  value        <- rep(NA, uniquePoints)
  
  for (i in 1:length(lat)){
    for (j in 1:length(lon)){
     
      # Keep track of which polygon we are on 
      polyCount <- polyCount + 1 
      value[polyCount] <- v[j,i]
      
      if(gridType=="center"){
      
        # East-West extent    
        W <- lon[j] - dx/2 # left edge
        E <- W + dx        # right edge
        
        # North-South extent 
        N <- lat[i] + dy/2 # top edge
        S <- N-dy          # bottom edge 
        
      }
      # TODO: Create corner
      if(gridType=="NW_edge"){
        stop("This edge grid functionality not developed yet")
      }
      
      
      # Create coords of box starting in NW corner, going clockwise and 
      # ending in the NEW corner
      plons <- c( W, E, E, W, W)
      plats <- c( N, N, S, S, N)
      
      coords <- cbind(plons, plats)
      
      Sr1 = Polygon(coords)
      
      # Store this object in a list 
      Srs1 = Polygons(list(Sr1), paste0("s",polyCount))
      
      polyList[[polyCount]] <- Srs1 
      
      
    }
  }
  if(!polyCount == uniquePoints){
    stop("Something is wrong with grid logic. Data points and polys don't match")
  }
  
  # Length of polylist should be the same as number of TRUE in urbanMask
  sp <- SpatialPolygons(polyList, 1:polyCount)
  
  # Create the dataframe associated with these polygons
  df <- data.frame(value)
  row.names(df) <- paste0("s",row.names(df))
  spdf <- SpatialPolygonsDataFrame(sp, data=df)
  return(spdf)
  
  
}
  






# Save a figure showing urban polygons and top 10% of NOx emitters based on NEI
pdf(file = "figures/NOXRegines.pdf",
    width=10,
    height=6)
map("state")
plot(Top10PercentNOX, add=TRUE, col="red")

# Save the data 
