# exploreLandCover.R 

# This script will be used for figuring out what different land-cover data has 
# to offer. Usefull functions may be developed here and implemented later on. 

# Load required libraries
library(sp)
library(rgdal)
library(rgeos)
library(stringr)
library(R.utils)
library(maps)
library(ncdf4)


################################################################################
# Explore Olson_2001 (used by GEOS-Chem)
################################################################################


# Open the NEI netcdf file 
ncFile <- paste0("landCoverData/Olson_2001_Land_Map.025x025.generic.nc")
nc     <- nc_open(ncFile)

cover <- ncvar_get(nc, "OLSON") 
lat    <- ncvar_get(nc, "lat") 
lon    <- ncvar_get(nc, "lon") 
