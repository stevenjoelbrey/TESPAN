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

# data URL: http://wiki.seas.harvard.edu/geos-chem/index.php/Olson_land_map

################################################################################
# NOTE: WATCH OUT HOW THESE LABELS START AT O!!!
################################################################################

# Open the NEI netcdf file 
ncFile <- paste0("landCoverData/Olson_2001_Land_Map.025x025.generic.nc")
nc     <- nc_open(ncFile)

cover <- ncvar_get(nc, "OLSON") 
# max = 72
# min = 0
# NOTE: These are indeed grid centers 
lat    <- ncvar_get(nc, "lat") 
lon    <- ncvar_get(nc, "lon") 







