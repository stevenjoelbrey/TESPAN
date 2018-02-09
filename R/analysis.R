# analysis.R 

#-------------------------------DESCRIPTION-----------------------------------#
# This script reads in TES retrieval smoke summary data csv and plots the 
# locations of retrivals for a desired time period. The retrievals are color 
# coded by whether they were taken on a day when a smoke plume was present 
# or not present. 

# The data being used to make the plots in this figure were created using the
# script R/maskTESWithHMS.R 

version   <- "version_3"
alpha     <- 1
sym       <- 19
pointSize <- 2.5

# TODO: Getting rid of 2010. Make 4x4 with options for point sizes. 
# TODO: Also send as individual. 
# TODO: Use new smoke plumes. 

library(R.matlab)
library(maps)
library(stringr)
library(sp)
library(rgdal)
library(rgeos)
library(stringr)
library(R.utils)

################################################################################
# Plot the retrievals and colorcode them based on if they are in smoke 
################################################################################
if(version == "version_3"){
  dataFile <- paste0("DataOut/TES_SmokeSumary_update.csv")
  df <- read.csv(file=dataFile, stringsAsFactors=FALSE)
  
  # For this version we subset the rows by DOF >= 0.60
  rowMask <- df$DOF >= 0.6
  df <- df[rowMask,]
  
}else{
  dataFile <- paste0("DataOut/TES_SmokeSumarry_wNOX_", version, ".csv")
  df <- read.csv(file=dataFile, stringsAsFactors=FALSE)
  print("Using version_2 or version_1")
}

dates <- df$date
yearsString <- str_sub(dates, 1,4)
monthString <- str_sub(dates,5,6)
uniqueYears <- unique(yearsString)

# Plot the July inter-annual variability 
for(year in 2006:2009){
  print(year)
  # Mask out all rows that occur in month "07" and loop year 
  timeMask <- (as.character(year) == yearsString) & (monthString == "07")
  
  df_subset <- df[timeMask,]
 
  # Remove points from the maps that are not within one of these boxes: 
  # 30N-50N, 125W – 70 W OR 50N-70N, 130W-60W.
  lat <- df_subset$Latitude
  lon <- df_subset$Longitude
  boxOne <- (lat >= 30) & (lat <= 50) & (lon >= -125) & (lon <= -70)
  boxTwo <- (lat >= 50) & (lat <= 70) & (lon >= -130) & (lon <= -60)
  
  # Want to keep points where either is true
  boxMask <- boxOne | boxTwo
  df_subset <- df_subset[boxMask,]
  
  # Color based on in smoke | not | or not available, defualt all to black, to
  # be changed when evidence warrents. 
  COLOR <- rep("murph", dim(df_subset)[1])
  
  NAMask      <- is.na(df_subset$inSmoke)
  COLOR[NAMask] <- "black"
  
  # so no match on TRUE or FALSE, now NA is signalled by -1 
  df_subset$inSmoke[NAMask] <- -1 
  
  # change color array based on in smoke or not. NA taken care of with black
  COLOR[df_subset$inSmoke == 1] <- "red"
  COLOR[df_subset$inSmoke == 0] <- "lightblue" #"lightblue"
  
  fileName <- paste0("figures/", version, "/TESHMSOverlay_",year,"-","07",".pdf")
  pdf(file=fileName, width=10, height=10)
  par(mfrow=c(1,1), mai = c(0, 0, 0, 0), mar=c(1.5,1,1,1.5))
  
  # Draw this years map panel and color coded points 
  map(database = "world", ylim = c(30,71), xlim = c(-131,-59), 
      mar = c(1.5,1,1,2.5), myborder = 0.01, lwd=2)
  
  COLOR <- adjustcolor(COLOR, alpha.f = alpha)
  
  points(df_subset$Longitude, df_subset$Latitude, col=COLOR, 
         pch=sym,
         lwd=2,
         cex=pointSize)
  
  # Draw outlines of box one & 2 as merged polygon
  lineWidth <- 3
  # Horizontal lines
  segments(x0=-125, y0=30, x1 = -70, y1 = 30, lwd=lineWidth, col="blue")
  segments(x0=-130, y0=50, x1 = -125, y1 = 50, lwd=lineWidth, col="blue")
  segments(x0=-70, y0=50, x1 = -65, y1 = 50, lwd=lineWidth, col="blue")
  segments(x0=-130, y0=70, x1 = -65, y1 = 70, lwd=lineWidth, col="blue")
  # Vertical lines
  segments(x0=-125, y0=30, x1 = -125, y1 = 50, lwd=lineWidth, col="blue")
  segments(x0=-70, y0=30, x1 = -70, y1 = 50, lwd=lineWidth, col="blue")
  segments(x0=-130, y0=50, x1 = -130, y1 = 70, lwd=lineWidth, col="blue")
  segments(x0=-65, y0=50, x1 = -65, y1 = 70, lwd=lineWidth, col="blue") # changed 
  
  
  # Add a basic title
  title(paste("July", year), cex.main=3, line=1)
  
  legendCol <- c("red", "lightblue", "black")
  
  # Add a legend at the bottom 
  legend(x = "bottom",
         horiz=TRUE,
         xpd=TRUE,
         inset=c(0,-0.08),
         legend=c("In Smoke","Not in Smoke", "No data"),
         text.width=c(10, 12, 14),
         col=adjustcolor(legendCol, alpha),
         border=legendCol,
         pch=sym,
         cex=1.7,
         pt.cex = 2.7,
         bty="n")
  
  dev.off()
  
}

# Add a legend at the bottom 
# plot(1, type = "n", axes=FALSE, xlab="", ylab="")
# legend(x = "top",
#        inset = 0,
#        horiz=TRUE,
#        xpd=TRUE,
#        #inset=c(0,0.4),
#        legend=c("In Smoke","Not in Smoke", "No data"),
#        col=c("red", "midnightblue", "black"),
#        pch=19,
#        cex=1,
#        bty="n")

# Turn off multipanel plot 
#dev.off()

################################################################################
# Make nice multipanel figure as specified by Emily. 
################################################################################


# # Plot the May-sep  variability for 2006
# for(m in 5:9){
#   
#   month = paste0("0",m)
#   
#   timeMask <- "2006" == yearsString & monthString == month
#   
#   df_subset <- df[timeMask,]
#   
#   # Color based on in smoke | not | or not available
#   col <- rep("murph", dim(df_subset)[1])
#   
#   NAMask      <- is.na(df_subset$inSmoke)
#   col[NAMask] <- "black"
#   
#   # so no match on TRUE or FALSE, now NA is signalled by -1 
#   df_subset$inSmoke[NAMask] <- -1 
#   
#   col[df_subset$inSmoke == 1]  <- "red"
#   col[df_subset$inSmoke == 0] <- "lightblue"
#   
#   
#   fileName <- paste0("figures/", version, "/TESHMSOverlay_",2006,"_",month,".pdf")
#   pdf(file=fileName, width=10, height=7)
#   
#   par( mar=c(0,2,1,4)  )
#   
#   map(database = "world", ylim = c(16,75), xlim = c(-190,-65))
#   points(df_subset$Longitude, df_subset$Latitude, col=col, pch=19)
#   
#   
#   legend("left",
#          horiz=FALSE,
#          xpd=TRUE,
#          inset=c(0,0.4),
#          legend=c("In Smoke","Not in Smoke", "No data"),
#          col=c("red", "lightblue", "black"),
#          pch=19,
#          cex=1.4,
#          bty="n")
#   
#   title(paste(month,2006, "TES retrievals"))
#   
#   dev.off()
#   
# }