# analysis.R 

#-------------------------------DESCRIPTION-----------------------------------#
# This script reads in TES_SmokeSummary.csv data and plots the locations of 
# retrivals for a desired time period. The retrievals are color coded by
# whether they were taken on a day when a smoke plume was present or not 
# present. 

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
df <- read.csv(file="DataOut/TES_SmokeSumarry.csv", stringsAsFactors=FALSE)

dates <- df$date
yearsString <- str_sub(dates, 1,4)
monthString <- str_sub(dates,5,6)
uniqueYears <- unique(yearsString)


# Set pdf parameters for multipanel figure
fileName <- paste0("figures/TESHMSOverlay_2006-2010.pdf")
pdf(file=fileName, width=15, height=10)
par( mfrow=c(2,3), mai = c(0, 0, 0, 0), mar=c(2,1,1,1.5))


# Plot the July inter-annual variability 
for(year in uniqueYears){
  
  # Mask out all rows that occur in month "07" and loop year 
  timeMask <- year == yearsString & monthString == "07"
  
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
  col <- rep("black", dim(df_subset)[1])
  
  # change color array based on in smoke or not. NA taken care of with black
  col[which(df_subset$inSmoke == TRUE)]  <- "red"
  col[which(df_subset$inSmoke == FALSE)] <- "lightblue"
  
  # Draw this years map panel and color coded points 
  map(database = "world", ylim = c(30,71), xlim = c(-131,-59), 
      mar = c(1.5,1,1,2.5), myborder = 0.01, lwd=2)
  points(df_subset$Longitude, df_subset$Latitude, col=col, pch=19)
  
  # Draw outlines of box one & 2 as merged polygon
  lineWidth <- 3
  # Horizontal lines
  segments(x0=-125, y0=30, x1 = -70, y1 = 30, lwd=lineWidth, col="blue")
  segments(x0=-130, y0=50, x1 = -125, y1 = 50, lwd=lineWidth, col="blue")
  segments(x0=-70, y0=50, x1 = -60, y1 = 50, lwd=lineWidth, col="blue")
  segments(x0=-130, y0=70, x1 = -60, y1 = 70, lwd=lineWidth, col="blue")
  # Vertical lines
  segments(x0=-125, y0=30, x1 = -125, y1 = 50, lwd=lineWidth, col="blue")
  segments(x0=-70, y0=30, x1 = -70, y1 = 50, lwd=lineWidth, col="blue")
  segments(x0=-130, y0=50, x1 = -130, y1 = 70, lwd=lineWidth, col="blue")
  segments(x0=-60, y0=50, x1 = -60, y1 = 70, lwd=lineWidth, col="blue")
  
  
  # Add a basic title
  title(paste("July",year), cex.main=3, line=1)
  
}
plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')

legend("center",
       horiz=FALSE,
       xpd=TRUE,
       #inset=c(0,0.4),
       legend=c("In Smoke","Not in Smoke", "No data"),
       col=c("red", "lightblue", "black"),
       pch=19,
       cex=3.6,
       bty="n")

# Turn off multipanel plot 
dev.off()

################################################################################
# Make nice multipanel figure as specified by Emily. 
################################################################################


# Plot the May-sep  variability for 2006
for(m in 5:9){
  
  month = paste0("0",m)
  
  timeMask <- "2006" == yearsString & monthString == month
  
  df_subset <- df[timeMask,]
  
  # Color based on in smoke | not | or not available
  col <- rep("black", dim(df_subset)[1])
  
  col[which(df_subset$inSmoke == TRUE)]  <- "red"
  col[which(df_subset$inSmoke == FALSE)] <- "lightblue"
  
  
  fileName <- paste0("figures/TESHMSOverlay_",2006,"_",month,".pdf")
  pdf(file=fileName, width=10, height=7)
  
  par( mar=c(0,2,1,4)  )
  
  map(database = "world", ylim = c(16,75), xlim = c(-190,-60))
  points(df_subset$Longitude, df_subset$Latitude, col=col, pch=19)
  
  
  legend("left",
         horiz=FALSE,
         xpd=TRUE,
         inset=c(0,0.4),
         legend=c("In Smoke","Not in Smoke", "No data"),
         col=c("red", "lightblue", "black"),
         pch=19,
         cex=1.3,
         bty="n")
  
  title(paste(month,2006, "TES retrievals"))
  
  dev.off()
  
}