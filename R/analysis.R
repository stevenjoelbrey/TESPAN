# analysis.R 

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


# Plot the July inter-annual variability 
for(year in uniqueYears){
  
  timeMask <- year == yearsString & monthString == "07"
  
  df_subset <- df[timeMask,]
  
  # Color based on in smoke | not | or not available
  col <- rep("black", dim(df_subset)[1])
  
  col[which(df_subset$inSmoke == TRUE)]  <- "red"
  col[which(df_subset$inSmoke == FALSE)] <- "lightblue"
  
  
  fileName <- paste0("figures/TESHMSOverlay_",year,".pdf")
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
  
  title(paste("July",year, "TES retrievals"))
  
  dev.off()
  
}

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