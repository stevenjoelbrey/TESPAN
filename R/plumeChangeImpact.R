# plumeChangeImpact.R

# The first time I checked to see if these plumes overlapped smoke plumes I did
# not use the merged smoke plumes that were created for smokeSource work. In the
# smokeSource work I did additional quality control on high latitude plumes. The
# new versions of this work reflect that change in quality. 
# The goal of this script is to understand how big a change there is when using
# these new plumes


oldFile <- "DataOut/TES_SmokeSumarry.csv"
df_old <- read.csv(file=oldFile, stringsAsFactors=FALSE)

newFile <- "DataOut/TES_SmokeSumarry_wNOX_version_1.csv"
df_new  <- read.csv(file=newFile, stringsAsFactors=FALSE)


# Do the locations match for both methods?
lonMask <- df_old$Longitude == df_new$Longitude
if(sum(lonMask) == length(lonMask)){
  print("Each retrieval location lines up. Rows can be compared.")
} else{
  stop("Rows are not aligned. Stopping analysis.")
}

# I want to see the total number of in plume retrievals for each data type
oldSum <- sum(df_old$inSmoke, na.rm=TRUE)
newSum <- sum(df_new$inSmoke, na.rm=TRUE)

print(paste("The total TES retrievals in smoke plume for old plumes=", oldSum))
print(paste("The total TES retrievals in smoke plume for new plumes=", newSum))
d <- oldSum - newSum
print(paste("There are now", d, "fewer overlaps out of", length(lonMask)))
print(paste(d/oldSum*100, "% of old plume overlap were from plumes that have been removed."))


