###############################################################################
# Objective: Take algamated WCTSS ROV data and remove species obs, 
#            removed duplicate observations at one point,
#            matched dominant/subdominant substrate combo with BType4 (RMSM),
#            exported results as a shapefile and csv
#
# Date: March 13, 2019
###############################################################################

require(rgdal)
require(sf)
require(tidyverse)
require(sp)
require(rstudioapi)

# Set working directory 
setwd("C:/Users/daviessa/Documents/CURRENT PROJECTS/Substrate models/Validation data/ROV")

# Read in shapefile
dsn="T:/Substrate/VALIDATION DATA/ROV_2013_2015"
lay="WCTSS_ROV_SpeciesSubstrateObs"

#shp <- readOGR(dsn = dsn, layer = lay)
#shp <- read_sf(dsn = dsn, layer = lay) # is this suppose to read it faster??
crs <- proj4string(shp) 
bbox(shp)

saveRDS(shp, "shp.rds") 
plot(shp)
shp <- readRDS("shp.rds")

# Pull out coords and attribute data and add coords to dataframe
coords <- as.data.frame(shp@coords)
df <- shp@data
str(df)
df <- dplyr::bind_cols(df, coords)
colnames(df)[colnames(df)=="coords.x1"] <- "coords.x"
colnames(df)[colnames(df)=="coords.x2"] <- "coords.y"


# Remove unnecessary rows
# Rows containing species data, NAs, or incomplete data
df1 <- dplyr::select(df, Datetim,TrnsctD,TextTim,PrjctNm,TrnsctN,OnBottm,DmnntSb,DmnntPr,SbdmnnS,SbdmnnP,dtstr,Depth,coords.x,coords.y)
df1 <- unique(df1)

# Recode survey - original df was incomplete
df1$Survey1 <- as.factor(substr((as.character(df1$PrjctNm)), 1, 11))

# Determine the number of transects
transects <- aggregate(TrnsctN ~ Survey1, data=df1, FUN=function(x) length(unique(x)))
transects

# Determine counts per dominant substrate type
count(df1, "DmnntSb")
# Remove sponge records - not relevant for this analysis
df1 <- dplyr::filter(df1, DmnntSb!=12 & DmnntSb!=13)
# Determine the percent of dominant substrates
count(df1, "DmnntPr")

# Determine counts per subdominant substrate type
count(df1, "SbdmnnS")
# Recode subdominant sponge records to NA - in order to preserve the dominant substrate
df1$SbdmnnS[df1$SbdmnnS==12] <- NA
df1$SbdmnnS[df1$SbdmnnS==13] <- NA
# Check that it worked!
count(df1, "SbdmnnS")
# Determine the percent of subdominant substrates
count(df1, "SbdmnnP")

# Read in substrate category table
sub.cat <- read.csv( "C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep/Data/LookupTbls/SubstrateCategories.csv", header=T, sep="," )

# Rename columns to match WCTSS data
colnames(sub.cat)[colnames(sub.cat)=="Substrate1"] <- "DmnntSb"
colnames(sub.cat)[colnames(sub.cat)=="Substrate2"] <- "SbdmnnS"

# Match substrateID to substrate category
df2 <- dplyr::left_join(df1, sub.cat, by=c("DmnntSb","SbdmnnS"))
summary(df2)

colnames(df2)[colnames(df2)=="Sub.cat"] <- "BType4"
colnames(df2)[colnames(df2)=="SubstrateCat.Nme"] <- "BType4.nme"

count(df2, "Sub.cat")
count(df2, "OnBottm")
head(df2,3)

# Pull out the NA's to examine them - this is a check to determine if they exist
df.na <- df2[is.na(df2$Sub.cat),]
#summary(df.na)
 
# # Look for unique combo's of substrates to update crosswalk table
# df.sub <- unique(dplyr::select(df.na, DmnntSb, SbdmnnS))
# df.sub <- arrange(df.sub,DmnntSb)
# df.sub

# Write as a shapefile in Albers projection
coordinates(df2) <- c("coords.x","coords.y") 
# Albers
crs.geo <- crs
# define projection
proj4string(df2) <- crs.geo
filename <- "WCTSS_ROV_SubstrateOnly"
dsn <- getwd()

writeOGR(df2, dsn=dsn, layer=filename, driver="ESRI Shapefile") #, overwrite_layers=TRUE )
write.table(df2,"C:/Users/daviessa/Documents/CURRENT PROJECTS/Substrate models/Validation data/ROV/WCTSS_ROV_SubstrateOnly.csv", sep=",", row.names=F)
