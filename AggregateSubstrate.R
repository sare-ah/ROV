###############################################################################
# Objective: Create validation data for substrate model
# 
# Methods:  Use cellFromXY() to determine corresponding raster cell number 
#           for each data point. Group by cell ID and calculate most common
#           substrate code. Output dataset reduced to one data point for each 
#           raster cell.
#
# Author:   Sarah Davies 
#
# Created:  January 6th, 2020 
###############################################################################

require(rgdal)
require(sf)
require(tidyverse)
require(sp)
require(raster)

# Set working directory 
setwd("C:/Users/daviessa/Documents/CURRENT PROJECTS/Substrate models/Validation data/ROV")

# Function to calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Read in ROV substrate shapefile
dsn <- "C:/Users/daviessa/Documents/CURRENT PROJECTS/Substrate models/Validation data/ROV"
rds <- "/WCTSS_Substrate.rds"
file <- paste0(dsn,rds)
shp <- readRDS(file) 
crs <- proj4string(shp) 

# Create dataframe with attributes and coordinates
xy.coords <- coordinates(shp)
att <- shp@data
pts <- cbind(xy.coords,att)

# Read in bathymetry data for HG and NCC 
hg <- raster("C:/Users/daviessa/Documents/ArcGIS/20mDEM_tifs/hg_bathy.tif")
ncc <- raster("C:/Users/daviessa/Documents/ArcGIS/20mDEM_tifs/ncc_bathy.tif")

# Separate points into bathy regions
pts.hg <- dplyr::filter(pts, PrjctNm=="Pac2015_013")
pts.ncc <- dplyr::filter(pts, PrjctNm!="Pac2015_013")

# Get the raster cell ID for each intersecting substrate point
pts.ncc$cellID <- raster::cellFromXY(ncc, pts.ncc)
pts.hg$cellID <- raster::cellFromXY(hg, pts.hg)
new.pts <- rbind(pts.ncc,pts.hg)
str(new.pts)
summary(new.pts)
# Remove extra columns
new.pts <- dplyr::select(new.pts, coords.x,coords.y,PrjctNm, TrnsctN, RMSM.cat, cellID)
head(new.pts)

# Calculate substrate mode for each unique raster cell
substrate <- new.pts %>%
  group_by(PrjctNm,cellID) %>%
  drop_na(RMSM.cat) %>%
  mutate(x.coord = max(coords.x)) %>%
  mutate(y.coord = max(coords.y)) %>%
  mutate(cntROVobs = length(RMSM.cat)) %>%
  mutate(RMSM.cat = Mode(RMSM.cat)) %>%
  distinct(x.coord,y.coord,PrjctNm,TrnsctN,cntROVobs,RMSM.cat,cellID)
head(substrate)
summary(substrate)

# Write as a shapefile in Albers projection
coordinates(substrate) <- c("x.coord","y.coord") 
# Define projection as Albers and set
crs.geo <- crs
proj4string(substrate) <- crs.geo
# Plot to check results
plot(substrate)

# Save as RDS, shp, and csv on local drive
rds.name <- paste0(getwd(),"/ROV_validationData.rds")
saveRDS(substrate, rds.name)
filename <- "ROV_ValidationData"
dsn <- getwd()
writeOGR(substrate, dsn=dsn, layer=filename, driver="ESRI Shapefile", overwrite_layer=TRUE )
outfile <- paste0(getwd(),"/ROV_ValidationData.csv")
outfile
write.table(substrate,outfile, sep=",", row.names=F)

# Save as shp on Spatial datasets drive
dsn <- "T:/Substrate/VALIDATION DATA/ROV_2013_2015/Validation_data4Substrate_model"
writeOGR(substrate, dsn=dsn, layer=filename, driver="ESRI Shapefile", overwrite_layer=TRUE )





