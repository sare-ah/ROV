###############################################################################
# Objective: Create validation data for substrate model
# 
# Methods:  
#
# Author:   Sarah Davies 
#
# Created:  January 6th, 2020 
###############################################################################

require(rgdal)
require(sf)
require(tidyverse)
require(sp)
require(rstudioapi)
require(raster)



# Set working directory 
setwd("C:/Users/daviessa/Documents/CURRENT PROJECTS/Substrate models/Validation data/ROV")

# Read in ROV substrate shapefile
dsn <- "C:/Users/daviessa/Documents/CURRENT PROJECTS/Substrate models/Validation data/ROV"
rds <- "/WCTSS_Substrate.rds"
file <- paste0(dsn,rds)
pts <- readRDS(file)  

# path <- "C:/Users/daviessa/Documents/ArcGIS/20mDEM_tifs"
# setwd(path)
# Read in bathymetry data for HG and NCC 
hg <- raster("C:/Users/daviessa/Documents/ArcGIS/20mDEM_tifs/hg_bathy.tif")
ncc <- raster("C:/Users/daviessa/Documents/ArcGIS/20mDEM_tifs/ncc_bathy.tif")
ras.list <- list(hg, ncc)

plot(ncc)
points(pts)
pts$cellID <- cellFromXY(ncc,pts)
str(pts)

pts.ncc <- dplyr::filter(pts@data, !is.na(cellID))
str(pts.ncc)
pts.ncc <- dplyr::select(pts.ncc, PrjctNm, TrnsctN, RMSM.cat, RMSM.Nme, cellID)
summary(pts.ncc)
