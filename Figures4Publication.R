knitr::opts_chunk$set(message = FALSE, warning = FALSE)
###################################################### #
###  Script to make a BC map                       ### #
###  Author:  D.K. Okamoto (modified by Jenn Burt) ### #
###################################################### #


library(cowplot)
# Libraries needed to run this code
library(raster)
library(maps) 
library(mapdata)
library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(ggsn)
library(tidyverse)
library(here)

library(sp)

setwd("C:/Users/daviessa/Documents/CURRENT_PROJECTS/Substrate_models/Figures4Publication")



# Input data
#-----------

# Observation (training data)
setwd("F:/GIS/Requests 2019/Heatmaps4Substrate")

# Path to gridded shp
path <- paste0(getwd(),"/","Obs_per1kmGrid")
obslist <- list.files(path = path, pattern='\\.shp$', 
                      all.files=TRUE, full.names=TRUE)
obslist

# Read all shp into a list using function shapefile from raster
obs <- lapply(obslist, shapefile)

# Read in each shp - sf & rgdal
# qcs.sf <- st_read("F:/GIS/Requests 2019/Heatmaps4Substrate/Obs_per1kmGrid/QCS_Obs_1kmGrid.shp")
qcs <- readOGR(dsn = "F:/GIS/Requests 2019/Heatmaps4Substrate/Obs_per1kmGrid", layer="QCS_Obs_1kmGrid")
vi <- readOGR(dsn = "F:/GIS/Requests 2019/Heatmaps4Substrate/Obs_per1kmGrid", layer="WCVI_Obs_1kmGrid")


# plot(st_geometry(qcs.sf))
# 
# ggplot() +
#   geom_sf(data=qcs.sf, aes(fill = Cnt)) +
#   scale_y_continuous(breaks = 34:36)
#   
# library(mapview)
# mapview(qcs.sf["Cnt"], col.regions = sf.colors(10))


# Pull out the 5 regions
#hg <- obs[[1]]
# ncc <- obs[[2]]
# qcs <- obs[[3]]
# sog <- obs[[4]]
# wcvi <- obs[[5]]

# ***To do***
# Build loops / apply function to:
# Grab all shp
# Fortify
# Plot
# Cowplot
# Play with insets
# ***********

# ggplot does not process spatial polygon data frames directly
# Need to transfer polygons into a dataframe using fortify
# generate a unique ID for each polygon to fortify the data
# in order to plot polygons, first fortify the data
qcs@data$id <- rownames(qcs@data)
vi@data$id <- rownames(vi@data)

# create a data.frame from our spatial object
qcsdata <- fortify(qcs, region = "id")
vidata <- fortify(vi, region = "id")

# merge the "fortified" data with the data from our spatial object
qcsdf <- merge(qcsdata, qcs@data, by = "id")
vidf <- merge(vidata, vi@data, by = "id")

# plot on continuous variable
pq <- ggplot(data = qcsdf, aes(x = long, y = lat, group = group, fill = Cnt)) +
  geom_polygon() +
  geom_path(color = "white", size = 0.2) +
  scale_fill_gradient(breaks=c(33,66,99), labels=c("Low","Medium","High")) + 
  coord_equal() +
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "QCS")
print(pq)


pv<- ggplot(data = vidf, aes(x = long, y = lat, group = group, fill = Cnt)) +
  geom_polygon() +
  geom_path(color = "white", size = 0.2) +
  scale_fill_gradient(breaks=c(33,66,99), labels=c("Low","Medium","High")) + 
  coord_equal() +
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "WCVI")
print(pv)

cowplot::plot_grid(pv, pq, nrow=1, labels=c("WCVI","QCS"),
                   label_x = .15, label_y = 0.85, label_fontface = )

# plot(qcs)
# 
# p.qcs <- ggplot(qcs) +
#   geom_polygon(colour='black', fill='white')
# 
# p.qcs
# p.ncc <- plot(ncc)
# p.qcs <- plot(qcs)
# p.sog <- plot(sog)
# p.vi <- plot(wcvi)

# hist(hg$Cnt)
# hist(ncc$Cnt)
# hist(qcs$Cnt)
# hist(sog$Cnt)
# hist(wcvi$Cnt)
# 
# cowplot::plot_grid(p.hg, p.ncc, nrow=1, labels=c("HG","NCC"),
#                    label_x = .15, label_y = 0.85, label_fontface = )




# Transform to LL coordinate system
library(sp)

# Plot
#-----
library(PBSmapping)
data("nepacLL")
source("C:/Users/daviessa/Documents/R/Courses/PBS Mapping course/0 Initialize.R")

# plotMap(nepacLL)
# 
# plotBC = function(xlim=c(-134,-124), ylim=c(48,54.5), 
#                   zlev=seq(200,1200,200), ...)
# {
#   data(nepacLL,nepacLLhigh)
#   coast = if (diff(xlim)<5) nepacLLhigh else nepacLL
#   # clin = contourLines(bcBathymetry, levels=zlev)
#   # poly = convCP(clin)
#   #isob = clipLines(poly$PolySet,xlim=xlim,ylim=ylim)
#   pdat = poly$PolyData
#   # attr(isob,"projection") <- "LL"
#   # clrFN = colorRampPalette(c("cyan4","blue","navy"))
#   # clrs = clrFN(length(zlev))
#   plotMap(coast,xlim=xlim,ylim=ylim,col="lemonchiffon",border="grey50",plt=NULL)
#   #addLines(isob, col=clrs, ...)
#   #invisible(isob)
# }


# # one example
# plotGSL = function(coastlines, xlim = c(-70, -55), ylim = c(46, 52), ...) {
#   plotMap(coastlines, xlim = xlim, ylim = ylim, bg = clr$sea, col = clr$land, tck = -0.014, las = 1, 
#           ...)
# }
# data(worldLLhigh)
# myBC <- worldLLhigh  # longitudes in our region are given as °E
# #myBC <- clipPolys(myBC, xlim = c(250, 300), ylim = c(48,54.5))
# myBC <- clipPolys(myBC, xlim = c(280, 320), ylim = c(48,54.5)) # EXAMPLE
# # we make a smaller copy of the map data file. Eq. to -80 to -20°W Note that I made it larger than
# # the GSL so that I can easily plot a larger area, which I need to do from time to time Note that
# # polygons have to be closed to the smaller region
# myBC$X <- myBC$X - 360  # we make all longitudes negatives (longitudes West)
# plotGSL(myBC)

# another example
plotMap(nepacLL, col=clr$land, bg=clr$sea)

longitude <- c(-134, -122)
latitude <- c(48, 55)
plotMap(nepacLL, col=clr$land, bg=clr$sea, xlim = longitude, ylim = latitude)
points()

## Plot the map
data(nepacLLhigh)       # DFO BC Coastline data - high resolution
plotMap(nepacLLhigh, xlim=c(-128.52, -127.93), ylim=c(51.56, 52.07), col="grey90", bg="white", tckMinor = 0,
        xlab="", ylab="", lwd=0.5)
box()
#add a scale bar
#map.scale(x=-128.455, y=51.61, ratio=FALSE, relwidth=0.2) # could not find function


plot(drops, pch = 1, col = "steelblue", add = T)
plot(rov, pch=3, col="red", add = T)



# ############## Make a map with the sites ################## #
# ############# Using DFO coastline data file ################
# 
# 
# ## Plot the map
# data(nepacLLhigh)       # DFO BC Coastline data - high resolution
# plotMap(nepacLLhigh, xlim=c(-128.52, -127.93), ylim=c(51.56, 52.07), col="grey90", bg="white", tckMinor = 0,
#         xlab="", ylab="", lwd=0.5)
# box()
# 
# #add a scale bar
# map.scale(x=-128.455, y=51.61, ratio=FALSE, relwidth=0.2)
# 
# data("nepacLL")
# # Try all of BC
# plotMap(nepacLL, xlim=c(-230, -225), ylim=c(50, 55), col="grey90", bg="white", tckMinor = 0,
#         xlab="", ylab="", lwd=0.5)
# box()
# 
# 
# ############## Pacific Coast Map ##################
# ################################################### #
# 
# #creata a data file to make a basemap
# # this database has a lower resolution (which is fine for large scale map)
# m <- map_data("world", c("usa", "Canada"))
# 
# #this database has a way higher resolution
# d <- map_data("worldHires", c("Canada", "usa", "Mexico"))
# 
# #make a basic map, all one colour
# # play around with xlim and ylim to change the extent
# ggplot() + 
#   geom_polygon(data = d, aes(x=long, y = lat, group = group)) + 
#   theme_bw() +
#   coord_map("conic", lat0 = 18, xlim=c(228, 240), ylim=c(47,55))



# Quick plotting of validation data
#==================================

# Read data
#----------
# DropCam
dsn <- "C:/Users/daviessa/Documents/CURRENT_PROJECTS/Substrate_models/Validation data/DropCamera/from_SpatialDatasets/DropCam_data/Shapefiles"
shp <- "DropCam_sites4SubstrateModel"
drops <- readOGR(dsn=dsn, layer=shp)

# ROV
dsn <- "C:/Users/daviessa/Documents/CURRENT_PROJECTS/Substrate_models/Validation data/ROV"
shp <- "ROV_ValidationData"
rov <- readOGR(dsn=dsn, layer=shp)

# Format points for ggplot
#-------------------------
coords <- data.frame(drops@coords)
colnames(coords) <- c("lat","long")
drops.df <- data.frame(drops@data,coords)

coords <- data.frame(rov@coords)
colnames(coords) <- c("lat","long")
rov.df <- data.frame(rov@data,coords)

# Read polygon shapefile 
#-----------------------
PNW <- readOGR(dsn="C:/Users/daviessa/Documents/ArcGIS", layer="Pacific_Northwest")
BC.ncsc <- readOGR(dsn="F:/GIS/Base layers", layer="ncst_scst_land")
plot(PNW)
head(PNW)
BC <- subset(PNW, PNW$NAME=="BRITISH COLUMBIA")

# Format for ggplot
#------------------
BC.df <- fortify(BC)

# Plot
#-----
ggplot() + theme_bw()+
  geom_polygon(data= BC.df, aes(x=long,y=lat,group= group),
               colour= "black", size=0.1, fill='grey95') +
  geom_point(data=drops.df, aes(x=lat, y=long), size=0.5, colour="blue") + 
  geom_point(data=rov.df, aes(x=lat, y=long), size=0.5, colour="red") +  
  #coord_cartesian(xlim = c(-132, -122), ylim=c(48, 55)) +
  theme(panel.grid.minor = element_line(colour = NA),
        panel.grid.major = element_line(colour = NA),
        axis.title.y= element_blank(), axis.title.x = element_blank(),
        axis.text.y= element_text(size=10), axis.text.x = element_text(size=10))


# Example graph
# here is where you can see the styles of north arrow (scroll to bottom): http://oswaldosantos.github.io/ggsn/
# the high resolution shape file works well at this scale as it gives lots of the coastline detail
# ggplot()+ theme_bw()+
#   geom_polygon(data= BC.df, aes(x=long,y=lat,group= group),
#                colour= "black", size=0.1, fill='grey95')+
#   coord_cartesian(xlim = c(-128.17, -127.95), ylim=c(51.63, 51.772)) +
#geom_point(data=EXPTsites, aes(x=long, y=lat, shape=otter), size=4, colour="blue", stroke=1.5)+  #add this to plot site locations
#scale_shape_manual(values=c(21,24))+         #this makes different shapes for otter "yes" and otter "no" sites
# scalebar(BC.df, dist = 3, st.size=4, height=0.01, dd2km = TRUE, model = 'WGS84', anchor = c(x = -127.96, y = 51.63))+
# north(data = BC.df, scale = 0.1, symbol = 3, anchor= c(x = -128.15, y = 51.775)) +
# theme(panel.grid.minor = element_line(colour = NA),
#       panel.grid.major = element_line(colour = NA),
#       axis.title.y= element_blank(), axis.title.x = element_blank(),
#       axis.text.y= element_text(size=10), axis.text.x = element_text(size=10))
