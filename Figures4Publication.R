# Add description
#----------------

library(cowplot)
# Libraries needed to run this code
library(raster)
library(rgdal)
library(tidyverse)

library(sf)
library(sp)

setwd("C:/Users/daviessa/Documents/CURRENT_PROJECTS/Substrate_models/Figures4Publication")

# ***To do***
# Build loops / apply function to:
# Grab all shp
# Fortify
# Plot
# Cowplot
# Play with insets
# ***********


# Input data
#-----------

# Observation (training data)
setwd("F:/GIS/Requests 2019/Heatmaps4Substrate")

# Path to gridded shp
path <- paste0(getwd(),"/","Obs_per1kmGrid")
obslist <- list.files(path = path, pattern='\\.shp$', 
                      all.files=TRUE, full.names=TRUE)
obslist
nmes <- c("HG","NCC","QCS","SoG","WCVI")

# Read all shp into a list using function shapefile from raster
obs <- lapply(obslist, shapefile)
names(obs) <- nmes

# Determine range of count values for each region
CntRng <- vector("list",5)
names(CntRng) <- nmes

for (i in 1:length(obs)){
  CntRng[[i]] <- range(obs[[i]]@data$Cnt)
}
CntRng

# Build breaks from range values
#brks <- c(75,150,225)

# Read in each shp - sf & rgdal - to read in simple features I think they need special fields already? Check class notes
#------------------------------
# qcs.sf <- st_read("F:/GIS/Requests 2019/Heatmaps4Substrate/Obs_per1kmGrid/QCS_Obs_1kmGrid.shp")
qcs <- readOGR(dsn = "F:/GIS/Requests 2019/Heatmaps4Substrate/Obs_per1kmGrid", layer="QCS_Obs_1kmGrid")
sog <- readOGR(dsn = "F:/GIS/Requests 2019/Heatmaps4Substrate/Obs_per1kmGrid", layer="SoG_Obs_1kmGrid")


sog <- st_read("F:/GIS/Requests 2019/Heatmaps4Substrate/Obs_per1kmGrid/SoG_Obs_1kmGrid.shp", package="sf")


# ggplot does not process spatial polygon data frames directly
# Need to transfer polygons into a dataframe using fortify
# generate a unique ID for each polygon to fortify the data
# in order to plot polygons, first fortify the data
qcs@data$id <- rownames(qcs@data)
sog@data$id <- rownames(sog@data)

# create a data.frame from our spatial object
qcsdata <- fortify(qcs, region = "id")
sog.data <- fortify(sog, region = "id")

# merge the "fortified" data with the data from our spatial object
qcsdf <- merge(qcsdata, qcs@data, by = "id")
sog.df <- merge(sog.data, sog@data, by = "id")

# First plot attempt
pq <- ggplot(data = qcsdf, aes(x = long, y = lat, group = group, fill = Cnt)) +
  geom_polygon() +
  geom_path(color = "white", size = 0.2) +
  scale_fill_gradient(breaks=brks, labels=c("Low","Medium","High")) + 
  coord_equal() +
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "QCS")
print(pq)

# Better colours, better background?? Check differences and fix
p.sog <- ggplot(data = sog.df, aes(x = long, y = lat, group = group, fill = Cnt)) +
  geom_polygon() +
  scale_fill_gradient(low="blue", high="red") +
  coord_sf(crs = 3005) 
print(p.sog)

# Make smaller polygon for victoria only 
# OR can we just limit the extent of the data within the plotting screen?
# Google of answer!
#---------------------------------------
# Need to start working with simple features

# Use widget to build bounding box
# (using http://arthur-e.github.io/Wicket/sandbox-gmaps3.html)
area <- st_as_sfc("POLYGON((-123.6090656891057 48.72785160096076,-123.2080647125432 48.72785160096076,-123.2080647125432 48.30576888069326,-123.6090656891057 48.30576888069326,-123.6090656891057 48.72785160096076))")
area <- st_set_crs(area, 3005)

# Crop SoG data - but which spatial format??? Check notes
sog_sp <- as(sog, "Spatial")
sog_sf <- sf::st_as_sf(sog_sp)
coast <- read_sf("C:/Users/daviessa/Documents/R/Courses/TESA Spatial Stats 2019/Practicals/data/coastline")

cropped_sog <- sf::st_crop(coast,area)


# Plot smaller map
p.vic <- ggplot(data = sog.df, aes(x = long, y = lat, group = group, fill = Cnt)) +
  geom_polygon() +
  coord_sf(crs = 3005) +
  scale_fill_gradient(low="blue", high="red") 
p.vic


# Plot together and alter layout
#-------------------------------
cowplot::plot_grid(pq, ps, nrow=1, labels=c("QCS","SoG"),
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



# Plot with PBSmapping
#---------------------
library(PBSmapping)
data("nepacLL")
source("C:/Users/daviessa/Documents/R/Courses/PBS Mapping course/0 Initialize.R")

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




