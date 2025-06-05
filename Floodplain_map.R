#### Install packages #### 

install.packages(c("cowplot", 
                   "googleway", 
                   "ggplot2", 
                   "ggrepel", 
                   "ggspatial", 
                   "libwgeom", 
                   "sf", 
                   "rnaturalearth", 
                   "rnaturalearthdata", 
                   "ozmaps"))

install.packages("ggplot2", version='3.4.9')

install.packages("leaflet")
install.packages("rgdal")
install.packages("raster")

#### Load packages ####

library(ozmaps)   
library(sf)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(leaflet) #for making the interactive map
library(rgdal) #for importing vector data
library(raster)
library(RColorBrewer)
library(terra)
library(devEMF)



rescue_map <- ggmap(get_googlemap(center = c(lon = 130.579219, lat = -13.597935), zoom = 13,  maptype = 'satellite', 
color = 'color'))


rescue_map <- rescue_map + geom_point(data = rescue,aes(x = Long,    y = Lat,colour = as.factor(Rescue_year)),size = 5, 
 alpha = 0.5) +
labs(colour = "Rescue Year") +
theme(axis.text = element_blank(), 
axis.title = element_blank(), 
axis.ticks = element_blank(),
legen.position = "inside",
legend.position.inside = c(.94, .90)) +
scale_colour_brewer(palette = "YlOrRd")


r_map2 <- rescue_map + 
ggspatial::annotation_scale(location = "tr", width_hint = 0.5) +
ggspatial::annotation_north_arrow(location = "tl", 
                                        +                          which_north = "true", 
                                        +                          style = north_arrow_fancy_orienteering) +
  +   coord_sf(crs = 4326)