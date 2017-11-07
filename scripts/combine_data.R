
library(tidyverse)
library(feather)
library(sf)

setwd("~/Documents/wu_waterbudget")

nwis <- read_feather("data/nwis.feather")
irrigation <- read_feather("data/irrigation.feather")
thermo <- read_feather("data/thermo.feather")

acf_huc <- st_read("data/acf_hucs/acf_wbd.shp") 
acf10 <- aggregate(acf_huc, list(acf_huc$HUC_10), function(x) x[1])

ggplot() + 
  theme_bw() +
  geom_sf(data=acf10, fill="white") + 
  geom_point(data = filter(irrigation,year==2009), aes(lon,lat), alpha=0.3,size=0.5) +
  #geom_point(data = filter(nwis,year==2009), aes(lon,lat), alpha=0.7,size=2, color="red") +
  geom_point(data=filter(thermo,year==2009), aes(lon,lat), shape=23,fill="dodgerblue",size=4) 
  

plot(st_geometry(acf_huc), col = sf.colors(12, categorical = TRUE), border = 'grey', axes = TRUE)
# plot(acf_huc["ACRES"],key.pos = 1, axes = TRUE, key.size = lcm(1.3))
