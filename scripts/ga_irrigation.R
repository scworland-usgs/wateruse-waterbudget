
library(raster)
library(sf)
library(rgdal)
library(tidyverse)
library(feather)
library(rgdal)

setwd("~/Documents/wu_waterbudget")

# list all shape files in folder
all_files <- list.files(path="data/ga_irrigation/acf_agwateruse_2008_2012_shapefiles/",pattern = glob2rx("*depth*.shp"))
file_list <- gsub("\\..*","",all_files)

# Function to extract relevant data
ga_irr <- NULL
for (i in 1:length(file_list)){
  
  print(paste0("loading ",i," out of ",length(file_list)))
  
  options(warn=-1) # turn off warnings
  fname=file_list[i] # grab first file name
  shpfile <- st_read(path.expand("data/ga_irrigation/acf_agwateruse_2008_2012_shapefiles"), fname)
  coordinates <- st_coordinates(shpfile)
  year <- parse_number(fname)
  month <- substr(fname,1,3)
  data <- shpfile %>% st_set_geometry(NULL)
  
  if("Acres" %in% names(data)){
    depth <- select(data,contains("irrig"))
    acres <- select(data,contains("Acres"))
    source <- select(data,contains("Source"))
  }else{
    depth <- Filter(function(d) all(0<=d & d<15), data)
    acres <- Filter(function(d) all(0<=d & max(d)>500 & max(d)<2000), select_if(data,is.numeric))
    source <- Filter(function(d) any(levels(d)=="G"), data)
  }
  
  out <- data.frame(year,month,coordinates,source,depth,acres) %>% 
    setNames(c("year","month","lon","lat","source","depth_in","acres"))
  
  ga_irr <- rbind(ga_irr,out) # append dataframes
  options(warn=0) # turn on warnings
}

# write_feather(ga_irr,"data/GA_irrigation/ga_irr.feather")
ga_irr <- read_feather("data/GA_irrigation/ga_irr.feather") 

# load ACF study area
clipped_hucs <- st_read("data/clipped_hucs/clipped_hucs.shp") 

# CRS from shapefile Jamie Painter provided
irr_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-83.5 +x_0=0 +y_0=0 
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# reproject data and find points in ACF study area
ga_irr_sf <- st_as_sf(ga_irr, coords=c("lon","lat"),crs=irr_crs) %>%
  st_transform(crs=st_crs(clipped_hucs)) %>%
  st_intersection(clipped_hucs) 

# plot 2010 points
ga_irr2010 <- ga_irr_sf %>%
  filter(year==2010) %>%
  mutate(depth_in = ifelse(depth_in==-9999,NA,depth_in),
         mgal = ((1/12*(depth_in)*(acres*43560))*7.48052)/1e6,
         x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>%
  group_by(x,y) %>%
  summarize(mgal = sum(mgal, na.rm=T))

ggplot(ga_irr2010) +
  geom_sf(data=clipped_hucs$geometry) +
  geom_sf(aes(color=log10(mgal)),alpha=0.5) +
  coord_sf(datum = NA) +
  scale_color_viridis_c() +
  theme_void()
  

# clean up and export
irrigation <- ga_irr_sf %>%
  mutate(depth_in = ifelse(depth_in==-9999,NA,depth_in),
         mgal = ((1/12*(depth_in)*(acres*43560))*7.48052)/1e6) %>%
  group_by(HUC_10,year) %>%
  summarize(mgal = sum(mgal, na.rm=T)) %>%
  st_set_geometry(NULL)

# plot time series
ggplot(irrigation, aes(year,mgal,color=HUC_10)) +
  geom_line(show.legend=F) +
  geom_point(show.legend=F) +
  # facet_wrap(~HUC_10) +
  scale_color_viridis_d(begin=0.0,end=0.9,option="C") +
  ggtitle('Irrigation from 2008-2012',
          subtitle="Each line represents an individual HUC 10") +
  theme_bw() 

# lon = st_coordinates(.)[,1]
# lat = st_coordinates(.)[,2]

ga_irrigation <- clipped_hucs %>%
  left_join(irrigation, by="HUC_10")

st_write(ga_irrigation, "data/ga_irrigation/shapefile/ga_irrigation.shp")

# plot facets of huc10 irrigation
ggplot(ga_irrigation) +
  geom_sf(aes(fill=mgal),alpha=1) +
  coord_sf(datum = NA) +
  scale_fill_viridis_c() +
  facet_wrap(~year, nrow=1) +
  theme_void()





  