library(tidyverse)
library(stringr)
library(sf)

# read WBD dataset
wbd <- st_read('data/nhd_wbd/WBDSnapshot_National.shp') %>%
  filter(str_detect(STATES, 'GA')) %>%
  group_by(HUC_10) %>%
  summarize(acres = sum(AreaHUC12))

# list all irrigation shape files
folder <- "data/ga_irrigation/acf_agwateruse_2008_2012_shapefiles/"
all_files <- list.files(path=folder,pattern = glob2rx("*depth*.shp"))
file_list <- gsub("\\..*","",all_files)

irr_area <- NULL
for (i in 1:length(file_list)){
  
  print(paste0("loading ",i," out of ",length(file_list)))
  
  options(warn=-1) # turn off warnings
  fname=file_list[i] # grab first file name
  shpfile <- st_read(path.expand(folder), fname)
  d <- st_union(shpfile)
  hull <- st_convex_hull(d)
  area <- data.frame(fname=fname, 
                     area=st_area(hull))
  
  irr_area <- rbind(irr_area,area)
}

# load irrigation data with the smallest area
irr <- st_read(path.expand(folder), file_list[which.min(irr_area$area)]) %>%
  st_transform(st_crs(wbd))

# select HUC10s that intersect with irr
wbd_acf <- wbd[lengths(st_intersects(wbd,irr)) > 0,]

st_write(wbd_acf, "data/acf_hucs/acf_wbd.shp")

# generate convex hull with buffer
hull <- st_union(irr) %>%
  st_convex_hull() %>%
  st_buffer(dist=0.01)

# mapview(cnty_acf,pane=NULL) + 
#   mapview(wbd_acf,pane=NULL) + 
#   mapview(irr,cex = 3,zcol="july_irrig") + 
#   hull

# clip hucs to the hull
clipped_hucs <- wbd_acf %>%
  st_intersection(hull) %>%
  mutate(area_km = as.numeric(st_area(geometry)/1e6))

st_write(clipped_hucs, "data/clipped_hucs/clipped_hucs.shp")

# mapview(cnty_acf,pane=NULL) +
#   mapview(clipped_hucs,pane=NULL) + 
#   mapview(irr,cex = 3,zcol="july_irrig")

# find counties that intersect study area
counties <- st_read("data/county_shapefiles/cb_2013_us_county_500k.shp") 

counties_acf <- counties[lengths(st_intersects(counties, hucs_acf)) > 0,] %>%
  mutate(cnty_fips=as.character(GEOID)) %>%
  filter(cnty_fips != "12063") %>%
  mutate(area_km = as.numeric(st_area(geometry)/1e6))

# mapview(counties_acf,pane=NULL) + 
#   mapview(clipped_hucs) + 
#   mapview(irr,cex = 3,zcol="july_irrig")

st_write(counties_acf, "data/acf_counties/acf_county.shp")

# find counties completely contained in the study area
counties_within_acf <- counties[lengths(st_within(counties, st_buffer(hull,0.05))) > 0,]

# mapview(counties_acf, col.region="grey", pane=NULL) + 
#   mapview(clipped_hucs, col.region="white", pane=NULL) + 
#   mapview(counties_within_acf$geometry)

st_write(counties_within_acf, "data/acf_counties/counties_within_acf.shp")
