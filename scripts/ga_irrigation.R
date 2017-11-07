
library(raster)
library(sp)
library(rgdal)
library(tidyverse)
library(feather)

setwd("~/Documents/wu_waterbudget")

# list all shape files in folder
all_files <- list.files(path="data/ga_irrigation/acf_agwateruse_2008_2012_shapefiles/",pattern = glob2rx("*depth*.shp"))
file_list <- gsub("\\..*","",all_files)

# Function to extract relevant data
ga_irr <- NULL
for (i in 1:length(file_list)){
  options(warn=-1) # turn off warnings
  fname=file_list[i] # grab first file name
  shpfile <- readOGR(path.expand("data/ga_irrigation/acf_agwateruse_2008_2012_shapefiles"), fname)
  data <- shpfile@data
  
  if("Acres" %in% names(data)){
    depth <- select(data,contains("irrig"))
    acres <- select(data,contains("Acres"))
    source <- select(data,contains("Source"))
  }else{
    depth <- Filter(function(d) all(0<=d & d<15), data)
    acres <- Filter(function(d) all(0<=d & max(d)>500 & max(d)<2000), select_if(data,is.numeric))
    source <- Filter(function(d) any(levels(d)=="G"), data)
  }
  
  coords <- coordinates(shpfile)
  year=parse_number(fname)
  month = substr(fname,1,3)
  out <- data.frame(year,month,coords,source,depth,acres) %>% 
    setNames(c("year","month","lon","lat","source","depth_in","acres"))
  
  ga_irr <- rbind(ga_irr,out) # append dataframes
  options(warn=0) # turn on warnings
}

# write_feather(ga_irr,"data/GA_irrigation/ga_irr.feather")
ga_irr <- read_feather("data/GA_irrigation/ga_irr.feather") %>%
  mutate(depth_in = ifelse(depth_in==-9999,NA,depth_in),
         mgal = ((1/12)*depth_in)*(acres*43560)*7.48e-6) %>%
  na.omit() %>%
  group_by(lon,lat,source,year) %>%
  summarize(mgal=sum(mgal))
  

# Add HUC data ----

# load H12 shape file and subset for GA and AL,GA
huc12 <- readOGR(path.expand("data/nhd_wbd"), "WBDSnapshot_National")
ga12 <- base::subset(huc12, STATES %in% c("GA","AL,GA","FL,GA","AL,FL,GA"))
rm(huc12)

# reproject irrigation data
ga_irrsp <- ga_irr
coordinates(ga_irrsp) <- ~lon+lat
proj4string(ga_irrsp) <- proj4string(shpfile)
ga_irrsp <- spTransform(ga_irrsp,CRS(proj4string(ga12)))

# join points to huc12
huc_sub <- over(ga_irrsp, ga12[,c('HUC_8','HUC_10','HUC_12')])

# add huc12 to data matrix
ga_irr$huc12 <- as.character(huc_sub$HUC_12)
ga_irr$huc10 <- as.character(huc_sub$HUC_10)
ga_irr$huc8 <- as.character(huc_sub$HUC_8)

# plot subset
ga12_sub <- subset(ga12, HUC_12 %in% huc_sub$HUC_12)
plot(ga12_sub)
points(ga_irrsp,pch=20,cex=0.1)

# Add county data ----

# load shapefile and join
counties <- readOGR(path.expand("data/county_shapefiles"), "cb_2013_us_county_500k")
county_sub <- over(ga_irrsp,counties[,"GEOID"])

# add county fips to data matrix
ga_irr$cnty_fips <- as.character(county_sub$GEOID)

# plot subset
counties <- subset(counties, GEOID %in% county_sub$GEOID)
plot(counties)
points(ga_irrsp,pch=20,cex=0.1)

# Aggregate by huc12 and year
irrigation <- ga_irr %>%
  ungroup() %>%
  mutate(lon=ga_irrsp@coords[,1],
         lat=ga_irrsp@coords[,2])


write_feather(irrigation,"data/irrigation.feather")

#writeOGR(obj=ga12, dsn=path.expand("data/acf_hucs"), layer="acf_wbd", driver="ESRI Shapefile")


  