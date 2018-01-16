
library(tidyverse)
library(stringr)
library(feather)

setwd("~/Documents/wu_waterbudget")

nwis_full <- readr::read_delim("data/ga_nwis/ga_nwis_2000_2014.rdb",delim="\t",skip = 178,na=c(""," "))

nwis <- nwis_full %>%
  data.frame() %>%
  select_all(tolower) %>%
  filter(agency_cd!="5s") %>%
  select(from_agency_cd,from_site_no,to_agency_cd,
         to_site_no,from_state_cd,from_county_cd,
         from_huc_cd,contains("_dec"),contains("tp_ln"),
         water_cd,from_nat_water_use_nm,year,annual_val) %>%
  mutate(mgal=round(as.numeric(annual_val)*365,2),
         from_uid = paste0(from_agency_cd,from_site_no)%>% 
           str_replace_all(" ", ""),
         to_uid=paste0(to_agency_cd,to_site_no) %>% 
           str_replace_all(" ", ""),
         cnty_fips=sprintf("%03d",as.numeric(from_county_cd)) %>%
           paste0(from_state_cd,.),
         lat=as.numeric(from_dec_lat_va),
         lon=as.numeric(from_dec_long_va)) %>%
  filter(year %in% c(2008:2012)) %>%
  filter(from_huc_cd %in% irrigation$huc8) %>%
  select(from_uid,to_uid,cnty_fips,huc8=from_huc_cd,lon,lat,
         source=water_cd,type=from_nat_water_use_nm,year,mgal)

write_feather(nwis,"data/nwis.feather")

# checking coordinates from N.B.
hold <- read_excel("data/ga_nwis/FA-DV-sites-HUC12.xlsx") %>%
  mutate(from_uid = paste0(from_agency_cd,from_site_no)%>% 
          str_replace_all(" ", ""),
         lat = as.numeric(lat),
         lon = as.numeric(lon)) 

coordinates(hold) <- ~lon+lat
proj4string(hold) <- CRS("+init=EPSG:26967")
hold_deg <- spTransform(hold,CRS("+init=epsg:4326"))
wu_coords <- hold_deg

write_csv(data.frame(wu_coords@coords),"data/ga_nwis/coords/wu_coords.csv")

library(sf)
acf_huc <- st_read("data/acf_hucs/acf_wbd.shp") 

gw <- st_read("data/acf_gw_model/ActGrid_clean.shp") %>%
  st_transform(crs=4269) %>%
  st_write("data/acf_gw_model/two/ActGrid_clean.shp")

plot(acf_huc["HUC_12"]); plot(gw["col"], add=T)
