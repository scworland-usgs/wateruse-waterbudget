
library(tidyverse)
library(sf)
library(stringr)
library(feather)
library(readxl)
library(sp)

setwd("~/Documents/wu_waterbudget")

thermo_all <- read.csv("data/ga_thermo/thermo_2010_coefficients.csv", stringsAsFactors = F)

# load H12 shape file and subset for HUC12s in footprint
irr <- read_feather("data/irrigation.feather") %>% distinct(huc12)
acf_huc <- st_read("data/acf_hucs/acf_wbd.shp") %>% as("Spatial")

# join points to huc12
thermosp <- thermo_all 
coordinates(thermosp) <- ~lon+lat
proj4string(thermosp) <- CRS(proj4string(acf_huc))
huc_sub <- over(thermosp, acf_huc[,c('HUC_8','HUC_10','HUC_12')])

# add huc12 to data matrix
thermo_all$huc12 <- as.character(huc_sub$HUC_12)
thermo_all$huc10 <- as.character(huc_sub$HUC_10)
thermo_all$huc8 <- as.character(huc_sub$HUC_8)

thermo_all <- thermo_all %>%
  na.omit() %>%
  mutate(cnty_fips = c("01069","13095","13321"))

# read in EIA data
eia08 <- read_excel("data/ga_thermo/eia2008/eia923December2008.xls",sheet=1,skip=7)
eia09 <- read_excel("data/ga_thermo/eia2009/EIA923 SCHEDULES 2_3_4_5 M Final 2009 REVISED 05252011.xls",sheet=1,skip=7)
eia10 <- read_excel("data/ga_thermo/eia2010/EIA923 SCHEDULES 2_3_4_5 Final 2010.xls",sheet=1,skip=7)
eia11 <- read_excel("data/ga_thermo/eia2011/EIA923_Schedules_2_3_4_5_2011_Final_Revision.xlsx",sheet=1,skip=5)
eia12 <- read_excel("data/ga_thermo/eia2012/EIA923_Schedules_2_3_4_5_M_12_2012_Final_Revision.xlsx",sheet=1,skip=5)

names(eia11) <- names(eia12) <- names(eia08)

thermo <- rbind(eia08,eia09,eia10,eia11,eia12) %>%
  rename(plant_cd="Plant ID",netgen="NET GENERATION (megawatthours)",
         year=Year,mover="Reported Prime Mover") %>%
  filter(mover %in% c("ST","CT","CA","CS")) %>%
  filter(plant_cd %in% thermo_all$plant_cd) %>%
  select(plant_cd,year,netgen) %>%
  mutate(netgen = netgen*1000) %>%
  group_by(plant_cd,year) %>%
  summarize(netgen=sum(abs(netgen))) %>%
  left_join(thermo_all,by="plant_cd") %>%
  mutate(mgal_cons=(netgen*c_gkwh)/1e6,
         mgal_wthrl=(netgen*w_gkwh)/1e6) %>%
  ungroup() %>%
  select(lon,lat,year,mgal_wthrl,mgal_cons,huc12,huc10,huc8,cnty_fips)

write_feather(thermo,"data/thermo.feather")



