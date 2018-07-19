
library(tidyverse)
library(sf)
library(stringr)
library(feather)
library(readxl)
library(sp)

setwd("~/Documents/wu_waterbudget")

# load thermo coefficients
thermo_all <- read.csv("data/ga_thermo/thermo_2010_coefficients.csv", stringsAsFactors = F)

# load clipped hucs
clipped_hucs <- st_read("data/clipped_hucs/clipped_hucs.shp") 

# find plants in ACF study area
# thermo_sf <- st_as_sf(thermo_all, coords=c("lon","lat"),crs=st_crs(clipped_hucs)) %>%
#   st_intersection(st_union(clipped_hucs)) %>%
#   mutate(cnty_fips = c("01069","13095","13321")) %>%
#   mutate(lon = st_coordinates(.)[,1],
#          lat = st_coordinates(.)[,2]) %>%
#   select(plant_cd,lon,lat,c_gkwh,w_gkwh,state,cnty,cnty_fips,
#          huc8=HUC_8,huc10=HUC_10,huc12=HUC_12) %>%
#   st_set_geometry(NULL)

thermo_sf <- st_as_sf(thermo_all, coords=c("lon","lat"),crs=st_crs(clipped_hucs)) %>%
  st_intersection(clipped_hucs) %>%
  st_set_geometry(NULL)

# mapview(clipped_hucs) + mapview(thermo_sf, col.regions="green")

# read in EIA data
eia08 <- read_excel("data/ga_thermo/eia2008/eia923December2008.xls",sheet=1,skip=7)
eia09 <- read_excel("data/ga_thermo/eia2009/EIA923 SCHEDULES 2_3_4_5 M Final 2009 REVISED 05252011.xls",sheet=1,skip=7)
eia10 <- read_excel("data/ga_thermo/eia2010/EIA923 SCHEDULES 2_3_4_5 Final 2010.xls",sheet=1,skip=7)
eia11 <- read_excel("data/ga_thermo/eia2011/EIA923_Schedules_2_3_4_5_2011_Final_Revision.xlsx",sheet=1,skip=5)
eia12 <- read_excel("data/ga_thermo/eia2012/EIA923_Schedules_2_3_4_5_M_12_2012_Final_Revision.xlsx",sheet=1,skip=5)

# makes consistent names
names(eia11) <- names(eia12) <- names(eia08)

# function to set neg values to zero
neg2zero <- function(x){ifelse(x<0,0,as.numeric(x))}

# built thermo dataset
thermo <- rbind(eia08,eia09,eia10,eia11,eia12) %>%
  dplyr::select(plant_cd="Plant ID",NETGEN_JAN:NETGEN_DEC,
         year=Year,mover="Reported Prime Mover") %>%
  filter(mover %in% c("ST","CT","CA","CS")) %>% # see email mharris@usgs.gov 4/10/2018
  right_join(thermo_sf, by = "plant_cd") %>% # add coefficients
  mutate_at(vars(NETGEN_JAN:NETGEN_DEC),funs(neg2zero)) %>% # set neg values to 0
  mutate(netgen = dplyr::select(., NETGEN_JAN:NETGEN_DEC) %>% # sum monthly gen values
           apply(1, sum, na.rm=TRUE)) %>%
  mutate(netgen = netgen*1000) %>% # convert to MWh
  mutate(mgal_cons=(netgen*c_gkwh)/1e6, # use consumption coefficient 
         mgal_wthrl=(netgen*w_gkwh)/1e6) %>% # use withdrawal coefficient 
  group_by(plant_cd,year,HUC_10) %>% # group for summary
  summarize(mgal_cons = sum(mgal_cons), 
            mgal_wthrl = sum(mgal_wthrl)) %>%
  ungroup() 

# build dataframe with all years and HUC 10s
all_thermo <- expand.grid(year=min(thermo$year):max(thermo$year),
                          HUC_10=base::unique(clipped_hucs$HUC_10)) %>%
  left_join(thermo,by=c("year","HUC_10")) 

# plot time series
ggplot(na.omit(all_thermo), aes(year,mgal_wthrl)) +
  geom_line() +
  geom_point() +
  facet_wrap(~HUC_10, ncol=1, scales="free_y") +
  ggtitle('Thermoelectric from 2008-2012',
          subtitle="Each facet represents an individual HUC 10") +
  theme_bw()

ga_thermo <- clipped_hucs %>%
  left_join(all_thermo, by="HUC_10") 

# mapview(ga_thermo, zcol="mgal_wthrl")

st_write(ga_thermo, "data/ga_thermo/shapefile/ga_thermo.shp")




