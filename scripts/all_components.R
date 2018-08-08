library(tidyverse)
library(feather)
library(sf)
library(zoo)
library(velox)
library(mapview)
source("scripts/utils.R")

# precip, Q, and AET --------------
# load precip in mm
precip <- readRDS("data/huc10_components/Precip.rds") 

precip_df <- t(coredata(precip$PRISM)) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  set_names(c("HUC_10",2008:2012)) %>%
  gather(year,P,-HUC_10)
  
# load Q in mm
Q <- readRDS("data/huc10_components/Runoff.rds") 

Q_df <- t(coredata(Q$'NLDAS-VIC')) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  set_names(c("HUC_10",2008:2012)) %>%
  gather(year,Q,-HUC_10)

# load AET in mm
AET <- readRDS("data/huc10_components/AET.rds") 

AET_df <- t(coredata(AET$'SSEBop')) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  set_names(c("HUC_10",2008:2012)) %>%
  gather(year,AET,-HUC_10)

# NWIS data --------------
# load county-level data
cnty <- read_tsv("data/county_water_use_2010.txt") %>%
  dplyr::select(GEOID="FIPS", municipal="PS-Wtotl",
                thermoelectric="PT-Wtotl", irrigation="IR-IrTot") %>%
  mutate_at(vars(municipal,thermoelectric,irrigation),function(x){x*365}) %>%
  gather(type,nwis_value,-GEOID) %>%
  mutate(year=2010) 

# load GA counties and join
ga_counties <- st_read("data/county_shapefiles/cb_2013_us_county_500k.shp") %>%
  filter(STATEFP %in% c("01",12,13,37,45,47)) %>%
  left_join(cnty) %>%
  dplyr::select(GEOID, nwis_value, type)

# load GA WBD
ga_wbd <- st_read('data/nhd_wbd/WBDSnapshot_National.shp') %>%
  filter(str_detect(STATES, 'GA')) %>%
  group_by(HUC_10) %>%
  summarize(acres = sum(AreaHUC12))

# area-weighted irrigation
cnty_irr <- ga_counties %>%
  dplyr::filter(type=="irrigation") %>%
  dplyr::select(-type)

cnty_huc_irr <- st_interpolate_aw(cnty_irr['nwis_value'],ga_wbd, extensive=TRUE) %>%
  mutate(HUC_10 = as.character(ga_wbd$HUC_10),
         IRR = nwis_value) %>%
  dplyr::select(HUC_10,IRR) %>%
  st_set_geometry(NULL)

# population weighted public supply

# load population raster data
pop <- velox('data/ga_pop_rasters/GA_pop.tif')

cnty_ps <- ga_counties %>%
  dplyr::filter(type=="municipal") %>%
  dplyr::select(-type)

cnty_huc_ps <- sw_weighted_interp(value="nwis_value", from=cnty_ps, to=ga_wbd, 
                                  to_group_id="HUC_10", weight_var=pop)


