library(tidyverse)
library(feather)
library(sf)

# load ACF study area
clipped_hucs <- st_read("data/clipped_hucs/clipped_hucs.shp") 

# load permit data
ga_permits <- read_csv("data/ACF_WaterUse18/data/WU/ga-permits.csv",
                       col_types=cols_only(DEC_LAT_VA = "d",
                                           DEC_LONG_VA = "d",
                                           NAT_WATER_USE_NM = "c",
                                           SITE_NO = "c",
                                           PERMIT_TX = "c")) %>%
  purrr::set_names(c("site_no", "lat","lon","type","permit")) %>%
  mutate(site_no = as.character(site_no)) %>%
  drop_na(lon,lat) %>%
  st_as_sf(coords=c("lon","lat"),crs=4269)

# load data from Andy and Tara
municipal <- read_csv("data/ACF_WaterUse18_v2/results/annual_volumes.csv",
                      col_types=cols_only(year="i",
                                          annual_val = "d",
                                          HUC10 = "c",
                                          DEC_LAT_VA = "d",
                                          DEC_LONG_VA = "d",
                                          SWUDS_SITE_NO = "c",
                                          PERMIT_TX = "c")) %>%
  purrr::set_names(c("site_no","year","mgal","HUC_10","permit","lon","lat")) %>%
  drop_na(lon,lat) %>%
  st_as_sf(coords=c("lon","lat"),crs=4269) %>%
  st_transform(crs=st_crs(clipped_hucs)) %>%
  st_intersection(clipped_hucs$geometry) %>%
  # break here for plot
  group_by(year,HUC_10) %>%
  dplyr::summarize(mgal=sum(mgal)) %>%
  st_set_geometry(NULL)
  
ggplot(filter(municipal, year==2010)) +
  geom_sf(data=clipped_hucs$geometry) +
  geom_sf(aes(fill=mgal),alpha=0.7, size=3, shape=21) +
  coord_sf(datum = NA) +
  scale_fill_viridis_c() +
  theme_void()
  
mapview(clipped_hucs) + mapview(filter(municipal,year=="2010"),zcol="mgal",legend=TRUE)

# build dataframe with all years and HUC 10s
all_municipal <- expand.grid(year=min(municipal$year):max(municipal$year),
                             HUC_10=as.character(base::unique(clipped_hucs$HUC_10)), 
                             stringsAsFactors = F) %>%
  data.frame() %>%
  left_join(municipal,by=c("year","HUC_10")) %>%
  select(-geometry)

# plot time series
ggplot(all_municipal, aes(year,mgal)) +
  geom_line() +
  geom_point() +
  facet_wrap(~HUC_10) +
  theme_bw()

# join data to hucs
ga_municipal <- clipped_hucs %>%
  full_join(municipal, by="HUC_10")

ggplot(filter(ga_municipal, year==2010)) +
  geom_sf(data=clipped_hucs$geometry) +
  geom_sf(aes(fill=log10(mgal)),alpha=0.5) +
  coord_sf(datum = NA) +
  scale_fill_viridis_c() +
  theme_void()

# mapview(ga_municipal,zcol="mgal",legend=TRUE)

st_write(ga_municipal, "data/ga_municipal/ga_municipal.shp")


