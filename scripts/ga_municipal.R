library(tidyverse)
library(feather)
library(sf)

# load ACF study area
clipped_hucs <- st_read("data/clipped_hucs/clipped_hucs.shp") 

# load data from Andy and Tara
municipal <- read_csv("data/ga_nwis/nwis_volumes_gross_bock.csv") %>%
  dplyr::select(lon=DEC_LONG_VA, lat=DEC_LAT_VA, year, mgd=annual_val, HUC_10=HUC10_Fin) %>%
  dplyr::mutate(mgal = mgd*365) %>%
  st_as_sf(coords=c('lon','lat'), crs=4269) %>%
  group_by(year,HUC_10) %>%
  dplyr::summarize(mgal=sum(mgal)) %>%
  st_transform(crs=st_crs(clipped_hucs)) %>%
  st_intersection(st_union(clipped_hucs))

mapview(clipped_hucs) + mapview(filter(municipal,year==2010),zcol="mgal",legend=TRUE)

# build dataframe with all years and HUC 10s
all_municipal <- expand.grid(year=min(municipal$year):max(municipal$year),
                             HUC_10=base::unique(clipped_hucs$HUC_10)) %>%
  left_join(municipal,by=c("year","HUC_10")) %>%
  st_set_geometry(NULL)

# plot time series
ggplot(all_municipal, aes(year,mgal)) +
  geom_line() +
  geom_point() +
  facet_wrap(~HUC_10) +
  theme_bw()

# join data to hucs
ga_municipal <- clipped_hucs %>%
  full_join(all_municipal, by="HUC_10") 

# mapview(ga_municipal,zcol="mgal",legend=TRUE)

st_write(ga_municipal, "data/ga_municipal/ga_municipal.shp")


