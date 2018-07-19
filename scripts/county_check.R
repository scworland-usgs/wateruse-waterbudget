
library(tidyverse)
library(sf)
library(lwgeom)
library(purrr)
library(feather)

# load data for sectors
municipal <- read_feather("data/municipal.feather")
irrigation <- read_feather("data/irrigation.feather")
thermo <- read_feather("data/thermo.feather")

# load ACF study area
acf_huc <- st_read("data/acf_hucs/acf_wbd.shp") %>%
  group_by(HUC_10) %>%
  summarize() 

# load cnty shapefile and join
counties <- st_read("data/county_shapefiles/cb_2013_us_county_500k.shp") 

counties_acf <- counties[lengths(st_intersects(counties, acf_huc)) > 0,] %>%
  mutate(cnty_fips=as.character(GEOID)) %>%
  mutate(area_km = as.numeric(st_area(geometry)/1e6))

# plot(st_geometry(counties_acf), col = sf.colors(12, categorical = TRUE), border = 'grey',
#      axes = TRUE)
# plot(st_geometry(acf_huc), pch = 3, col = NA, add = TRUE)


acf_cnty_fips <- unique(counties_acf$cnty_fips)

# load NWIS county data
cnty <- read_tsv("data/county_water_use_2010.txt") %>%
  select(cnty_fips="FIPS", municipal="PS-Wtotl",
         thermoelectric="PT-Wtotl", irrigation="IR-IrTot") %>%
  mutate_at(vars(municipal,thermoelectric,irrigation),function(x){x*365}) %>%
  filter(cnty_fips %in% acf_cnty_fips) %>%
  gather(type,nwis_value,-cnty_fips) %>%
  mutate(year=2010)

# combine data
county_check <- irrigation %>%
  select(huc12,huc10,huc8,cnty_fips,year,irrigation=mgal) %>%
  left_join(select(municipal,huc12,huc10,cnty_fips,huc8,year,municipal=mgal),
            by=c("huc12","huc10","huc8","cnty_fips","year")) %>%
  left_join(select(thermo,huc12,huc10,huc8,cnty_fips,year,thermoelectric=mgal_cons),
            by=c("huc12","huc10","huc8","cnty_fips","year")) %>%
  replace_na(list(irrigation=0,municipal=0,thermoelectric=0)) %>%
  group_by(cnty_fips,year) %>%
  summarize_at(vars(municipal,irrigation,thermoelectric), funs(sum)) %>%
  # filter(year %in% 2010) %>%
  mutate_at(vars(municipal,irrigation,thermoelectric), funs(round), digits=2) %>%
  gather(type,new_value,-cnty_fips, -year) %>%
  left_join(cnty,by=c("cnty_fips","type","year")) %>%
  mutate(nwis_value=round(nwis_value,2))

write_csv(county_check,"data/county_check.csv")

county_check_all <- county_check %>%
  group_by(year,type) %>%
  summarize(new_value=sum(new_value),
            nwis_value=sum(nwis_value))

ggplot(county_check_all) +
  geom_line(aes(year,new_value)) +
  geom_point(aes(year,nwis_value), color='red') +
  facet_wrap(~type,scales="free_y",ncol=1)




#### testing stuff -----

# sf irrigate
sf_irr <- ga_irr %>%
  st_as_sf(coords = c("lon","lat")) %>%
  st_geometry()

d <- st_union(sf_irr)
hull <- st_convex_hull(d)

clipped_hucs <- acf_huc %>%
  st_intersection(hull)

plot(d)
plot(hull, col="red", add= TRUE)

# load ACF study area and aggregate by huc10
acf_huc <- st_read("data/acf_hucs/acf_wbd.shp") %>%
  group_by(HUC_10) %>%
  summarize() %>%
  mutate(area_km = as.numeric(st_area(geometry)/1e6))

# load counties 
counties <- st_read("data/county_shapefiles/cb_2013_us_county_500k.shp") 

counties_acf <- counties[lengths(st_intersects(counties, acf_huc)) > 0,] %>%
  mutate(cnty_fips=as.character(GEOID)) %>%
  mutate(area_km = as.numeric(st_area(geometry)/1e6))

# calculate area of each huc10 for each county
hucs_counties <- acf_huc %>%
  st_intersection(counties_acf) %>%
  mutate(area_km=as.numeric(st_area(geometry)/1e6)) %>%
  group_by(GEOID) %>%
  mutate(cnty_area = sum(area_km),
         prop_area = round(area_km/cnty_area,3))
  
mapview(hucs_counties)


irr_huc10 <- irrigation %>%
  group_by(huc10,year) %>%
  summarize(mgal = sum(mgal))

# add irrigatgion to ACF hucs
acf_huc <- st_read("data/acf_hucs/acf_wbd.shp") %>%
  group_by(HUC_10) %>%
  summarize() %>%
  rename(huc10=HUC_10) %>%
  left_join(irr_huc10, by = "huc10") %>%
  filter(year=="2008")

plot(st_geometry(counties_acf), col = sf.colors(12, categorical = TRUE), border = 'grey',
     axes = TRUE)
plot(st_geometry(acf_huc), pch = 3, col = NA, add = TRUE)

hold <- st_interpolate_aw(acf_huc['mgal'], counties, extensive=TRUE)
plot(hold["mgal"])

int <- st_intersection(st_geometry(cnty_irr),st_geometry(clipped_hucs)) %>%
  st_cast("MULTIPOLYGON") 




#add in an area count column to the tibble (area of each arable poly in intersect layer)

int$area_km <- as.numeric(st_area(int$geometry)/1e6)

mapview(int)

int2 <- int %>%
  group_by(GEOID) %>%
  mutate(cnty_area = sum(area))
