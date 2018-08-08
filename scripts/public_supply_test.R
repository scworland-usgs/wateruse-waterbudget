
# load hucs
wbd8 <- st_read('data/nhd_wbd/WBDSnapshot_National.shp') %>%
  filter(str_detect(STATES, 'GA')) %>%
  group_by(HUC_8) %>%
  summarize() 

# load county level data
cnty_municipal <- read_tsv("data/county_water_use_2010.txt") %>%
  dplyr::select(cnty_fips="FIPS", municipal="PS-Wtotl",
                thermoelectric="PT-Wtotl", irrigation="IR-IrTot") %>%
  mutate_at(vars(municipal,thermoelectric,irrigation),function(x){x*365}) %>%
  gather(type,nwis_value,-cnty_fips) %>%
  mutate(year=2010) %>%
  filter(type=="municipal") %>%
  select(GEOID=cnty_fips,nwis_value)

# load ga nwis data
ga_nwis <- read_csv("data/ACF_WaterUse18/data/WU/ga_nwis.csv") %>%
  select(lat=from_dec_lat_va,lon=from_dec_long_va,HUC_8=from_huc_cd,
         type=from_nat_water_use_nm,state=from_state_cd,county=from_county_cd,
         year,annual_val:dec_val) %>%
  mutate(mgal = annual_val*365,
         GEOID = paste0(state,county)) %>%
  filter(type=="Water Supply" & year==2010) %>%
  group_by(GEOID) %>%
  summarize(ga_nwis=sum(mgal))

# load permit data
ga_permits <- read_csv("data/ACF_WaterUse18/data/WU/ga-permits.csv") %>%
  select(lat=DEC_LAT_VA,lon=DEC_LONG_VA,type=NAT_WATER_USE_NM,
         permit=PERMIT_TX) %>%
  na.omit() %>%
  st_as_sf(coords=c("lon","lat"),crs=4269)

# join to huc8s
wbd_nwis <- wbd8 %>%
  left_join(ga_nwis, by="HUC_8")

# join to county shapefiles
county_nwis <- st_read("data/county_shapefiles/cb_2013_us_county_500k.shp") %>%
  right_join(ga_nwis, by="GEOID") %>%
  left_join(cnty_municipal, by="GEOID") %>%
  select(GEOID,ga_nwis,nwis_value) %>%
  #mutate(diff = nwis_value-ga_nwis)
  gather(type,value,-geometry,-GEOID)

# calculate centroids and values
centers <- county_nwis %>%
  st_centroid() %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2],
         value=round(value)) %>%
  st_set_geometry(NULL)

ggplot(data=county_nwis) +
  geom_sf(aes(fill=value), color='white',size=0.1) +
  scale_fill_viridis_c("mgal", option='D', labels = scales::comma) +
  geom_text(data=centers, aes(x,y,label=value),color='white',size=2.5) +
  facet_wrap(~type, ncol=2) +
  labs(x="", y="") +
  theme_bw()

mapview(county_nwis,zcol="mgd",legend=T) + mapview(clipped_hucs, col.region="white")
