library(raster)
library(velox)

# load ACF study area
wbd_acf <- st_read("data/acf_hucs/acf_wbd.shp")

# load clipped hucs
clipped_hucs <- st_read("data/clipped_hucs/clipped_hucs.shp") 

# load county-level data
counties_acf <- st_read("data/acf_counties/acf_county.shp") 

acf_cnty_fips <- unique(counties_acf$cnty_fips)

# load NWIS county data
cnty <- read_tsv("data/county_water_use_2010.txt") %>%
  dplyr::select(cnty_fips="FIPS", municipal="PS-Wtotl",
         thermoelectric="PT-Wtotl", irrigation="IR-IrTot") %>%
  mutate_at(vars(municipal,thermoelectric,irrigation),function(x){x*365}) %>%
  filter(cnty_fips %in% acf_cnty_fips) %>%
  gather(type,nwis_value,-cnty_fips) %>%
  mutate(year=2010)

# irrigation ---------------------------------------
# aggregate county irrigation to hucs
cnty_irr <- counties_acf %>%
  left_join(cnty) %>%
  dplyr::filter(type=="irrigation") %>%
  dplyr::select(-type)

cnty_huc_irr <- st_interpolate_aw(cnty_irr['nwis_value'],clipped_hucs, extensive=TRUE) %>%
  mutate(HUC_10 = as.character(clipped_hucs$HUC_10)) %>%
  dplyr::select(HUC_10,nwis_value) %>%
  st_set_geometry(NULL)
  
ga_irrigation <- st_read("data/ga_irrigation/shapefile/ga_irrigation.shp") %>%
  dplyr::filter(year=="2010") %>%
  dplyr::select(HUC_10,site_value=mgal) %>%
  left_join(cnty_huc_irr, by='HUC_10') %>%
  gather(type,value,-geometry,-HUC_10)

# calculate centroids and values
centers <- ga_irrigation %>%
  st_centroid() %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2],
         value=round(value)) %>%
  st_set_geometry(NULL)
  
ggplot() +
  geom_sf(data=st_geometry(counties_acf), fill="white") +
  geom_sf(data=wbd_acf, color="grey",alpha=0.5) +
  geom_sf(data=ga_irrigation, aes(fill=value), color='white',size=0.1) +
  scale_fill_viridis_c("mgal", option='D', labels = scales::comma) +
  geom_text(data=centers, aes(x,y,label=value),color='white',size=2.5) +
  facet_wrap(~type, ncol=2) +
  labs(x="", y="") +
  theme_bw()

# only counties within ACF
counties_within_acf <- st_read("data/acf_counties/counties_within_acf.shp")

ga_irr2010 <- read_feather("data/GA_irrigation/ga_irr.feather") %>%
  filter(year==2010) %>%
  mutate(depth_in = ifelse(depth_in==-9999,NA,depth_in),
       mgal = ((1/12*(depth_in)*(acres*43560))*7.48052)/1e6) %>%
  group_by(lon,lat,year) %>%
  summarize(mgal = sum(mgal, na.rm=T))

# CRS from shapefile Jamie Painter provided
irr_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-83.5 +x_0=0 +y_0=0 
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# reproject data and find points in ACF study area
irr_cnty <- st_as_sf(ga_irr2010, coords=c("lon","lat"),crs=irr_crs) %>%
  st_transform(crs=st_crs(counties_within_acf)) %>%
  st_intersection(counties_within_acf) %>%
  group_by(GEOID) %>%
  summarize(site_value=sum(mgal)) %>%
  st_set_geometry(NULL) %>%
  left_join(st_set_geometry(select(cnty_irr,GEOID,nwis_value),NULL), by="GEOID") %>%
  right_join(counties_within_acf, by="GEOID") %>%
  st_sf() %>%
  select(GEOID, nwis_value, site_value) %>%
  gather(type,value,-geometry,-GEOID)

centers <- irr_cnty %>%
  st_centroid() %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2],
         value=round(value)) %>%
  st_set_geometry(NULL)

ggplot() +
  geom_sf(data=st_geometry(clipped_hucs), fill="white") +
  geom_sf(data=irr_cnty, aes(fill=value), color='white',size=0.1) +
  scale_fill_viridis_c("mgal", option='D', labels = scales::comma) +
  geom_text(data=centers, aes(x,y,label=value),color='white',size=2.5) +
  facet_wrap(~type, ncol=2) +
  labs(x="", y="") +
  theme_bw()

# municipal ---------------------------------------
# load population raster data
pop <- velox('data/gridded_pop/clipped_pop.tif')

# aggregate population by county
cnty_pop <- pop$extract(st_transform(counties_acf,pop$crs),fun=function(x) sum(x,na.rm=T),df=TRUE)

counties_acf$pop <- cnty_pop$out

# aggregate population by partial hucs
cnty_huc <- counties_acf %>%
  dplyr::select(cnty_fips=GEOID,cnty_pop = pop) %>%
  st_intersection(clipped_hucs) %>%
  st_cast('MULTIPOLYGON') %>%
  st_transform(pop$crs)

huc_pop <- pop$extract(cnty_huc,fun=function(x) sum(x,na.rm=T),df=TRUE)

# add population and calculate pop fraction
cnty_huc_pop <- cnty_huc %>%
  dplyr::mutate(huc_pop = round(huc_pop$out,3),
                p = round(huc_pop/cnty_pop,3))

# county municipal (public supply)
cnty_huc_ps <- cnty_huc_pop %>%
  left_join(cnty) %>%
  filter(type=="municipal") %>%
  mutate(cnty_nwis = nwis_value,
         nwis_value = cnty_nwis*p) %>%
  #mutate(gpcd = (nwis_value*1e6/365)/huc_pop)
  dplyr::select(-acres,-area_km,-year,-type) %>%
  group_by(HUC_10) %>%
  dplyr::summarize(nwis_value = sum(nwis_value)) %>%
  st_set_geometry(NULL)
  
ga_municipal <- st_read("data/ga_municipal/ga_municipal.shp") %>%
  filter(year==2010) %>%
  dplyr::select(HUC_10,site_value=mgal) %>%
  left_join(cnty_huc_ps, by='HUC_10') %>%
  gather(type,value,-geometry,-HUC_10)

centers <- ga_municipal %>%
  st_centroid() %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2],
         value=round(value)) %>%
  st_set_geometry(NULL)

ggplot() +
  geom_sf(data=st_geometry(counties_acf), fill="white") +
  geom_sf(data=wbd_acf, color="grey",alpha=0.5) +
  geom_sf(data=ga_municipal, aes(fill=value), color='white',size=0.1) +
  scale_fill_viridis_c("mgal", option='D', labels = scales::comma) +
  geom_text(data=centers, aes(x,y,label=value),color='white',size=2.5) +
  facet_wrap(~type, ncol=2) +
  labs(x="", y="") +
  theme_bw()

# only counties within ACF

# reproject data and find points in ACF study area
cnty_municipal <- cnty %>%
  filter(type=="municipal") %>%
  select(GEOID=cnty_fips,nwis_value)

ps_cnty <- read_csv("data/ga_nwis/nwis_volumes_gross_bock.csv") %>%
  dplyr::select(lon=DEC_LONG_VA, lat=DEC_LAT_VA, year, 
                mgd=annual_val, HUC_10=HUC10_Fin,permit=PERMIT_SHORT) %>%
  #dplyr::mutate(mgal = mgd*365) %>%
  st_as_sf(coords=c('lon','lat'), crs=4269) %>%
  filter(year==2010) %>%
  st_transform(crs=st_crs(counties_within_acf)) %>%
  st_intersection(counties_within_acf) %>%
  group_by(GEOID) %>%
  summarize(site_value=sum(mgal)) %>%
  st_set_geometry(NULL) %>%
  left_join(cnty_municipal, by="GEOID") %>%
  right_join(counties_within_acf, by="GEOID") %>%
  st_sf() %>%
  select(GEOID, nwis_value, site_value) %>%
  gather(type,value,-geometry,-GEOID)

centers <- ps_cnty %>%
  st_centroid() %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2],
         value=round(value)) %>%
  st_set_geometry(NULL)

ggplot() +
  geom_sf(data=st_geometry(clipped_hucs), fill="white") +
  geom_sf(data=ps_cnty, aes(fill=value), color='white',size=0.1) +
  scale_fill_viridis_c("mgal", option='D', labels = scales::comma) +
  geom_text(data=centers, aes(x,y,label=value),color='white',size=2.5) +
  facet_wrap(~type, ncol=2) +
  labs(x="", y="") +
  theme_bw()

# thermoelectric ---------------------------------------

# county thermoelectric
cnty_thermo <- cnty %>%
  filter(type=="thermoelectric") %>%
  filter(nwis_value > 0)

value_0313000801 = sum(cnty_thermo$nwis_value[cnty_thermo$cnty_fips!="13321"])
value_0313000606 = cnty_thermo$nwis_value[cnty_thermo$cnty_fips=="13321"]

ga_thermo <- st_read("data/ga_thermo/shapefile/ga_thermo.shp") %>%
  mutate(nwis_value = ifelse(HUC_10 == "0313000801",value_0313000801,
                             ifelse(HUC_10 == "0313000606",value_0313000606,NA)),
         site_value = mgal_wthrl) %>%
  filter(year==2010) %>%
  dplyr::select(HUC_10,nwis_value,site_value) %>%
  gather(type,value,-geometry,-HUC_10)
  
centers <- ga_thermo %>%
  st_centroid() %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2],
         value=round(value)) %>%
  st_set_geometry(NULL)

ggplot() +
  geom_sf(data=st_geometry(counties_acf), fill="white") +
  geom_sf(data=wbd_acf, color="grey",alpha=0.5) +
  geom_sf(data=ga_thermo, aes(fill=value), color='white',size=0.1) +
  scale_fill_viridis_c("mgal", option='D', labels = scales::comma) +
  geom_text(data=centers, aes(x,y,label=value),color='white',size=2.5) +
  facet_wrap(~type, ncol=2) +
  labs(x="", y="") +
  theme_bw()
  

