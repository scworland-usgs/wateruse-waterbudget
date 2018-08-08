library(raster)
library(velox)
library(tidyverse)
library(sf)
library(stringr)
library(purrr)

# downscale county estimates ---------------------
# load county-level data
cnty <- read_tsv("data/county_water_use_2010.txt") %>%
  dplyr::select(GEOID="FIPS", municipal="PS-Wtotl",
                thermoelectric="PT-Wtotl", irrigation="IR-IrTot") %>%
  mutate_at(vars(municipal,thermoelectric,irrigation),function(x){x*365}) %>%
  gather(type,nwis_value,-GEOID) %>%
  mutate(year=2010) %>%
  filter(type=="municipal")

# load GA counties and join
ga_counties <- st_read("data/county_shapefiles/cb_2013_us_county_500k.shp") %>%
  filter(STATEFP == 13) %>%
  left_join(cnty) %>%
  dplyr::select(GEOID, nwis_value)

# load GA WBD
ga_wbd <- st_read('data/nhd_wbd/WBDSnapshot_National.shp') %>%
  filter(str_detect(STATES, 'GA')) %>%
  group_by(HUC_10) %>%
  summarize(acres = sum(AreaHUC12))

# write huc10 list to csv for Sam Saxe
# ga_hucs <- ga_wbd %>%
#   mutate(HUC_10 = as.character(HUC_10)) %>%
#   st_set_geometry(NULL)
# 
# write_csv(ga_hucs, "data/ga_hucs/ga_wbd.csv")

# mapview(ga_counties, pane=NULL) + ga_wbd

# load population raster data
pop <- velox('data/ga_pop_rasters/GA_pop.tif')

sw_weighted_interp <- function(from,value,to,to_group_id,weight_var,sum=TRUE){
  
  # several checks
  if(inherits(weight_var,"VeloxRaster")==FALSE) stop("'weight_var' must be of class VeloxRaster")
  if(inherits(from,"sf")==FALSE) stop("'from' must be of class sf")
  #if(all(st_is(from, "MULTIPOLYGON"))==FALSE) stop("'from' geometry type must be 'MULTIPOLYGON'")
  if(inherits(to,"sf")==FALSE) stop("'to' must be of class sf")
  #if(all(st_is(to, "MULTIPOLYGON"))==FALSE) stop("'from' geometry type must be 'MULTIPOLYGON'")
  
  if(sum==T){# use sum
    # aggregate weighting variable to "from" geometry
    from_weight_var <- weight_var$extract(st_transform(from,weight_var$crs),fun=function(x) sum(x,na.rm=T),df=TRUE)
    
    # add weight var to "from" geometry
    from$from_weight_var <- from_weight_var$out
    
    # find intersection between "from" and "to"
    from_to <- from %>%
      st_intersection(to) %>%
      st_cast('MULTIPOLYGON') %>%
      st_transform(weight_var$crs)
    
    # aggregate weighting variable to "to" geometry
    to_weight_var <- weight_var$extract(from_to,fun=function(x) sum(x,na.rm=T),df=TRUE)
    
    # calculate weight and new value
    result <- from_to %>%
      dplyr::mutate(to_weight_var = to_weight_var$out,
                    p = to_weight_var/from_weight_var,
                    to_value = round(!!sym(value)*p,3)) %>%
      group_by(!!sym(to_group_id)) %>%
      dplyr::summarize(to_value = sum(to_value, na.rm=T))
    
  }else{#use mean
    # aggregate weighting variable to "from" geometry
    from_weight_var <- weight_var$extract(st_transform(from,weight_var$crs),fun=function(x) mean(x,na.rm=T),df=TRUE)
    
    # add weight var to "from" geometry
    from$from_weight_var <- from_weight_var$out
    
    # find intersection between "from" and "to"
    from_to <- from %>%
      st_intersection(to) %>%
      st_cast('MULTIPOLYGON') %>%
      st_transform(weight_var$crs)
    
    # aggregate weighting variable to "to" geometry
    to_weight_var <- weight_var$extract(from_to,fun=function(x) mean(x,na.rm=T),df=TRUE)
    
    # calculate weight and new value
    result <- from_to %>%
      dplyr::mutate(to_weight_var = to_weight_var$out,
                    p = to_weight_var/from_weight_var,
                    to_value = round(!!sym(value)*p,3)) %>%
      group_by(!!sym(to_group_id)) %>%
      dplyr::summarize(to_value = mean(to_value, na.rm=T))
  }
  
  return(result)
}

huc_ps <- sw_weighted_interp(value="nwis_value", from=ga_counties, to=ga_wbd, 
                             to_group_id="HUC_10", weight_var=pop)

# plot cnty ---> HUC
ggplot(huc_ps) +
  geom_sf(aes(fill=to_value)) +
  coord_sf(datum = NA) +
  scale_fill_viridis_c() +
  theme_void()

mapview(cnty_huc_ps, zcol="to_value")

# aggregate sites ------------------------
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
  group_by(year,HUC_10) %>%
  dplyr::summarize(mgal=sum(mgal)) 

# build dataframe with all years and HUC 10s
all_municipal <- expand.grid(year=min(municipal$year):max(municipal$year),
                             HUC_10=as.character(base::unique(ga_wbd$HUC_10)), 
                             stringsAsFactors = F) %>%
  data.frame() %>%
  left_join(municipal,by=c("year","HUC_10"))

# join data to hucs
ga_all_municipal <- ga_wbd %>%
  full_join(all_municipal, by="HUC_10") %>%
  filter(year==2010)

# plot sites ---> HUC
ggplot(ga_all_municipal) +
  geom_sf(aes(fill=mgal)) +
  coord_sf(datum = NA) +
  scale_fill_viridis_c() +
  theme_void()

