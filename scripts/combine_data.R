
library(tidyverse)
library(feather)
library(sf)

setwd("~/Documents/wu_waterbudget")

# load data for sectors
municipal <- read_feather("data/municipal.feather")
irrigation <- read_feather("data/irrigation.feather")
thermo <- read_feather("data/thermo.feather")

# combine data
data_all <- irrigation %>%
  select(huc12,huc10,huc8,cnty_fips,year,irrigation=mgal) %>%
  left_join(select(municipal,huc12,huc10,cnty_fips,huc8,year,municipal=mgal),
            by=c("huc12","huc10","huc8","cnty_fips","year")) %>%
  left_join(select(thermo,huc12,huc10,huc8,cnty_fips,year,thermoelectric=mgal_cons),
            by=c("huc12","huc10","huc8","cnty_fips","year")) %>%
  replace_na(list(irrigation=0,municipal=0,thermoelectric=0)) %>%
  group_by(huc12,year) %>%
  summarize_at(vars(municipal,irrigation,thermoelectric), funs(sum)) %>%
  filter(year %in% 2008:2012) %>%
  mutate(total = municipal+irrigation+thermoelectric) %>%
  mutate_at(vars(municipal,irrigation,thermoelectric,total), funs(round), digits=2) 

d_long <- data_all %>%
  gather(type,value,-huc12,-year) %>%
  filter(type != "total") %>%
  data.frame()

ggplot(d_long) + 
  geom_col(aes(year,value),position = "dodge") +
  facet_wrap(~type,ncol=1,scales="free_y") +
  labs(y="millions of gallons")

# load shapefiles
acf_huc <- st_read("data/acf_hucs/acf_wbd.shp") 
acf10 <- aggregate(acf_huc, list(acf_huc$HUC_10), function(x) x[1]) %>%
  select(huc10=HUC_10) 

# join data to geometry
acf10_data <- data_all %>%
  gather(source,mgal,-huc10,-year) %>%
  left_join(acf10,by="huc10") 

ggplot() + 
  theme_bw() +
  geom_sf(data=acf10_data, aes(fill=log10(mgal))) + 
  facet_grid(source~year) +
  scale_fill_viridis_c() +
  coord_sf(crs = st_crs(acf10_data), datum = NA) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

  geom_point(data = filter(irrigation,year==2009), aes(lon,lat), alpha=0.3,size=0.5) +
  geom_point(data = filter(municipal,year==2009), aes(lon,lat), alpha=0.7,size=2, color="red") +
  geom_point(data=filter(thermo,year==2009), aes(lon,lat), shape=23,fill="dodgerblue",size=4) 
  

plot(st_geometry(acf_huc), col = sf.colors(12, categorical = TRUE), border = 'grey', axes = TRUE)
# plot(acf_huc["ACRES"],key.pos = 1, axes = TRUE, key.size = lcm(1.3))
