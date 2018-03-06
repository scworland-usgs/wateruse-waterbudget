library(tidyr) #https://rpubs.com/bradleyboehmke/data_wrangling
library(magrittr)
library(dplyr)
library(sf)
library(data.table)
library(rgdal)

setwd("//IGSKAHCWUSCXBOX/ACF_WaterUse18")

huc12<-rgdal::readOGR(dsn="data/HUC",layer="HUC12")
wellXY<-read.csv("results/Sites/GW_ALLC_sites.csv")
sp::coordinates(wellXY)<- ~DEC_LONG_VA+DEC_LAT_VA
sp::proj4string(wellXY)<-CRS(proj4string(huc12))

# Point in polygon analysis for wells and huc12s
a.data<-sp::over(wellXY,huc12[,"HUC_12"])
wellXY$HUC12<-a.data$HUC_12
wellXY$HUC_Str<-paste("0",wellXY$HUC_CD,sep="")
wellXY$Right<-ifelse(wellXY$HUC_Str == wellXY$HUC12,1,0)
wellXY<-wellXY[!is.na(wellXY$Right),]
#writeOGR(wellXY,dsn="results/Sites",layer="wellXY_ALLC",driver="ESRI Shapefile",
#         overwrite_layer=T)

wellXY<-read.csv("results/Sites/GW_EPWS_sites.csv")
wellXY<-wellXY[wellXY$Dups==0,]
sp::coordinates(wellXY)<- ~DEC_LONG_VA+DEC_LAT_VA
sp::proj4string(wellXY)<-CRS(proj4string(huc12))

# Point in polygon analysis for wells and huc12s
a.data<-sp::over(wellXY,huc12[,"HUC_12"])
wellXY$HUC12<-a.data$HUC_12
wellXY$HUC_Str<-paste("0",wellXY$HUC_CD,sep="")
wellXY$Right<-ifelse(wellXY$HUC_Str == wellXY$HUC12,1,0)
wellXY<-wellXY[!is.na(wellXY$Right),]
writeOGR(wellXY,dsn="results/Sites",layer="wellXY_EPWS",driver="ESRI Shapefile",
         overwrite_layer=T)

# Shapefile where wrong huc12 attribution is checked (Right == 0 to Right==1)
wellXY2<-readOGR("results/Sites","wellXY_EPWS")
wellXY2_0<-wellXY2[wellXY2$Right==0,]
AG_sub<-AG[which(AG$PERMIT_SHORT %in% wellXY2_0$PERMIT_S),]



#************************************
# Simple Features Code
# #https://gis.stackexchange.com/questions/222978/lon-lat-to-simple-features-sfg-and-sfc-in-rsetwd("d:/abock/WaterCensus18")
# inTier<-read.csv("data/Sites/Tier1.csv")
# inTierDT<-data.table(inTier)
# 
# DT_sf = st_as_sf(inTierDT, coords = c("DEC_LONG_VA", "DEC_LAT_VA"), 
#                  crs = 4326, agr = "constant")
# plot(DT_sf)
# 
# # Build more complex geometries from a single feature
# sub<-DT_sf[which(DT_sf$PERMIT_SHORT=="121-0015"),]
# # https://github.com/r-spatial/sf/issues/231
# subLine<-st_linestring(sub$geometry,"LINESTRING")
