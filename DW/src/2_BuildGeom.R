#***********************************************************************
## 3/12/2018, Andrew R. Bock, Colorado Water Science Center, abock@usgs.gov
## 
## This script builds geometry from water use sites
##
##
## This software is provided "AS IS."
#***********************************************************************
setwd("//igskahcwgsdeli2/studies/ACF_WaterUse18")
# Import custom functions
source("src/func.R")

options(scipen=999)

# Read in HUC10 Layer
huc10<-rgdal::readOGR(dsn="data/HUC",layer="HUC10",use_iconv = T)

wellXY_ALLC<-read.csv("results/Sites/GW_ALLC_sites.csv")
#wellXY_ALLC<-feather::read_feather("results/Sites/ALLC_sites.feather")
# Convert data frame into a spatial points object with same coordinate system as the HUC12
sp::coordinates(wellXY_ALLC)<- ~DEC_LONG_VA+DEC_LAT_VA
sp::proj4string(wellXY_ALLC)<-sp::CRS(sp::proj4string(huc10))

# Perform Point in polygon analysis for wells and huc12s
# Determine if the tabular Huc10 matches the spatial huc10
a.data<-sp::over(wellXY_ALLC,huc10[,"HUC_10"])
# Add field to the points
wellXY_ALLC$HUC10_sp<-as.character(a.data$HUC_10)
# Does the tabular HUC10 match the spatial HUC10
wellXY_ALLC$Right<-ifelse(wellXY_ALLC$HUC10 == as.numeric(wellXY_ALLC$HUC10_sp),1,0)
# The "Right" field determines if the tabular HUC10 ("Right" = 0)
#  is the same as spatial HUC10 ("Right" = 1)
wellXY_ALLC<-wellXY_ALLC[!is.na(wellXY_ALLC$Right),]
rgdal::writeOGR(wellXY_ALLC,dsn="results/Sites",layer="wellXY_ALLC",driver="ESRI Shapefile",
      overwrite_layer=T)

write.table(wellXY_ALLC@data,"results/Sites/ALLC_sites_SU.csv",col.names=T,row.names=F,sep=",")
feather::write_feather(wellXY_ALLC@data,"results/Sites/ALLC_sites_SU.feather")

# Perform the same exercise for EPWS Sites
wellXY_EPWS<-read.csv("results/Sites/GW_EPWS_sites.csv",stringsAsFactors = F)
#wellXY_EPWS<-feather::read_feather("results/Sites/EPWS_sites.feather")
#wellXY_EPWS<-wellXY_EPWS[wellXY_EPWS$Dups==0,]
sp::coordinates(wellXY_EPWS)<- ~DEC_LONG_VA+DEC_LAT_VA
sp::proj4string(wellXY_EPWS)<-sp::CRS(sp::proj4string(huc10))

# Point in polygon analysis for wells and huc12s
b.data<-sp::over(wellXY_EPWS,huc10[,"HUC_10"])
wellXY_EPWS$HUC10_sp<-as.character(b.data$HUC_10)
wellXY_EPWS$Right<-ifelse(wellXY_EPWS$HUC10 == as.numeric(wellXY_EPWS$HUC10_sp),1,0)
wellXY_EPWS<-wellXY_EPWS[!is.na(wellXY_EPWS$Right),]

rgdal::writeOGR(wellXY_EPWS,dsn="results/Sites",layer="wellXY_EPWS",driver="ESRI Shapefile",
                overwrite_layer=T)


# write.table(wellXY_EPWS@data,"results/Sites/EPWS_sites_SU.csv",col.names=T,row.names=F,sep=",")
# feather::write_feather(wellXY_EPWS@data,"results/Sites/EPWS_sites_SU.feather")

