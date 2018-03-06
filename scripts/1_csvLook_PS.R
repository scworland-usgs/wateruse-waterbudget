library(tidyr) #https://rpubs.com/bradleyboehmke/data_wrangling
library(magrittr)
library(dplyr)
library(feather)
# force R not to use sci notation
options(scipen=999)

# Reads, organizes, subsets water use and permit databases
setwd("//IGSKAHCWUSCXBOX/ACF_WaterUse18")

# NWIS Data (Aggregate Use)
nwis<-read.csv("data/WU/ga_nwis.csv",stringsAsFactors = F)
nwis$fromID<-paste(nwis$from_agency_cd,"-",nwis$from_site_no,sep="")
nwis$toID<-paste(nwis$to_agency_cd,"-",nwis$to_site_no,sep="")
# Permits Database (Site-specific information)
perm<-read.csv("data/WU/ga-permits.csv",stringsAsFactors = F)
perm$fromID<-paste(perm$AGENCY_CD,"-",perm$SITE_NO,sep="")

# Join NWIS to SWUDS
join<-inner_join(nwis,perm,by="fromID")
# Get the number of unique IDs
uniqID<-unique(join$fromID)

# How many permit sites have addresses, all NWIS sites have addresses
haveAdd<-perm[!perm$ADDR_LINE_1_TX=="",] # 11 AG Sites, and 83 GW sites

# Subset to fields that initially might helpful
permSub<-perm[,colnames(perm) %in% c("AGENCY_CD","SITE_NO","SITE_TP_CD","DEC_LAT_VA","DEC_LONG_VA","HUC_CD","PERMIT_TX","PERMIT_CD","permID")]
permSub<-permSub[!permSub$PERMIT_CD == "NPDES" & permSub$SITE_TP_CD %in% c("AG","GW"),] # subset to ALLC and EPDS Based on Nancy's email

# Add a field (Permit_short) that is the first 7 digts of an aggregate site if nnn-nnnn format, 
# if GA site, use existing permit number
permSub$PERMIT_SHORT<-ifelse(startsWith(permSub$PERMIT_TX,"GA"),permSub$PERMIT_TX,substring(permSub$PERMIT_TX,1,8))

# separate by aggregate sites (AG) and individual sites (GW)
AG<-permSub[permSub$SITE_TP_CD=="AG",]
#write_feather(AG,"results/Sites/AG_sites.feather")
#df<-read_feather("results/Sites/AG_sites.feather")
write.csv(AG,"results/Sites/AG_sites.csv",quote=F,row.names=F)

# Get the GW Sites
GW<-permSub[permSub$SITE_TP_CD=="GW",]
# Separate the two by permit code
ALLC<-GW[GW$PERMIT_CD=="ALLC",]
EPWS<-GW[GW$PERMIT_CD=="EPWS",]

# Calcualte the the median lat/long for all permit numbers
ALLC_DF<-ALLC %>% group_by(DEC_LAT_VA) %>% mutate(medianLat=median(DEC_LAT_VA,na.rm=T),
                                                         medianLon=median(DEC_LONG_VA,na.rm=T))

# Grouping by dec_lat_va, as some of the GA and USGS sites share locations but 
# have different aggregate IDs.
# how many non-NAs by permit_tx for latitude
ALLC_DF<-ALLC_DF %>% group_by(medianLat) %>% mutate(latNA=sum(!is.na(DEC_LAT_VA))/n())
# how many non-huc 12s by permit_tx
ALLC_DF<-ALLC_DF %>% group_by(medianLat) %>% mutate(huc12=sum(nchar(HUC_CD)>10)/n())

write.csv(ALLC_DF,"results/Sites/GW_ALLC_sites.csv",quote=F,row.names=F)

# how many non-NAs by permit_tx for latitude
EPWS_DF<-EPWS %>% group_by(medianLat) %>% mutate(latNA=sum(!is.na(DEC_LAT_VA))/n())
# how many non-huc 12s by permit_tx
EPWS_DF<-EPWS_DF %>% group_by(medianLat) %>% mutate(huc12=sum(nchar(HUC_CD)>10)/n())

# Which EPWS permits are from the same site as ALLC
EPWS_DF$Dups<-ifelse(EPWS$DEC_LAT_VA %in% ALLC$DEC_LAT_VA,1,0)
# which EPWS does not have an AG Site
EPWS_DF$In_AG<-ifelse(EPWS_DF$PERMIT_SHORT %in% AG$PERMIT_SHORT,1,0)
EPWS_DF$In_SiteNO<-ifelse(EPWS_DF$SITE_NO %in% nwis$site_no,1,0)
write.csv(EPWS_DF,"results/Sites/GW_EPWS_sites.csv",quote=F,row.names=F)

# Unused pieces
#**************************
## how many records with lat/longs
#permLat<-permSub[which(!is.na(perm$DEC_LAT_VA)),]
## how many records with HUCs
#permHUC<-permSub[which(nchar(perm$HUC_CD)>10),]

# numMaster<-newDF%>% group_by(PERMIT_SHORT) %>% mutate(nMas=sum(SITE_TP_CD=="AG"))
# noMaster<-numMaster[numMaster$nMas==0 & numMaster$PERMIT_CD=="EPWS",]
# 
# numChild<-newDF %>% group_by(PERMIT_SHORT) %>% mutate(nChild=sum(SITE_TP_CD=="GW"))
# noChild<-numChild[numChild$nChild==0 & numChild$PERMIT_CD=="EPWS",]
# newDF<-GW %>% group_by(PERMIT_SHORT) %>%  mutate(noSites=n())

#fullDescrip<-newDF[newDF$huc12 == 1,]
# write.csv(fullDescrip,"results/Sites/Tier1_GW.csv",quote=F,row.names=F)
# fullParents<-AG[AG$PERMIT_SHORT %in% fullDescrip$PERMIT_SHORT,]
# write.csv(fullParents,"results/Sites/Tier1_AG.csv",quote=F,row.names=F)
# 
# partDescrip<-newDF[newDF$huc12 >= 0.75 & newDF$huc12 < 1,]
# write.csv(partDescrip,"results/Sites/Tier2_GW.csv",quote=F,row.names=F)
# partParents<-AG[AG$PERMIT_SHORT %in% partDescrip$PERMIT_SHORT,]
# write.csv(partParents,"results/Sites/Tier2_AG.csv",quote=F,row.names=F)
# 
# HalfDescrip<-newDF[newDF$huc12 >= 0.5 & newDF$huc12 < 0.75,]
# write.csv(HalfDescrip,"results/Sites/Tier3_GW.csv",quote=F,row.names=F)
# HalfParents<-AG[AG$PERMIT_SHORT %in% HalfDescrip$PERMIT_SHORT,]
# write.csv(HalfParents,"results/Sites/Tier3_AG.csv",quote=F,row.names=F)
# 
# restDescrip<-newDF[newDF$huc12 > 0 & newDF$huc12 < 0.5,]
# write.csv(restDescrip,"results/Sites/Tier4_GW.csv",quote=F,row.names=F)
# restParents<-AG[AG$PERMIT_SHORT %in% restDescrip$PERMIT_SHORT,]
# write.csv(restParents,"results/Sites/Tier4_AG.csv",quote=F,row.names=F)
# 
# noDescrip<-newDF[newDF$huc12 == 0 ,]
# write.csv(noDescrip,"results/Sites/Tier5_GW.csv",quote=F,row.names=F)
# noParents<-AG[AG$PERMIT_SHORT %in% noDescrip$PERMIT_SHORT,]
# write.csv(noParents,"results/Sites/Tier5_AG.csv",quote=F,row.names=F)

