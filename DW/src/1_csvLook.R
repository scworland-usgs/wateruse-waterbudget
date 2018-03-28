#***********************************************************************
## 3/12/2018, Andrew R. Bock, Colorado Water Science Center, abock@usgs.gov
## 
## This script links, parses, and summarizes pertinent variables from water use 
## volume and site information for the state of Georgia
##
##
## This software is provided "AS IS."
#***********************************************************************
# Prevent scientific notation and write value out in long form
options(scipen=999)

# Reads, organizes, subsets water use and permit databases
setwd("//igskahcwgsdeli2/studies/ACF_WaterUse18")

# NWIS Data (Aggregate Use)
nwis<-read.csv("data/WU/ga_nwis.csv",stringsAsFactors = F,colClasses="character")
#nwis<-readr::read_csv("data/WU/ga_nwis.csv",col_names=T)
# Add two fieds for from and To IDs
nwis$fromID<-paste(nwis$from_agency_cd,"-",nwis$from_site_no,sep="")
nwis$toID<-paste(nwis$to_agency_cd,"-",nwis$to_site_no,sep="")

# Permits Database (Site-specific information)
# Converted from XLS to CSV

#perm<-readr::read_csv("data/WU/ga-permits.csv",col_names=T)
perm<-read.csv("data/WU/ga-permits.csv",stringsAsFactors = F,colClasses="character")
# Combine agency and site_no to create a new field
perm$fromID<-paste(perm$AGENCY_CD,"-",perm$SITE_NO,sep="")
# Join NWIS to SWUDS based on fromID field
join<-dplyr::inner_join(nwis,perm,by="fromID")
# Get the unique IDs
uniqID<-unique(join$fromID)

# How many sites have physical addre
print (paste(dim(perm[!is.na(perm$ADDR_LINE_1_TX),])[1]," have addresses",sep=""))
# How many sites have city names
print (paste(dim(perm[!is.na(perm$CITY_NM),])[1]," have city names",sep="")) 
# How many sites have zip codes
print (paste(dim(perm[!is.na(perm$ZIP_CD),])[1]," have zip codes",sep=""))

# Subset to fields that initially might helpful
# permSub<-perm[,colnames(perm) %in% c("AGENCY_CD","SITE_NO","SITE_TP_CD","DEC_LAT_VA","DEC_LONG_VA","HUC_CD","PERMIT_TX","PERMIT_CD","permID")]
# Decided to keep all fields for the final files
permSub<-perm

# Subset to just AG and GW Sites, remove NPDES sites
# 2025 AG Sites, and 4398 GW Sites
# subset to ALLC and EPDS Based on Nancy's 11/26 email
permSub<-permSub[!permSub$PERMIT_CD == "NPDES" & permSub$SITE_TP_CD %in% c("AG","GW"),] 

# Add a field (Permit_short) that is the first 7 digts of an aggregate site if nnn-nnnn format, 
# if GA site (EPWS), use existing permit number, this is from Nancy's 11/26 emali
permSub$PERMIT_SHORT<-ifelse(startsWith(permSub$PERMIT_TX,"GA"),permSub$PERMIT_TX,substring(permSub$PERMIT_TX,1,8))

# separate by aggregate sites (AG) and individual sites (GW)
AG<-permSub[permSub$SITE_TP_CD=="AG",]
# Write out file for aggregate sites
write.csv(AG,"results/Sites/AG_sites.csv",row.names=T,quote=T)
feather::write_feather(AG,"results/Sites/AG_sites.feather")

# Get the GW Sites
GW<-permSub[permSub$SITE_TP_CD=="GW",]
# Create a field for HUC10 code
GW$HUC10<-ifelse(nchar(GW$HUC_CD>8),substr(GW$HUC_CD, 1, 10),GW$HUC_CD)
# Separate the ALLC and EPWS sites by permit code and write out
ALLC<-GW[GW$PERMIT_CD=="ALLC",]
write.csv(ALLC,"results/Sites/GW_ALLC_sites.csv",quote=T,row.names=T)
feather::write_feather(ALLC,"results/Sites/GW_ALLC_sites.feather")

# GA Sites (EPWS Sites)
EPWS<-GW[GW$PERMIT_CD=="EPWS",]
# Which EPWS permits are from the same site as ALLC
# Not sure what to do with this information except for document it
# Same lat/lon (same well location)
EPWS$Dups<-ifelse(EPWS$DEC_LAT_VA %in% ALLC$DEC_LAT_VA,1,0)
# Same master permit number
EPWS$In_AG<-ifelse(EPWS$PERMIT_SHORT %in% AG$PERMIT_SHORT,1,0)
# Same site number
EPWS$In_SiteNO<-ifelse(EPWS$SITE_NO %in% nwis$site_no,1,0)
write.csv(EPWS,"results/Sites/GW_EPWS_sites.csv",quote=T,row.names=T)
feather::write_feather(EPWS,"results/Sites/GW_EPWS_sites.feather")

