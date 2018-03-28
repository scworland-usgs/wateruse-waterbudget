#setwd("//igskahcwgsdeli2/studies/ACF_WaterUse18/results/GW_ALLC_volumes/")
setwd("//igskahcwgsdeli2/studies/ACF_WaterUse18/results/GW_EPWS_volumes")

#volumes<-read.csv("//igskahcwgsdeli2/studies/ACF_WaterUse18/results/FinalALLCJoin.csv", dec = ".", row.names=NULL)
volumes<-read.csv("//igskahcwgsdeli2/studies/ACF_WaterUse18/results/FinalEPWSJoin.csv", dec = ".", row.names=NULL)

filenames<-""
#mainDir = "//igskahcwgsdeli2/studies/ACF_WaterUse18/results/GW_ALLC_volumes/"
mainDir = "//igskahcwgsdeli2/studies/ACF_WaterUse18/results/GW_EPWS_volumes"

#options(digits=15)#scipen = 999, 

for(volume in 1:nrow(volumes)){
  huc<-volumes[volume,"HUC10_Fin"]
  filename<-paste(format(volumes[volume,"t1_SITE_NO"], scientific=FALSE),"_",volumes[volume,"PERMIT_SHORT"],"_",format(volumes[volume,"t2_SITE_NO"], scientific = FALSE),sep = "")
  entries<-data.frame("year" = volumes[volume,"year"], "volume" = volumes[volume,"annual_val"])
 
  
    if(!dir.exists(file.path(mainDir, huc))) {
      dir.create(file.path(mainDir, huc))
    }

    if(filename %in% filenames) {
      dfRead<-read.csv(paste(huc,"/",filename,".csv",sep=""))
      all<-rbind(dfRead, entries) # rbind both data.frames
      # get only the non duplicate rows from the new data.frame
      nonDuplicate <- all[!duplicated(all)&c(rep(FALSE, dim(dfRead)[1]), rep(TRUE, dim(entries)[1])), ]
      write.table(nonDuplicate,paste(huc,"/",filename,".csv",sep=""),append = TRUE,col.names = FALSE,row.names=F,quote=F,sep = ",")
    } else {
      file.create(paste(huc,"/",filename,".csv",sep=""))
      write.table(entries,paste(huc,"/",filename,".csv",sep=""),append = FALSE,row.names=F,quote=F,sep = ",")
      filenames<-c(filenames, filename)
    }
}
