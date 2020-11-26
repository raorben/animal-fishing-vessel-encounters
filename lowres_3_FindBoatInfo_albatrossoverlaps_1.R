library(dplyr)
library(ggplot2)

# Go find more data from .csvs
# Directory with daily GFW .csv files
if(Sys.info()[7]=="rachaelorben") GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/daily_csvs" ##RAO
if(Sys.info()[7]=="rachaelorben") userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'
alllocsG<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_LR1_","2018-11-17",".rda"))
summary(alllocsG)

#bring in GFW Functions
source(paste0(userdir,"/Analysis/scripts/Functions_AlbatrossGFW.R"))

# extract additional data -------------------------------------------------

BOATnALBIES12<-extractBoatInfo(alllocsG=alllocsG%>%filter(fishingF!="no boat"),
                               yr=2012,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir,resolution="LR")
saveRDS(BOATnALBIES12, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2012interactions_LR1_boatinfo.rda"))
BOATnALBIES12<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2012interactions_LR1_boatinfo.rda"))


BOATnALBIES13<-extractBoatInfo(alllocsG=alllocsG%>%filter(fishingF!="no boat"),
                               yr=2013,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir,resolution="LR")
saveRDS(BOATnALBIES13, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2013interactions_LR1_boatinfo.rda"))


BOATnALBIES14<-extractBoatInfo(alllocsG=alllocsG%>%filter(fishingF!="no boat"),
                               yr=2014,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir,resolution="LR")
saveRDS(BOATnALBIES14, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2014interactions_LR1_boatinfo.rda"))


BOATnALBIES15<-extractBoatInfo(alllocsG=alllocsG%>%filter(fishingF!="no boat"),
                               yr=2015,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir,resolution="LR")
saveRDS(BOATnALBIES15, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2015interactions_LR1_boatinfo.rda"))


BOATnALBIES16<-extractBoatInfo(alllocsG=alllocsG%>%filter(fishingF!="no boat"),
                               yr=2016,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir,resolution="LR")
saveRDS(BOATnALBIES16, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2016interactions_LR1_boatinfo.rda"))


BOATnALBIES12<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2012interactions_LR1_boatinfo.rda"))
BOATnALBIES13<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2013interactions_LR1_boatinfo.rda"))
BOATnALBIES14<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2014interactions_LR1_boatinfo.rda"))
BOATnALBIES15<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2015interactions_LR1_boatinfo.rda"))
BOATnALBIES16<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2016interactions_LR1_boatinfo.rda"))

#all years
BOATnALBIES<-rbind(BOATnALBIES12,BOATnALBIES13,BOATnALBIES14,BOATnALBIES15,BOATnALBIES16)
dt<-Sys.Date()
saveRDS(BOATnALBIES, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_ALLinteractions_LR1_boatinfo_",dt,".rda"))

BOATnALBIES<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_ALLinteractions_LR1_boatinfo_2018-11-08.rda"))


# Summarize extracted data - data checking --------------------------------
a<-BOATnALBIES %>% dplyr::group_by(albatross_ID) %>% 
  dplyr::filter(Dist2Boat<10)%>%
  dplyr::summarize(n=n_distinct(mmsi))

b<-BOATnALBIES%>%dplyr::filter(Dist2Boat<5)
summary(b)

      
colnames(BOATnALBIES)
BOATnALBIESd<-BOATnALBIES%>%
  dplyr::filter(Dist2Boat<80)%>%
  dplyr::distinct(date,mmsi,albatross_ID,.keep_all = TRUE)%>%
  select(date,mmsi,albatross_ID,Dist2Boat)

BOATnALBIESd$year<-as.factor(year(BOATnALBIESd$date))
BOATnALBIESd%>%dplyr::group_by((year))%>%dplyr::summarise(n=n_distinct(mmsi))

dt<-Sys.Date()
write.csv(BOATnALBIESd,paste0(userdir,"/Analysis/compileddata/Albatross_BoatInteractions_whilefishing_80km_mmsi_wAlbID_",dt,".csv"))
saveRDS(BOATnALBIES, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_ALLinteractions_1_boatinfo_ed.rda"))




# Hourly interpolated LAAL & BFAL data ------------------------------------
# Go find more data from .csvs
# Directory with daily GFW .csv files
if(Sys.info()[7]=="rachaelorben") GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/daily_csvs" ##RAO
if(Sys.info()[7]=="rachaelorben") userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'
alllocsG<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Hr_LABFAL_Albatross_GFWbuff_LR1_2019-05-10.rda"))
summary(alllocsG)

#bring in GFW Functions
source(paste0(userdir,"/Analysis/scripts/Functions_AlbatrossGFW.R"))

# extract additional data -------------------------------------------------

BOATnALBIES12<-extractBoatInfo(alllocsG=alllocsG%>%filter(fishingF!="no boat"),
                               yr=2012,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir,resolution="LR")
saveRDS(BOATnALBIES12, file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_2012interactions_LR1_boatinfo.rda"))
BOATnALBIES12<-readRDS(file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_2012interactions_LR1_boatinfo.rda"))


BOATnALBIES13<-extractBoatInfo(alllocsG=alllocsG%>%filter(fishingF!="no boat"),
                               yr=2013,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir,resolution="LR")
saveRDS(BOATnALBIES13, file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_2013interactions_LR1_boatinfo.rda"))


BOATnALBIES14<-extractBoatInfo(alllocsG=alllocsG%>%filter(fishingF!="no boat"),
                               yr=2014,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir,resolution="LR")
saveRDS(BOATnALBIES14, file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_2014interactions_LR1_boatinfo.rda"))


BOATnALBIES15<-extractBoatInfo(alllocsG=alllocsG%>%filter(fishingF!="no boat"),
                               yr=2015,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir,resolution="LR")
saveRDS(BOATnALBIES15, file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_2015interactions_LR1_boatinfo.rda"))


BOATnALBIES16<-extractBoatInfo(alllocsG=alllocsG%>%filter(fishingF!="no boat"),
                               yr=2016,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir,resolution="LR")
saveRDS(BOATnALBIES16, file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_2016interactions_LR1_boatinfo.rda"))


BOATnALBIES12<-readRDS(file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_2012interactions_LR1_boatinfo.rda"))
BOATnALBIES13<-readRDS(file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_2013interactions_LR1_boatinfo.rda"))
BOATnALBIES14<-readRDS(file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_2014interactions_LR1_boatinfo.rda"))
BOATnALBIES15<-readRDS(file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_2015interactions_LR1_boatinfo.rda"))
BOATnALBIES16<-readRDS(file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_2016interactions_LR1_boatinfo.rda"))

#all years
BOATnALBIES<-rbind(BOATnALBIES12,BOATnALBIES13,BOATnALBIES14,BOATnALBIES15,BOATnALBIES16)
dt<-Sys.Date()
saveRDS(BOATnALBIES, file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_ALLinteractions_LR1_boatinfo_",dt,".rda"))

BOATnALBIES<-readRDS(file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_ALLinteractions_LR1_boatinfo_2018-11-08.rda"))


# Summarize extracted data - data checking --------------------------------
a<-BOATnALBIES %>% dplyr::group_by(albatross_ID) %>% 
  dplyr::filter(Dist2Boat<10)%>%
  dplyr::summarize(n=n_distinct(mmsi))

b<-BOATnALBIES%>%dplyr::filter(Dist2Boat<5)
summary(b)


colnames(BOATnALBIES)
BOATnALBIESd<-BOATnALBIES%>%
  dplyr::filter(Dist2Boat<80)%>%
  dplyr::distinct(date,mmsi,albatross_ID,.keep_all = TRUE)%>%
  select(date,mmsi,albatross_ID,Dist2Boat)

BOATnALBIESd$year<-as.factor(year(BOATnALBIESd$date))
BOATnALBIESd%>%dplyr::group_by((year))%>%dplyr::summarise(n=n_distinct(mmsi))

dt<-Sys.Date()
write.csv(BOATnALBIESd,paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatross_BoatInteractions_whilefishing_80km_mmsi_wAlbID_",dt,".csv"))
saveRDS(BOATnALBIES, file=paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatrossnointer_GFWbuff_ALLinteractions_1_boatinfo_ed.rda"))


BOATnALBIESd_hres<-read.csv(paste0(userdir,"/Analysis/compileddata/Albatross_BoatInteractions_whilefishing_80km_mmsi_wAlbID_2018-11-14.csv"))
locs<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_EEZ_lh_FAO.rds"))
birdIDs<-locs%>%group_by(ID.x,species)%>%summarise(n=n())
birdIDs$ID<-as.character(birdIDs$ID.x)
BOATnALBIESd_hres$ID<-as.character(BOATnALBIESd_hres$albatross_ID)

BOATnALBIESd_hres<-left_join(BOATnALBIESd_hres,birdIDs%>%select(ID,species), by=c("ID"="ID"))
head(BOATnALBIESd_hres)

nrow(BOATnALBIESd)

BOATnALBIESd$hourres_YN<-"Yes"

BOATnALBIESdHR<-BOATnALBIESd_hresID%>%filter(species!="STAL")
nrow(BOATnALBIESdHR)

BA<-left_join(BOATnALBIESdHR,BOATnALBIESd%>%
            select(date,mmsi,albatross_ID,hourres_YN),
          by=c("date"="date","mmsi"="mmsi","albatross_ID"="albatross_ID"))
BA$hourres_YN[is.na(BA$hourres_YN)==TRUE]<-"No"

BA%>%group_by(species,hourres_YN)%>%summarise(n=n())

notmatched<-BA%>%dplyr::filter(hourres_YN=="No")
notmatched%>%
  group_by(species)%>%
  summarise(dist2boat=mean(Dist2Boat),st=sd(Dist2Boat),n=n())
