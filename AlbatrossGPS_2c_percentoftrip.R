library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(sp)
library(adehabitatLT)

wrap360 = function(lon) {
  lon360<-ifelse(lon<0,lon+360,lon)
  return(lon360)
}
if(Sys.info()[7]=="rachaelorben") {
  GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/"
  userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'}

alltracks<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr.rds"))

a<-alltracks%>%group_by(species,UniID)%>%summarise(n=n_distinct(TripNum))
colnames(alltracks)

alltracksPT<-NULL
for (i in 1:nrow(a)){
  unitrip<-alltracks%>%filter(UniID==a$UniID[i])
  
  if (nrow(unitrip)==1){unitrip$sum_time_min<-NA; 
  unitrip$cumsum_min<-NA;
  unitrip$pertrip<-NA;
  unitrip$tripdur<-NA;
  alltracksPT<-rbind(alltracksPT,unitrip)}
  
  if (nrow(unitrip)==1){next}
  
  tdiff<-data.frame(tdiff=as.numeric(unitrip$datetime-lag(unitrip$datetime)))
  unitrip$sum_time_min<-sum(tdiff$tdiff,na.rm = TRUE)
  
  tdiff[is.na(tdiff$tdiff)==FALSE,]
  
  unitrip$cumsum_min<-c(NA,cumsum(tdiff$tdiff[is.na(tdiff$tdiff)==FALSE]))
  unitrip$pertrip<-c(NA,unitrip$cumsum_min[2:(nrow(unitrip))]/sum(tdiff$tdiff,na.rm = TRUE))
  #unitrip%>%filter(is.na(cumsum_min)==TRUE)
  
  unitrip$tripdur<-max(unitrip$datetime)-min(unitrip$datetime)
  
  alltracksPT<-rbind(alltracksPT,unitrip)
}

nrow(alltracks)
nrow(alltracksPT)
saveRDS(alltracksPT,paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_Pertrip.rds"))
alltracksPT<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_Pertrip.rds"))
names(alltracksPT)
narow<-alltracksPT%>%filter(is.na(cumsum)==TRUE)

ggplot()+
  geom_path(data=alltracksPT%>%filter(species!="STAL")%>%
               filter(pertrip>0)%>%filter(pertrip<1),aes(y=lat, x=lon360,color=pertrip))+
  facet_wrap(~species)

ggplot()+
  geom_histogram(data=alltracksPT%>%filter(species!="STAL"),aes(x=pertrip))
