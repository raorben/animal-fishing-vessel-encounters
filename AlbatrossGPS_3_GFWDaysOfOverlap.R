library(dplyr)
library(ggplot2)
library(lubridate)

#bring in GFW Functions
# Go find more data from .csvs
# Directory with daily GFW .csv files
if(Sys.info()[7]=="rachaelorben") GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/" ##RAO
if(Sys.info()[7]=="rachaelorben") userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'

source(paste0(userdir,"/Analysis/scripts/Functions_AlbatrossGFW.R"))

w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("USA","Russia","China","Japan","Mexico","Canada","North Korea","South Korea","Taiwan","Mongolia"),]

boats<-read.csv(paste0(userdir,"/GFW/allyear_vessel_overlap.csv"))
alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr.rds"))
inters<-read.csv(paste0(userdir,"/Analysis/compileddata/Albatross_BoatInteractions_whilefishing_80km_mmsi_wAlbID_2018-11-14.csv"))

inters$dt<-as.character(inters$date)
inters$date<-as.Date(inters$dt)

boats$dt<-as.character(boats$timestamp)
boats$datetime_GMT<-ymd_hms(boats$dt)
tz(boats$datetime_GMT)<-"GMT"
boats$date<-as.character(date(boats$datetime_GMT))
boats$lon360<-wrap360(boats$lon)

#boatsinters<-left_join(boats,inters,by=c("date"="date","mmsi"="mmsi"))
alllocs$date<-as.character(date(alllocs$datetime))
inters$groupID<-inters %>% group_indices(mmsi,albatross_ID)
inters<-inters%>%group_by(groupID)%>%   
  filter(Dist2Boat<80)%>%
  mutate(tdiff=as.numeric(date)-as.numeric(lag(date)),
  gap_hour=tdiff>1, # find times when there is a gap > 60 minutes
  gap_hour=ifelse(is.na(gap_hour),0,gap_hour), #fill NAs
  gapID=(cumsum(gap_hour)), 
  # gap_hour is T/F so cumsum is adding 1 for each T aka giving a index number to each gap
  # gapID=ifelse(gap_hour==T,gapID,""), # Make any gapIDs for which gaps_hours are F an empty string
  # gapID=ifelse(!is.na(gapID),gapID,""),# Make any gapIDs that are NAs an empty string
  events=paste0(as.character(groupID),".",gapID)) %>%
  dplyr::select(-tdiff,-gap_hour,-gapID)
 


inters%>%filter(groupID==7) 
length(unique(inters$events))

eventIDs<-unique(inters$events)
noboatdata<-NULL
for (i in eventIDs){

  inters_info<-inters%>%filter(events==i)
  duration<-as.numeric(max(inters_info$date)-min(inters_info$date))
  datemin<-min(inters_info$date)
  
  birdtrack<-alllocs%>%filter(ID==inters_info$albatross_ID[1])%>%
    filter(date>=min(inters_info$date))%>%
    filter(date<=max(inters_info$date))
  
  boattrack<-boats%>%filter(mmsi==inters_info$mmsi[1])%>%
    filter(date>=min(inters_info$date))%>%
    filter(date<=max(inters_info$date))
  
  if(nrow(boattrack)==0) {noboatdata<-rbind(noboatdata,inters_info); next }
  
    albID=inters_info$albatross_ID
    mmsiB=inters_info$mmsi
    
    species<-birdtrack$species[1]
    colony<-birdtrack$colony[1]
    
    lats<-c(boattrack$lat,birdtrack$lat)
    lons<-c(boattrack$lon360,birdtrack$lon360)
    
    # plot<-
    #   ggplot()+
    #   geom_polygon(data=w2hr_sub,
    #                aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
    #   geom_path(data = birdtrack, aes(x = (lon360), y = lat, color="purple"), color="purple",size = 0.5) +
    #     geom_point(data = birdtrack, aes(x = (lon360), y = lat, color="purple"), color="purple",size = 0.5) +
    #   geom_path(data = boattrack, aes(x = (lon360), y = lat, color="black"), color="black",size = 0.5) +
    #     geom_point(data = boattrack, aes(x = (lon360), y = lat, color="black"),color="black",size = 0.5) +
    #   #beginning
    #   geom_point(data = birdtrack[1,], aes(x = lon360, y = lat), color="green",size = 2) +
    #   geom_point(data = boattrack[1,], aes(x = lon360, y = lat),color="green",size = 2) +
    #   #end
    #   geom_point(data = birdtrack[nrow(birdtrack),], aes(x = lon360, y = lat), color="red",size = 2) +
    #   geom_point(data = boattrack[nrow(boattrack),], aes(x = lon360, y = lat),color="red",size = 2) +
    #   coord_fixed(ratio=1.7,xlim = c(min(lons),max(lons)),ylim=c(min(lats),max(lats)))+
    #   xlab("Longitude360")+
    #   ylab("Latitude")+
    #   ggtitle(paste0(species,albID," boat:",mmsiB," dur(d):",duration," beg:",datemin))+
    #   theme_bw()+
    #   #theme(legend.title=element_blank())+
    #   NULL
    #   ggsave(plot, file=paste0(userdir, "/Analysis/PLOTS/interactionplots_multidays/",duration,"_",species,"_",i,"_albie",albID[1],"_mmsi",mmsiB[1],"_",datemin,".png"),width = 8,height=8,units = "in")
  }


# multiple boats / day ----------------------------------------------------

daysw_multiboats<-inters%>%group_by(albatross_ID,date)%>%
  summarise(boatNum=n_distinct(mmsi))%>%
  filter(boatNum>1)


for (i in 1:nrow(daysw_multiboats)){
  
  inters_info<-daysw_multiboats[i,]
  inters_mb<-inters%>%filter(albatross_ID==inters_info$albatross_ID)%>%
    filter(date==inters_info$date)
  dateE<-min(inters_info$date)
  
  birdtrack<-alllocs%>%filter(ID==inters_info$albatross_ID[1])%>%
    filter(date==inters_info$date)
  
  BT<-NULL
  for (k in 1:nrow(inters_mb)){
  boattrack<-boats%>%filter(mmsi==inters_mb$mmsi[k])%>%
    filter(date==min(inters_mb$date[k]))
  BT<-rbind(BT, boattrack)
  }
  
  if(nrow(boattrack)==0) {noboatdata<-rbind(noboatdata,inters_info); next }
  
  albID=inters_info$albatross_ID
  mmsiB=inters_mb$mmsi
  
  species<-birdtrack$species[1]
  colony<-birdtrack$colony[1]
  
  lats<-c(BT$lat,birdtrack$lat)
  lons<-c(BT$lon360,birdtrack$lon360)
  # plot<-
  #   ggplot()+
  #   geom_polygon(data=w2hr_sub,
  #                  aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  #   geom_path(data = birdtrack, aes(x = lon360, y = lat), color="purple",size = 1) +
  #   geom_point(data = birdtrack, aes(x =lon360, y = lat), color="red",size = 0.5) +
  #   geom_path(data = BT, aes(x =lon360, y = lat, group=mmsi,color= as.factor(mmsi)), size = 0.5) +
  #   geom_point(data = BT, aes(x =lon360, y = lat, group=mmsi,color= as.factor(mmsi)), size = 0.5) +
  #   xlab("Longitude360")+
  #   ylab("Latitude")+
  #   ggtitle(paste0(species,albID," day:",dateE))+
  #   coord_fixed(ratio=1.7,xlim = c(min(lons),max(lons)),ylim=c(min(lats),max(lats)))+
  #   theme_classic()+
  #   theme(legend.title=element_blank())+
  #   NULL
  # ggsave(plot, file=paste0(userdir, "/Analysis/PLOTS/interactionplots_multiboats/",k,"_",species,"_albie",albID[1],"_",dateE,".png"),width = 8,height=8,units = "in")
}

