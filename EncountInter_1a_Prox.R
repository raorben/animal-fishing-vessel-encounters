library(dplyr)
library(ggplot2)
library(lubridate)
library(adehabitatLT)
library(wildlifeDI)

# this code takes about a day to run!!! (could try to parallel process the loop)


#bring in GFW Functions
# Go find more data from .csvs
# Directory with daily GFW .csv files
if(Sys.info()[7]=="rachaelorben") GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/" ##RAO
if(Sys.info()[7]=="rachaelorben") userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'

source(paste0(userdir,"/Analysis/scripts/Functions_AlbatrossGFW.R"))

#AIS data from boats
boats<-read.csv(paste0(userdir,"/GFW/allyear_vessel_overlap.csv"))
head(boats)
#Bird GPS data
alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr.rds"))
head(alllocs)
#Potential encounters at 80kms
inters<-read.csv(paste0(userdir,"/Analysis/compileddata/Albatross_BoatInteractions_whilefishing_80km_mmsi_wAlbID_2018-11-14.csv"))
inters$dt<-as.character(inters$date)
inters$date<-as.Date(inters$dt)
head(inters)

#housekeeping
boats$dt<-as.character(boats$timestamp)
boats$datetime_GMT<-ymd_hms(boats$dt)
tz(boats$datetime_GMT)<-"GMT"
boats$date<-as.character(date(boats$datetime_GMT))
boats$lon360<-wrap360(boats$lon)
names(boats)
#AIS timestamp interval
boats<-boats%>%
  group_by(mmsi, date)%>%
  mutate(tdiff=as.numeric(datetime_GMT)-as.numeric(lag(datetime_GMT)))
b1<-boats%>%group_by(mmsi, date)%>%
  summarise(tdiff1=mean(tdiff, na.rm=TRUE))
mean(b1$tdiff1/60/60, na.rm=TRUE)
sd(b1$tdiff1/60/60, na.rm=TRUE)

#boatsinters<-left_join(boats,inters,by=c("date"="date","mmsi"="mmsi"))
alllocs$date<-as.character(date(alllocs$datetime))
inters$groupID<-inters %>% group_indices(mmsi,albatross_ID) #creates a new id for each group

inters<-inters%>%group_by(groupID)%>%   
  filter(Dist2Boat<80)%>%
  mutate(tdiff=as.numeric(date)-as.numeric(lag(date)),
         gap_hour=tdiff>1, # find times when there is a gap > 1 day
         gap_hour=ifelse(is.na(gap_hour),0,gap_hour), #fill NAs
         gapID=(cumsum(gap_hour)), 
         # gap_hour is T/F so cumsum is adding 1 for each T aka giving a index number to each gap
         # gapID=ifelse(gap_hour==T,gapID,""), # Make any gapIDs for which gaps_hours are F an empty string
         # gapID=ifelse(!is.na(gapID),gapID,""),# Make any gapIDs that are NAs an empty string
         events=paste0(as.character(groupID),".",gapID)) %>%
  arrange(events)%>%
  dplyr::select(-tdiff,-gapID)

inters%>%filter(groupID==7) 
length(unique(inters$events))


eventIDs<-unique(inters$events)
noboatdata<-NULL
prox.long<-NULL
prox.sum<-NULL

for (i in 1: length(eventIDs)){
  id<-eventIDs[i]
  inters_info<-inters%>%filter(events==id)

duration<-as.numeric(max(inters_info$date)-min(inters_info$date))+1
  datemin<-min(inters_info$date)
  
  bird<-alllocs%>%filter(ID==inters_info$albatross_ID[1])%>%
    filter(date>=min(inters_info$date))%>%
    filter(date<=max(inters_info$date))
  
  boat<-boats%>%filter(mmsi==inters_info$mmsi[1])%>%
    filter(date>=min(inters_info$date))%>%
    filter(date<=max(inters_info$date))
  
  if(nrow(boat)==0) {noboatdata<-rbind(noboatdata,inters_info); next }
  
# interaction day information
nAlb<-nrow(bird)
durBird<-max(bird$datetime)-min(bird$datetime)

boat<- boat %>%distinct(mmsi,datetime_GMT,.keep_all = T)
nBt<-nrow(boat)
durBoat<-max(boat$datetime_GMT)-min(boat$datetime_GMT)

b<-data_frame(lat=bird$lat,lon=bird$lon360,dataset="bird")
t<-data_frame(lat=boat$lat,lon=boat$lon360,dataset="boat")

D<-NULL
for (k in 1:nrow(b)){
  for (j in 1:nrow(t)){
    d<-argosfilter::distance(lat1 = b$lat[k],lat2 = t$lat[j],lon1 = b$lon[k], lon2 = t$lon[j])
    D<-rbind(D,d)
  }}
minDist_realpts<-min(D)
dataset<-rbind(b,t)

#Switch to UTM - find UTM of boat since bird is typically much longer track
zone<-min(find_UTM_zone(t$lon, t$lat))

dataset_sp<-SpatialPointsDataFrame(coords = cbind(wrap360(dataset$lon),dataset$lat),
                                   data = dataset,
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))

dataset_utm<-spTransform(dataset_sp,CRSobj = CRS(paste0("+proj=utm +zone=",zone," +units=m +ellps=WGS84")))

dataset$x<-coordinates(dataset_utm)[,1]
dataset$y<-coordinates(dataset_utm)[,2]

#plot(dataset$x,dataset$y,'l')

# make ltraj (adehabitat)
bird_lt<-adehabitatLT::as.ltraj(xy = cbind(dataset%>%filter(dataset=="bird")%>%dplyr::select(x),
                                           dataset%>%filter(dataset=="bird")%>%dplyr::select(y)),
                                date = bird$datetime,
                                id = bird$ID,
                                typeII = T,slsp="remove",infolocs = bird)
bird_lt<-adehabitatLT::redisltraj(l = bird_lt,u = 10*60,type = "time") 


boat_lt<-adehabitatLT::as.ltraj(xy = cbind(dataset%>%filter(dataset=="boat")%>%dplyr::select(x),
                                           dataset%>%filter(dataset=="boat")%>%dplyr::select(y)),
                                date = boat$datetime_GMT,
                                id = boat$mmsi,
                                typeII = T,slsp="remove",infolocs = boat,
                                proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
boat_lt<-adehabitatLT::redisltraj(l = boat_lt,u = 10*60,type = "time") 

#plot(bird_lt)
#plot(boat_lt)

overL<-checkTO(bird_lt,boat_lt)
time_search<-overL$TOend-overL$TOstart


if (overL$TO==TRUE){

  prox.3km<-Prox(bird_lt, boat_lt, tc=5*60, dc=1000*3)#tc= timeframe of matching (sec), dc=distance threshold (m)
  prox.30km<-Prox(bird_lt, boat_lt, tc=5*60, dc=1000*30)#tc= timeframe of matching (sec), dc=distance threshold (m)
  prox.80km<-Prox(bird_lt, boat_lt, tc=5*60, dc=1000*80)#tc= timeframe of matching (sec), dc=distance threshold (m)
  prox.df <- Prox(bird_lt, boat_lt,  tc=5*60,local=TRUE)
  min.dist<-min(prox.df$prox)
  n=nrow(prox.df)# this can be used to calculate duration (hours) of overlap at each threshold: 
  time80_min<-n*prox.80km*10
  time30_min<-n*prox.30km*10
  time3_min<-n*prox.3km*10
  
  if (prox.30km>0){
    tmin_30=min(prox.df$date1[prox.df$prox<80*1000])
    tmax_30=max(prox.df$date1[prox.df$prox<80*1000])
  }
  if (prox.30km==0){
    tmin_30=NA; tmax_30=NA
  }
  if (prox.3km>0){
    tmin_3=min(prox.df$date1[prox.df$prox<3*1000])
    tmax_3=max(prox.df$date1[prox.df$prox<3*1000])
  }
  if (prox.3km==0){
    tmin_3=NA; tmax_3=NA
  }
  prox.info<-data.frame(eventID=id,mmsi=boat$mmsi[1],
                        birdID=bird$ID[1],
                        species=bird$species[1],
                        nrow_boat=nAlb,dur_Bird=durBird,
                        time3_min,time30_min,time80_min, zone,nrow_boat=nrow(boat),
                        tmin_30,tmax_30,tmin_3, tmax_3,min.dist)
  prox.sum<-bind_rows(prox.sum,prox.info)
  #  n_overlap*10/60*prox.100km
  
  prox.df$proxkm<-prox.df$prox/1000
  prox.df$mmsi<-boat$mmsi[1]
  prox.df$birdID<-bird$ID[1]
  prox.df$species<-bird$species[1]
  prox.long<-rbind(prox.long,prox.df)
}
}

dt<-Sys.Date()
saveRDS(noboatdata,paste0(userdir,"/Analysis/compileddata/missingboatdata_prox_",dt,".rda"))
        saveRDS(prox.long,paste0(userdir,"/Analysis/compileddata/proxlong_",dt,".rda"))
                saveRDS(prox.sum,paste0(userdir,"/Analysis/compileddata/proxsummary_",dt,".rda"))
                
                noboatdata<-readRDS(paste0(userdir,"/Analysis/compileddata/missingboatdata_prox.rda"))
                prox.long<-readRDS(paste0(userdir,"/Analysis/compileddata/proxlong.rda"))
                prox.sum<-readRDS(paste0(userdir,"/Analysis/compileddata/proxsummary.rda"))
colnames(prox.sum)
                
                
                LAAL<-prox.sum%>%filter(species=="LAAL")%>%filter(time30_min!=0)
                BFAL<-prox.sum%>%filter(species=="BFAL")%>%filter(time30_min!=0)
                STAL<-prox.sum%>%filter(species=="STAL")%>%filter(time30_min!=0)
unique(STAL$birdID)                
                
            
prox.long.3km<-prox.long%>%filter(proxkm<3)
names(prox.long.3km)  
prox.long.3km$date<-date(prox.long.3km$date1)
a<-prox.long.3km%>%group_by(species,date,mmsi,birdID)%>%summarise(mindist=min(proxkm))
ggplot()+
  geom_boxplot(data=a,aes(x=birdID, y=mindist))

# hourly data LAAL & BFAL -------------------------------------------------

#bring in GFW Functions
# Go find more data from .csvs
# Directory with daily GFW .csv files
if(Sys.info()[7]=="rachaelorben") GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/" ##RAO
if(Sys.info()[7]=="rachaelorben") userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'

source(paste0(userdir,"/Analysis/scripts/Functions_AlbatrossGFW.R"))
locs<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_EEZ_lh_FAO.rds"))
birdIDs<-locs%>%group_by(ID.x,species)%>%summarise(n=n())
birdIDs$ID<-as.character(birdIDs$ID.x)
names(birdIDs)

boats<-read.csv(paste0(userdir,"/GFW/allyear_vessel_overlap.csv"))
alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_60minbytrip.rds"))
inters<-read.csv(paste0(userdir,"/Analysis/compileddata/HrLABFAL_Albatross_BoatInteractions_whilefishing_80km_mmsi_wAlbID_2019-07-01.csv"))
#BOATnALBIESd_hres<-read.csv(paste0(userdir,"/Analysis/compileddata/Albatross_BoatInteractions_whilefishing_80km_mmsi_wAlbID_2018-11-14.csv"))

inters$dt<-as.character(inters$date)
inters$date<-as.Date(inters$dt)

inters$alb_ID<-as.character(inters$albatross_ID)
inters<-left_join(inters, birdIDs%>%ungroup()%>%dplyr::select(species,ID), 
                  by=c("alb_ID"="ID"))
inters<-inters%>%filter(species!="STAL")


boats$dt<-as.character(boats$timestamp)
boats$datetime_GMT<-ymd_hms(boats$dt)
tz(boats$datetime_GMT)<-"GMT"
boats$date<-as.character(date(boats$datetime_GMT))
boats$lon360<-wrap360(boats$lon)

#boatsinters<-left_join(boats,inters,by=c("date"="date","mmsi"="mmsi"))
alllocs$date<-as.character(date(alllocs$datetime))
inters$groupID<-inters %>% group_indices(mmsi,albatross_ID) #creates a new id for each group
inters<-inters%>%group_by(groupID)%>%   
  filter(Dist2Boat<80)%>%
  mutate(tdiff=as.numeric(date)-as.numeric(lag(date)),
         gap_hour=tdiff>1, # find times when there is a gap > 1 day
         gap_hour=ifelse(is.na(gap_hour),0,gap_hour), #fill NAs
         gapID=(cumsum(gap_hour)), 
         # gap_hour is T/F so cumsum is adding 1 for each T aka giving a index number to each gap
         # gapID=ifelse(gap_hour==T,gapID,""), # Make any gapIDs for which gaps_hours are F an empty string
         # gapID=ifelse(!is.na(gapID),gapID,""),# Make any gapIDs that are NAs an empty string
         events=paste0(as.character(groupID),".",gapID)) %>%
  arrange(events)%>%
  dplyr::select(-tdiff,-gapID)



inters%>%filter(groupID==7) 
length(unique(inters$events))


eventIDs<-unique(inters$events)
noboatdata<-NULL
prox.long<-NULL
prox.sum<-NULL

for (r in 1: length(eventIDs)){
  i<-eventIDs[r]
  inters_info<-inters%>%filter(events==i)
  
  duration<-as.numeric(max(inters_info$date)-min(inters_info$date))+1
  datemin<-min(inters_info$date)
  
  bird<-alllocs%>%filter(ID==inters_info$albatross_ID[1])%>%
    filter(date>=min(inters_info$date))%>%
    filter(date<=max(inters_info$date))
  
  if(nrow(bird)==0){next}
  boat<-boats%>%filter(mmsi==inters_info$mmsi[1])%>%
    filter(date>=min(inters_info$date))%>%
    filter(date<=max(inters_info$date))
  
  if(nrow(boat)==0) {noboatdata<-rbind(noboatdata,inters_info); next }
  
  # interaction day information
  nAlb<-nrow(bird)
  durBird<-max(bird$datetime)-min(bird$datetime)
  
  boat<- boat %>%distinct(mmsi,datetime_GMT,.keep_all = T)
  nBt<-nrow(boat)
  durBoat<-max(boat$datetime_GMT)-min(boat$datetime_GMT)
  
  b<-data_frame(lat=bird$lat,lon=bird$lon360,dataset="bird")
  t<-data_frame(lat=boat$lat,lon=boat$lon360,dataset="boat")
  
  D<-NULL
  for (k in 1:nrow(b)){
    for (j in 1:nrow(t)){
      d<-argosfilter::distance(lat1 = b$lat[k],lat2 = t$lat[j],lon1 = b$lon[k], lon2 = t$lon[j])
      D<-rbind(D,d)
    }}
  minDist_realpts<-min(D)
  dataset<-rbind(b,t)
  
  #Switch to UTM - find UTM of boat since bird is typically much longer track
  zone<-min(find_UTM_zone(t$lon, t$lat))
  
  dataset_sp<-SpatialPointsDataFrame(coords = cbind(wrap360(dataset$lon),dataset$lat),
                                     data = dataset,
                                     proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))
  
  dataset_utm<-spTransform(dataset_sp,CRSobj = CRS(paste0("+proj=utm +zone=",zone," +units=m +ellps=WGS84")))
  
  dataset$x<-coordinates(dataset_utm)[,1]
  dataset$y<-coordinates(dataset_utm)[,2]
  
  #plot(dataset$x,dataset$y,'l')
  
  # make ltraj (adehabitat)
  bird_lt<-adehabitatLT::as.ltraj(xy = cbind(dataset%>%filter(dataset=="bird")%>%dplyr::select(x),
                                             dataset%>%filter(dataset=="bird")%>%dplyr::select(y)),
                                  date = bird$datetime,
                                  id = bird$ID,
                                  typeII = T,slsp="remove",infolocs = bird)
  bird_lt<-adehabitatLT::redisltraj(l = bird_lt,u = 10*60,type = "time") 
  
  
  boat_lt<-adehabitatLT::as.ltraj(xy = cbind(dataset%>%filter(dataset=="boat")%>%dplyr::select(x),
                                             dataset%>%filter(dataset=="boat")%>%dplyr::select(y)),
                                  date = boat$datetime_GMT,
                                  id = boat$mmsi,
                                  typeII = T,slsp="remove",infolocs = boat,
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  boat_lt<-adehabitatLT::redisltraj(l = boat_lt,u = 10*60,type = "time") 
  
  #plot(bird_lt)
  #plot(boat_lt)
  
  overL<-checkTO(bird_lt,boat_lt)
  time_search<-overL$TOend-overL$TOstart
  
  
  if (overL$TO==TRUE){
    prox.3km<-Prox(bird_lt, boat_lt, tc=5*60, dc=1000*3)#tc= timeframe of matching (sec), dc=distance threshold (m)
    prox.30km<-Prox(bird_lt, boat_lt, tc=5*60, dc=1000*30)#tc= timeframe of matching (sec), dc=distance threshold (m)
    prox.80km<-Prox(bird_lt, boat_lt, tc=5*60, dc=1000*80)#tc= timeframe of matching (sec), dc=distance threshold (m)
    prox.df <- Prox(bird_lt, boat_lt,  tc=5*60,local=TRUE)
    n=nrow(prox.df)# this can be used to calculate duration (hours) of overlap at each threshold: 
    time80_min<-n*prox.80km*10
    time30_min<-n*prox.30km*10
    time3_min<-n*prox.3km*10
    
    if (prox.30km>0){
      tmin_30=min(prox.df$date[prox.df$prox<80*1000])
      tmax_30=max(prox.df$date[prox.df$prox<80*1000])
    }
    if (prox.30km==0){
      tmin_30=NA; tmax_30=NA
    }
    if (prox.3km>0){
      tmin_3=min(prox.df$date[prox.df$prox<3*1000])
      tmax_3=max(prox.df$date[prox.df$prox<3*1000])
    }
    if (prox.3km==0){
      tmin_3=NA; tmax_3=NA
    }
    prox.info<-data.frame(eventID=i,mmsi=boat$mmsi[1],
                          birdID=bird$ID[1],
                          species=bird$species[1],
                          nrow_boat=nAlb,dur_Bird=durBird,
                          time3_min,time30_min,time80_min, zone,nrow_boat=nrow(boat),
                          tmin_30,tmax_30,tmin_3, tmax_3)
    prox.sum<-bind_rows(prox.sum,prox.info)
    #  n_overlap*10/60*prox.100km
    
    prox.df$proxkm<-prox.df$prox/1000
    prox.df$mmsi<-boat$mmsi[1]
    prox.df$birdID<-bird$ID[1]
    prox.df$species<-bird$species[1]
    prox.long<-rbind(prox.long,prox.df)
  }
}

saveRDS(noboatdata,paste0(userdir,"/Analysis/compileddata/Albie1hr_missingboatdata_prox.rda"))
saveRDS(prox.long,paste0(userdir,"/Analysis/compileddata/Albie1hr_proxlong.rda"))
saveRDS(prox.sum,paste0(userdir,"/Analysis/compileddata/Albie1hr_proxsummary.rda"))


noboatdata<-readRDS(paste0(userdir,"/Analysis/compileddata/Albie1hr_missingboatdata_prox.rda"))
prox.long<-readRDS(paste0(userdir,"/Analysis/compileddata/Albie1hr_proxlong.rda"))
prox.sum<-readRDS(paste0(userdir,"/Analysis/compileddata/Albie1hr_proxsummary.rda"))
colnames(prox.sum)


LAAL<-prox.sum%>%filter(species=="LAAL")%>%filter(time30_min!=0)
BFAL<-prox.sum%>%filter(species=="BFAL")%>%filter(time30_min!=0)
#STAL<-prox.sum%>%filter(species=="STAL")%>%filter(time30_min!=0)
#unique(STAL$birdID)                

BFAL$interYN<-"No"
BFAL$interYN[BFAL$time3_min>0]<-"Yes"
BFAL%>%group_by(interYN)%>%summarise(n=n())

LAAL$interYN<-"No"
LAAL$interYN[LAAL$time3_min>0]<-"Yes"
LAAL%>%group_by(interYN)%>%summarise(n=n())

