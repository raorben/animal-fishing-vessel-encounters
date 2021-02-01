library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(sp)
library(adehabitatLT)
library(argosfilter)
#devtools::install_github("collectivemedia/tictoc")
library(tictoc)

wrap360 = function(lon) {
  lon360<-ifelse(lon<0,lon+360,lon)
  return(lon360)
}

matlab2POS = function(x,tz = "UTC") {
  days = x - 719529 #719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 #86400 seconds in a day
  return(as.POSIXct(secs,origin = "1970-1-1",tz = tz))#returns POSIXct object
}

if(Sys.info()[7]=="rachaelorben") userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'
fundir<-'/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/CommonFunctions/forR'

###reads in functions in function folder....
for(i in 1:length(list.files(fundir))) {
  source(list.files(fundir,full.names = T)[i])
}

tracksA<-readRDS(paste0(userdir,"/Analysis/compileddata/AlbatrossData_bytrip.rda"))
unique(tracksA$ID)
tracksA$ID<-gsub("I", "1", tracksA$ID)
tracksA$ID[tracksA$ID=="nest101"]<-10100

tracksA$year<-year(tracksA$datetime)
tracksA$speciesNum<-0
tracksA$Num[tracksA$species=="BFAL"]<-1
tracksA$Num[tracksA$species=="LAAL"]<-2
tracksA$Num[tracksA$species=="STAL"]<-3
# Adds in a unique identifier for each trip with year, capture number, and trip number
A<-str_pad(tracksA$ID, width=7, pad="0")
B<-str_pad(tracksA$TripNum, width=3, pad="0")
C<-str_pad(tracksA$speciesNum, width=1, pad="0")
tracksA$UniID<-paste0(tracksA$year,A,B,C)
unique(as.numeric(tracksA$UniID))
rm(A,B,C)

# Rediscretionize LAAL & BFAL tracks to 10 minutes -------------------------------------
library(xts)
tracks<-tracksA%>%filter(species!="STAL")
rm(tracksA)
#tracks$datetime_ns<-align.time(tracks$datetime, n=60)
head(tracks)

# Prep tracks
tracks<- tracks %>%
  distinct(ID,datetime,.keep_all = T) %>%
  arrange(ID,datetime) %>% 
  mutate(tdiff=as.numeric(datetime)-as.numeric(lag(datetime)),
         gap_hour=tdiff>120*60, # find times when there is a gap > 120 minutes
         gap_hour=ifelse(is.na(gap_hour),0,gap_hour), #fill NAs
         gapID=(cumsum(gap_hour)), # gap_hour is T/F so cumsum is adding 1 for each T aka giving a index number to each gap
         # gapID=ifelse(gap_hour==T,gapID,""), # Make any gapIDs for which gaps_hours are F an empty string
         # gapID=ifelse(!is.na(gapID),gapID,""),# Make any gapIDs that are NAs an empty string
         UniID_gap=paste0(as.character(UniID),gapID)) 

# make ltraj (adehabitat)
tracks_lt<-adehabitatLT::as.ltraj(xy = cbind(wrap360(tracks$lon),tracks$lat),
                                  date = tracks$datetime,
                                  id = tracks$UniID,
                                  burst = tracks$UniID_gap,
                                  typeII = T,slsp="remove",infolocs = tracks,
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
rm(tracks)
# resample at 10 minutes
tracks_lt_redis<-adehabitatLT::redisltraj(l = tracks_lt,u = 10*60,type = "time")
tracks10<-adehabitatLT::ld(tracks_lt_redis)
tracks10<-tracks10%>%dplyr::select(x,y,date,id)
tracks10<-rename(tracks10,"lat_int"="y")
tracks10<-rename(tracks10,"lon_int"="x")
tracks10<-rename(tracks10,"datetime"="date")
head(tracks10)
tracks10$Int_int<-"10min"


# resample BFAL & LAAL to 2hr
tracks_lt_2hr<-adehabitatLT::redisltraj(l = tracks_lt,u = 60*60*2,type = "time")
t<-adehabitatLT::ld(tracks_lt_2hr)%>%dplyr::select(date,id)
t$PT<-"Orig"
tracks_lt_redis_2hr<-adehabitatLT::redisltraj(l = tracks_lt_2hr,u = 10*60,type = "time")

tracks2<-adehabitatLT::ld(tracks_lt_redis_2hr)
tracks2<-tracks2%>%dplyr::select(x,y,date,id)
tracks2<-rename(tracks2,"lat_int"="y")
tracks2<-rename(tracks2,"lon_int"="x")
tracks2<-rename(tracks2,"datetime"="date")
tracks2$Int_int<-"2hour"
head(tracks2)

tracks2<-left_join(tracks2,t,by=c("id"="id","datetime"="date"))
rm(tracks_lt_2hr, tracks_lt_redis_2hr, t)
tracks2$date<-date(tracks2$datetime)
orig<-tracks2%>%filter(PT=="Orig")

tracks2$diff<- rep(NA, nrow(tracks2))
for (i in 1:nrow(tracks2)){
  if(is.na(tracks2$PT[i])!=TRUE) next
  b<-orig%>%filter(id==tracks2$id[i])%>%
    filter(date>(tracks2$date[i]-2))%>%
    filter(date<(tracks2$date[i]+2)) #avoids missing times at 
  D<-rep(NA, nrow(b))
  for (k in 1:nrow(b)){
    d<-abs(as.numeric(difftime(tracks2$datetime[i], b$datetime[k],units = "mins")))
    D[k]<-d
  }
  tracks2$diff[i]<-min(D)
}

# resample BFAL & LAAL to 4hr
tracks_lt_4hr<-adehabitatLT::redisltraj(l = tracks_lt,u = 60*60*4,type = "time")
t<-adehabitatLT::ld(tracks_lt_4hr)%>%dplyr::select(date,id)
t$PT<-"Orig"
tracks_lt_redis_4hr<-adehabitatLT::redisltraj(l = tracks_lt_4hr,u = 10*60,type = "time")

tracks4<-adehabitatLT::ld(tracks_lt_redis_4hr)
tracks4<-tracks4%>%dplyr::select(x,y,date,id)
tracks4<-rename(tracks4,"lat_int"="y")
tracks4<-rename(tracks4,"lon_int"="x")
tracks4<-rename(tracks4,"datetime"="date")
tracks4$Int_int<-"4hour"
rm(tracks_lt_4hr, tracks_lt_redis_4hr)
head(tracks4)

tracks4<-left_join(tracks4,t,by=c("id"="id","datetime"="date"))
tracks4$date<-date(tracks4$datetime)
orig<-tracks4%>%filter(PT=="Orig")

tracks4$diff<- rep(NA, nrow(tracks4))
for (i in 1:nrow(tracks4)){
  if(is.na(tracks4$PT[i])!=TRUE) next
  b<-orig%>%filter(id==tracks4$id[i])%>%
    filter(date>(tracks4$date[i]-2))%>%
    filter(date<(tracks4$date[i]+2)) #avoids missing times at 
  D<-rep(NA, nrow(b))
  for (k in 1:nrow(b)){
    d<-abs(as.numeric(difftime(tracks4$datetime[i], b$datetime[k],units = "mins")))
    D[k]<-d
  }
  tracks4$diff[i]<-min(D)
}


# resample BFAL & LAAL to 12hr
tracks_lt_12hr<-adehabitatLT::redisltraj(l = tracks_lt,u = 60*60*12,type = "time")
t<-adehabitatLT::ld(tracks_lt_12hr)%>%dplyr::select(date,id)
t$PT<-"Orig"
tracks_lt_redis_12hr<-adehabitatLT::redisltraj(l = tracks_lt_12hr,u = 10*60,type = "time")

tracks12<-adehabitatLT::ld(tracks_lt_redis_12hr)
tracks12<-tracks12%>%dplyr::select(x,y,date,id)
tracks12<-rename(tracks12,"lat_int"="y")
tracks12<-rename(tracks12,"lon_int"="x")
tracks12<-rename(tracks12,"datetime"="date")
tracks12$Int_int<-"12hour"
head(tracks12)

tracks12<-left_join(tracks12,t,by=c("id"="id","datetime"="date"))
tracks12$date<-date(tracks12$datetime)
orig<-tracks12%>%filter(PT=="Orig")

tracks12$diff<- rep(NA, nrow(tracks12))
for (i in 1:nrow(tracks12)){
  if(is.na(tracks12$PT[i])!=TRUE) next
  b<-orig%>%filter(id==tracks12$id[i])%>%
    filter(date>(tracks12$date[i]-2))%>%
    filter(date<(tracks12$date[i]+2)) #avoids missing times at 
  D<-rep(NA, nrow(b))
  for (k in 1:nrow(b)){
    d<-abs(as.numeric(difftime(tracks12$datetime[i], b$datetime[k],units = "mins")))
    D[k]<-d
  }
  tracks12$diff[i]<-min(D)
}
rm(tracks_lt,tracks_lt_12hr, tracks_lt_redis,tracks_lt_redis_12hr)

# join 2,4,12 together ----------------------------------------------------
TRACKS<-rbind(tracks2,tracks4,tracks12)
rm(tracks2,tracks4,tracks12,orig,t,b)
head(TRACKS)

ids=unique(TRACKS$id)
ints<-unique(TRACKS$Int_int)
#TRACKS$datetime_ns<-align.time(TRACKS$datetime, n=60)

tracks10<-rename(tracks10,"y"="lat_int")
tracks10<-rename(tracks10,"x"="lon_int")

dat_paired<-NULL
for (k in 1:length(ints)){
  tracks_int<-TRACKS%>%filter(Int_int==ints[k])

for (i in 1:length(ids)){
  birdy<-tracks10%>%filter(id==ids[i])%>%
    dplyr::select(id,datetime,x,y)
  #birdy$datetime_n10<-align.time(birdy$datetime, n=10*60)
  
  b1<-tracks_int%>%filter(id==ids[i])
  #b1$datetime_n10<-align.time(b1$datetime_ns, n=10*60)
  
  a<-left_join(b1,birdy,by=c("datetime"="datetime", "id"="id"))
  a<-a%>%filter(is.na(y)==FALSE)
  a<-a%>%filter(is.na(x)==FALSE)
  
  if(nrow(a)==0) next
  a$Dist<- rep(NA, nrow(a))
  for (k in 1:nrow(a)){
    di<-distance(lat1=a$lat_int[k],lat2=a$y[k],lon1=a$lon_int[k],lon2=a$x[k])
    a$Dist[k]<-di
  }
  dat_paired<-rbind(dat_paired,a)
}
}
saveRDS(dat_paired,paste0(userdir,"/Analysis/compileddata/dat_paired_2412.rda"))  
dat_paired<-readRDS(paste0(userdir,"/Analysis/compileddata/dat_paired_2412.rda"))  


tracks4_dp<-dat_paired%>%filter(Int_int=="4hour")
quartz()
ggplot()+
  geom_boxplot(data=dat_paired, aes(x=Int_int,y=Dist))
ggplot()+
  geom_point(data=dat_paired, aes(x=diff,y=Dist))
ggplot()+
  geom_point(data=dat_paired%>%
               filter(Int_int=="12hour"), aes(x=diff,y=Dist))
ggplot()+
  geom_boxplot(data=dat_paired%>%
               filter(Int_int=="12hour"), aes(x=as.factor(diff),y=Dist))

data=dat_paired%>%filter(Int_int=="12hour")

dat_paired%>%group_by(Int_int)%>%
  summarise(u=mean(Dist, na.rm=TRUE),
            sd=sd(Dist, na.rm=TRUE),
            med=median(Dist, na.rm=TRUE))

dat_paired$diff_r<-round(dat_paired$diff,digits = (-1))
a<-dat_paired%>%group_by(Int_int,diff_r)%>%
  summarise(u=mean(Dist, na.rm=TRUE),
            sd=sd(Dist, na.rm=TRUE),
            med=median(Dist, na.rm=TRUE))%>%filter(diff_r<361)
write.csv(a,paste0(userdir,"/Analysis/compileddata/InterpolationCheck_intervalerror.csv"))  

hr12<-dat_paired%>%group_by(diff_r)%>%filter(Int_int=="12hour")%>%
  summarise(u=mean(Dist, na.rm=TRUE),
            sd=sd(Dist, na.rm=TRUE),
            med=median(Dist, na.rm=TRUE))%>%filter(diff_r<180)

hr2<-dat_paired%>%group_by(diff_r)%>%filter(Int_int=="2hour")%>%
  summarise(u=mean(Dist, na.rm=TRUE),
            sd=sd(Dist, na.rm=TRUE),
            med=median(Dist, na.rm=TRUE))%>%filter(diff_r<180)

hr4<-dat_paired%>%group_by(diff_r)%>%filter(Int_int=="4hour")%>%
  summarise(u=mean(Dist, na.rm=TRUE),
            sd=sd(Dist, na.rm=TRUE),
            med=median(Dist, na.rm=TRUE))%>%filter(diff_r<180)
library(flextable)
(hr2$u2<-paste0(round(hr2$u,digits=2)," ± ",round(hr2$sd,digits = 2)))
(hr12$u12<-paste0(round(hr12$u,digits=2)," ± ",round(hr12$sd,digits = 2)))
(hr4$u4<-paste0(round(hr4$u,digits=2)," ± ",round(hr4$sd,digits = 2)))
hr24<-full_join(hr2%>%dplyr::select(diff_r,u2),
          hr4%>%dplyr::select(diff_r,u4),by=c("diff_r"="diff_r"))
(h<-full_join(hr24,hr12%>%dplyr::select(diff_r,u12),by=c("diff_r"="diff_r")))

h_format<-h%>%flextable%>%
  merge_v(j=1)
save_as_docx("my table"=h_format, path = paste0(userdir,"/Analysis/compileddata/GPSlocerror_SupplimentalTable1.docx"))

dat_paired$Int_int<-ordered(dat_paired$Int_int,levels=c( "2hour","4hour","12hour"))

quartz(width=5,height=4)
ggplot()+
  geom_boxplot(data=dat_paired%>%
                 filter(diff_r<180), 
               aes(x=as.factor(diff_r),y=Dist, fill=Int_int),
               position="dodge",outlier.size = 0.1,size=.1)+
  theme_classic()+
  xlab("Time Difference (min)")+
  ylab("Distance (km)")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.title = element_blank())
quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_Oct2020/SupplimentalFig1.png"),dpi=300)


saveRDS(dat_paired,paste0(userdir,"/Analysis/compileddata/dat_paired_2412.rda"))  
dat_paired<-readRDS(paste0(userdir,"/Analysis/compileddata/dat_paired_2412.rda"))  

# Check how many encounters/interactions occured with points >4hr  --------
matlab2POS = function(x,tz = "UTC") {
  days = x - 719529 #719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 #86400 seconds in a day
  return(as.POSIXct(secs,origin = "1970-1-1",tz = tz))#returns POSIXct object
}
userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'
birds<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_lowresDen_60km2019-09-27.rda"))  
stal<-birds%>%filter(species=="STAL")
birds%>%group_by(species)%>%summarise(n=n_distinct(birdID))
z<-birds%>%filter(species!="STAL")%>%group_by(species,birdID)%>%summarise(n=n())

STALraw<-read.csv("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Albs_STAL_OConnor_pubs/Post_fledging_tracking/data/STALfledglings_all_MTIandSPDfiltered_excluding_deadbirds_and_postfledging_drift_lessthan20kmhr_forR.csv")
STALraw$datetime<-matlab2POS(STALraw$mday)

ids<-unique(STALraw$name)
STALrawN<-NULL
for (i in 1:length(ids)){
  birdy<-STALraw%>%filter(name==ids[i])
  birdy$difftime =difftime(birdy$datetime, lag(birdy$datetime, 1),units = "mins")
  STALrawN<-rbind(STALrawN,birdy)
}
STALraw<-STALrawN

nrow(birds%>%filter(nrow_boat.1<=5))
birds%>%filter(nrow_boat.1<=5)%>%
  group_by(species)%>%
  summarise(n=n())


STALevents<-NULL
for (i in 1:nrow(stal)){
  event<-stal[i,]
  #number of raw GPS data used
  event$EncounterDataPts<-nrow(STALraw%>%filter(name==event$birdID)%>%
    filter(datetime>event$tmin_30)%>%
    filter(datetime<event$tmax_30))
  
  locs<-STALraw%>%filter(name==event$birdID)%>%
                              filter(datetime>(event$tmin_30-(60*60*64)))%>%
                              filter(datetime<(event$tmax_30+(60*60*64)))
  #interactions
  if (event$time3_min!=0){
    dat=NULL
    for (k in 1:nrow(locs)){
      tdiff<-difftime(event$tmin_3, locs$datetime[k],units = "mins")
      dat<-rbind(dat,tdiff)
    }
    event$InteractionTimeDiff<-as.numeric(min(abs(dat)))
    if(which.min(abs(dat))==1){event$TimeDiff_proceedingInter<-NA}
    if(which.min(abs(dat))==1)next
    event$TimeDiff_proceedingInter<-locs$difftime[which.min(abs(dat))-1]
  }
  if (event$time3_min==0){
    event$InteractionTimeDiff<-NA
    event$TimeDiff_proceedingInter<-NA
  }
  #encounters
    dat=NULL
    for (k in 1:nrow(locs)){
      tdiff<-difftime(event$tmin_30, locs$datetime[k],units = "mins")
      dat<-rbind(dat,tdiff)
    }
    event$EncounterTimeDiff<-as.numeric(min(abs(dat)))
    if(which.min(abs(dat))==1){event$TimeDiff_proceedingEncount<-NA}
    if(which.min(abs(dat))==1)next
    event$TimeDiff_proceedingEncount<-locs$difftime[which.min(abs(dat))-1]
  STALevents<-bind_rows(STALevents,event)
}
saveRDS(STALevents,paste0(userdir,"/Analysis/compileddata/STALevents.rda"))  
STALevents<-readRDS(paste0(userdir,"/Analysis/compileddata/STALevents.rda"))  

STALevents%>%group_by(birdID)%>%
  filter(interactionYN=="Y")%>%
  summarise(n=n())


library(cowplot)
h<-3
STALevents$InteractionTimeDiff_aboveh<-0
STALevents$InteractionTimeDiff_aboveh[STALevents$interactionYN=="N"]<-99
STALevents$InteractionTimeDiff_aboveh[STALevents$InteractionTimeDiff>h]<-1
STALevents$InteractionTimeDiff_aboveh<-as.factor(STALevents$InteractionTimeDiff_aboveh)
STALevents$EncounterTimeDiff_aboveh<-0
STALevents$EncounterTimeDiff_aboveh[STALevents$EncounterTimeDiff>h]<-1
STALevents$EncounterTimeDiff_aboveh<-as.factor(STALevents$EncounterTimeDiff_aboveh)
p1<-ggplot()+
  geom_histogram(data=STALevents,aes(x=InteractionTimeDiff, 
                                     fill=EncounterTimeDiff_aboveh))+
  geom_text(data=data.frame(cc=nrow(STALevents%>%
                                      filter(InteractionTimeDiff>h*60))),
            aes(y=150, x=600, label=cc), size=3)+
  geom_vline(xintercept = 120, color="green")+
  theme(legend.position = "bottom",
        legend.text = element_text(size=5),
        legend.title = element_blank())
p2<-ggplot()+
  geom_histogram(data=STALevents,aes(x=EncounterTimeDiff, 
                                     fill=InteractionTimeDiff_aboveh))+
  geom_text(data=data.frame(cc=nrow(STALevents%>%
                                      filter(EncounterTimeDiff>h*60))),
            aes(y=300, x=600, label=cc), size=3)+
  geom_vline(xintercept = 120, color="green")+
  theme(legend.position = "bottom",
        legend.text = element_text(size=5),
        legend.title = element_blank())
plot_grid(p1,p2)
ggsave("/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch/Analysis/PLOTS/TimeDiffs.png")
library(viridis)

ggplot()+
  geom_point(data=STALevents,
             aes(y=InteractionTimeDiff, x=EncounterTimeDiff))+
  geom_hline(yintercept = 120, color="black",linetype="dashed")+
  geom_vline(xintercept = 120, color="black",linetype="dashed")+
  xlab("Encounter Time Difference (min)")+
  ylab("Interaction Time Difference (min)")+
  theme(legend.position = "none")+
  xlim(0,600)+
  theme_classic()
quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_Oct2020/SupplimentalFig2.png"),dpi=300)

nrow(STALevents%>%filter(nrow_boat.1<=5))
nrow(STALevents%>%filter(EncounterTimeDiff>3*60))
nrow(STALevents%>%filter(InteractionTimeDiff>(3*60)))

STAL_Elist<-vector(mode = "list", length = 7)
for (i in 1:7){
  h=i+1
  sL<-vector(mode = "list", length = 2)
  sL[[1]]<-h
  a<-STALevents%>%filter(nrow_boat.1>=5)%>%
       filter(EncounterTimeDiff<h*60)%>%
       filter(is.na(InteractionTimeDiff)==TRUE | InteractionTimeDiff<(h*60))%>%
       filter(EncounterDataPts>0)
  sL[[2]]<-unique(a$eventID)
  STAL_Elist[[i]]<-sL
}

saveRDS(STAL_Elist,paste0(userdir,"/Analysis/compileddata/STAL_eventlist_disttimegaps.rda"))  
STAL_Elist<-readRDS(paste0(userdir,"/Analysis/compileddata/STAL_eventlist_disttimegaps.rda"))  

h=2
a<-STALevents%>%filter(nrow_boat.1>=5)%>%
  filter(EncounterTimeDiff<h*60)%>%
  filter(is.na(InteractionTimeDiff)==TRUE | InteractionTimeDiff<(h*60))%>%
  filter(EncounterDataPts>0)
mean(a$EncounterTimeDiff); sd(a$EncounterTimeDiff)
a%>%filter(interactionYN=="Y")%>%
  summarise(u=mean(InteractionTimeDiff),
            sd=sd(InteractionTimeDiff))
b<-a%>%filter(interactionYN=="Y")
nrow(b)/nrow(a)

birds<-birds%>%filter(species!="STAL")
events<-bind_rows(birds,a)
saveRDS(events,paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_lowresDen_60km2020-08-24_interpinfo.rda"))  
events<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_lowresDen_60km2020-08-24_interpinfo.rda"))  


length(unique(events$mmsi))
nrow(events)

info<-a%>%group_by(birdID)%>%summarise(n=n())
mean(info$n)
sd(info$n)
info<-a%>%group_by(birdID)%>%
  filter(interactionYN=="Y")%>%summarise(n=n())
mean(info$n)
sd(info$n)
a%>%filter(interactionYN=="Y")%>%
  summarize(uInter=mean(time3_min)/60,
            sdInter=sd(time3_min)/60)
length(unique(a$mmsi))

nrow(a%>%filter(geartypeCLASS=="Trawlers"))/nrow(a%>%filter(geartypeCLASS!="Unknown"))
nrow(a%>%filter(geartypeCLASS=="Longliners"))/nrow(a%>%filter(geartypeCLASS!="Unknown"))
nrow(a%>%filter(geartypeCLASS=="Unknown"))
ggplot()+
  geom_bar(data=a,aes(x=geartypeCLASS))
nrow(a%>%filter(is.na(Territory1)==FALSE))/nrow(a)


# STAL Events -------------------------------------------------------------
STAL_enc<-readRDS(paste0(userdir,"/Analysis/compileddata/STAL_enc_2hr_2020-08-24.rds"))
STAL_enc_int<-readRDS(paste0(userdir,"/Analysis/compileddata/STAL_enc_int_2hr_2020-08-24.rds"))
STALevents<-readRDS(paste0(userdir,"/Analysis/compileddata/STALevents.rda"))  
STALmeta<-read.csv("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Albs_STAL_OConnor_pubs/Post_fledging_tracking/data/stal_capturedata_meta.csv")
head(STALmeta)
STALmeta<-STALmeta%>%dplyr::select(ID,Attachment_method,GPS_schedule)
STALmeta$Tag_ID<-as.character(STALmeta$ID)
unique(STALmeta$GPS_schedule)

STALmeta$GPSint<-"4-hr"
STALmeta$GPSint[STALmeta$GPS_schedule=="Start 0700, stop 1700, hour step 2. 6 locations/day."]<-"2-hr"

STAL_enc<-left_join(STAL_enc,STALevents%>%
                      dplyr::select(eventID,InteractionTimeDiff,EncounterTimeDiff),
                    by=c("eventID"="eventID"))
str(STAL_enc$birdID)
STAL_enc$birdID<-as.character(STAL_enc$birdID)
STAL_enc<-left_join(STAL_enc,STALmeta%>%
                      dplyr::select(Tag_ID,Attachment_method,GPSint),
                    by=c("birdID"="Tag_ID"))

STAL_enc$eventDiff<-STAL_enc$EncounterTimeDiff
idx<-which(is.na(STAL_enc$InteractionTimeDiff)==FALSE)
STAL_enc$eventDiff[is.na(STAL_enc$InteractionTimeDiff)==FALSE]<-STAL_enc$InteractionTimeDiff[idx]

library(cowplot)
names(STALevents)
str(STAL_enc)
STAL_enc$Attachment_method
A<-ggplot()+
  geom_histogram(data=STAL_enc,aes(x=EncounterTimeDiff,fill=GPSint),bins=12)+
  xlab(" ")+
  ylab("Count")+
  theme_classic()+
  theme(legend.title = element_blank(),
        legend.position = "none")
B<-ggplot()+
  geom_histogram(data=STAL_enc,aes(x=eventDiff,fill=GPSint),bins=12)+
  xlab("GPS Time Difference (min)")+
  ylab(" ")+
  theme_classic()+
  theme(legend.title = element_blank(),
        legend.position = "none")
C<-ggplot()+
  geom_histogram(data=STAL_enc,aes(x=InteractionTimeDiff,fill=GPSint),bins=12)+
  xlab(" ")+
  ylab(" ")+
  theme_classic()+
  theme(legend.title = element_blank())

plot<-plot_grid(A,B,C,nrow = 1,rel_widths = c(1,1,1.3),
                labels = c('A', 'B','C'))

quartz(width=8,height=2.5)
cowplot::ggdraw(plot) + 
  theme(plot.background = element_rect(fill="white", color = NA))
quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_Oct2020/SupplimentalFig3.png"),dpi=300)

nrow(STAL_enc%>%filter(GPSint=="2-hr")%>%
  filter(InteractionTimeDiff>60))#%>%
  #group_by(birdID)%>%
  #summarise(n=n())
9/496

nrow(STAL_enc%>%
       filter(InteractionTimeDiff>60))#%>%
159/496
#group_by(birdID)%>%
#summarise(n=n())


nrow(STAL_enc%>%filter(GPSint=="2-hr")%>%
  filter(EncounterTimeDiff>60))#%>%
  #group_by(birdID)%>%
  #summarise(n=n())
nrow(STAL_enc%>%
       filter(EncounterTimeDiff>60))#%>%
729/1735
31/1735
