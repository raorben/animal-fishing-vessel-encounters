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

# Rediscretionize LAAL & BFAL tracks to 10 minutes -------------------------------------
tracks<-tracksA%>%filter(species!="STAL")

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

# resample at 10 minutes
tracks_lt_redis<-adehabitatLT::redisltraj(l = tracks_lt,u = 10*60,type = "time")
# resample BFAL & LAAL to 60 min
tracks_lt_redis_1hr<-adehabitatLT::redisltraj(l = tracks_lt,u = 60*60,type = "time")

# convert back to data.frame
tracks5<-adehabitatLT::ld(tracks_lt_redis) %>% mutate(UniID_gap=as.character(burst),UniID=(as.character(id)))
head(tracks5)
str(tracks5)
colnames(tracksA)
str(tracks5$UniID)
str(tracksA$UniID)
trackAinfo<-tracksA %>% dplyr::select(ID,TripNum,UniID,flightyr,colony,species,TagType)
colnames(trackAinfo)
str(trackAinfo)
trackAinfo<-trackAinfo %>% distinct(ID,TripNum,UniID,flightyr,colony,species,TagType)
tracks5<-left_join(tracks5,trackAinfo,by=c("UniID"="UniID"))

#adds hourly STAL data back in 
tracks5<-tracks5%>%mutate(lon360=x,lat=y,datetime=date)
tracksA_stal<-tracksA%>%filter(species=="STAL")%>%mutate(lon360=wrap360(lon))
colnames(tracksA_stal)
colnames(tracks5)

alltracks<-bind_rows(tracks5,tracksA_stal)
head(alltracks)

quartz()
ggplot()+
  geom_point(data=alltracks,
             aes(x=lon360,y=lat,color=as.factor(ID)),size=0.1)+
  theme(legend.position = "none")

saveRDS(alltracks,paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr.rds"))
alltracks<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr.rds"))


# Save hourly LAAL & BFAL data --------------------------------------------
# convert back to data.frame
tracks60<-adehabitatLT::ld(tracks_lt_redis_1hr) %>% 
  mutate(UniID_gap=as.character(burst),UniID=(as.character(id)))
trackAinfo<-tracksA %>% dplyr::select(ID,TripNum,UniID,flightyr,colony,species,TagType)

trackAinfo<-trackAinfo %>% distinct(ID,TripNum,UniID,flightyr,colony,species,TagType)
tracks60<-left_join(tracks60,trackAinfo,by=c("UniID"="UniID"))

tracks60<-tracks60%>%mutate(lon360=x,lat=y,datetime=date)
quartz()
ggplot()+
  geom_point(data=tracks60,
             aes(x=lon360,y=lat,color=as.factor(ID)),size=0.1)+
  theme(legend.position = "none")
saveRDS(tracks60,paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_60minbytrip.rds"))
tracks60<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_60minbytrip.rds"))

