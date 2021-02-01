library(dplyr)
library(ggplot2)
library(lubridate)
library(rworldmap)
library(marmap)
library(maptools)

wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

unwrap360<-function(lon360){
  lon<-ifelse(lon360 > 180, -360 + lon360, lon360)
  return(lon)}

if(Sys.info()[7]=="rachaelorben") {
  GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/"
  userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'}

#prox.sum<-readRDS(paste0(userdir,"/Analysis/compileddata/proxsummary.rda"))
prox.sumsel.uniID<-readRDS(file=paste0(userdir,"/Analysis/compileddata/proxsummary_uniIDforRST.rda"))
prox.sumsel.vessels<-readRDS(paste0(userdir,"/Analysis/compileddata/EI_vesseldata.rda"))

alltracks<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_EEZ_lh_FAO.rds"))
#Elocs<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_2019-02-25.rda"))  


head(alltracks)
head(prox.sumsel.uniID)
STAL<-alltracks%>%filter(species=="STAL")
#combine prox.sum with uniID for RST matching
prox.sumsel.vessels.uniID<-dplyr::left_join(prox.sumsel.vessels, prox.sumsel.uniID%>%
                                       dplyr::select(eventID,uniID))


# expand boat-bird days ---------------------------------------------------
head(prox.sumsel.vessels.uniID)

birdboat<-prox.sumsel.vessels.uniID%>%dplyr::select( eventID,mmsi,birdID,date_min,date_max)
head(birdboat)
 
BB<-NULL
for (i in 1:nrow(birdboat)){
  info<-birdboat[i,]
  start_date <- ymd(info$date_min)
  end_date <- ymd(info$date_max)
  n_days <- interval(start_date,end_date)/days(1)
  birdboatX<-data.frame(daysover=(start_date + days(0:n_days)))
  birdboatX$eventID<-info$eventID
  birdboatX$mmsi<-info$mmsi
  birdboatX$birdID<-info$birdID
  BB<-rbind(BB,birdboatX)
}

dailysum<-BB%>%group_by(birdID,daysover)%>%summarise(n=n())

birdboat$boatdays<-NA
for (i in 1:nrow(birdboat)){
  info<-birdboat[i,]
  bdat<-dailysum%>%filter(birdID==info$birdID)
    
  start_date <- ymd(info$date_min)
  end_date <- ymd(info$date_max)
  n_days <- interval(start_date,end_date)/days(1)
  dayso<-(start_date + days(0:n_days))
  
  bdatsel<-bdat%>%filter(daysover %in% dayso)
  birdboat$boatdays[i]<-sum(bdatsel$n)
}

prox.sumsel.vessels.uniID<-left_join(prox.sumsel.vessels.uniID,birdboat%>%
                                       dplyr::select(eventID,boatdays),by="eventID")


# Summarize and add RST results -------------------------------------------
MoveResid_DynScale<-readRDS(file = paste0(userdir,"/Analysis/compileddata/RSToutput/AllTracksInt_DynScale.rda"))
#MoveResid_DynScale<-list(all_tracksD, dynscale, threshold, time_units, bandIDs,missing)
tracksRST<-MoveResid_DynScale[[1]]
dynscale<-MoveResid_DynScale[[2]]

prox.sumsel.vessels.uniID$RST_Per24rest<-NA
prox.sumsel.vessels.uniID$RST_Per24transit<-NA
prox.sumsel.vessels.uniID$RST_Per24actfor<-NA
prox.sumsel.vessels.uniID$RST_domstate_2hr<-NA
prox.sumsel.vessels.uniID$RST_domstate_24hr<-NA
prox.sumsel.vessels.uniID$RSTnrows<-NA


tracksRST$RSTstate<-NA
tracksRST$RSTstate[tracksRST$res>0]<-"F"
tracksRST$RSTstate[tracksRST$res==0]<-"T"
tracksRST$RSTstate[tracksRST$res<0]<-"R"

for (i in 1: nrow(prox.sumsel.vessels.uniID)){
  info<-prox.sumsel.vessels.uniID[i,]
  
  RST<-tracksRST%>%dplyr::filter(band==info$uniID)%>%
    filter(datetime>=(info$tmin_30-86400))%>% # one day ahead
    filter(datetime<=info$tmax_30) #end of the encounter
  
  if (nrow(RST)==0){next}

  
  #if there is an interaction
  if (info$time3_min>0){
    RSTday<-RST%>%filter(datetime<=info$tmin_3)%>%
    filter(datetime>=(info$tmin_3-86400))#trims to the 24hrs before interaction (3km)
    dur<-nrow(RSTday)
    if (dur==0){prox.sumsel.vessels.uniID$RSTnrows[i]<-dur}
    if (dur==0){next}
    
    prox.sumsel.vessels.uniID$RST_Per24rest[i]<-nrow(RSTday%>%dplyr::filter(RSTstate=="R"))/dur
    prox.sumsel.vessels.uniID$RST_Per24transit[i]<-nrow(RSTday%>%dplyr::filter(RSTstate=="T"))/dur
    prox.sumsel.vessels.uniID$RST_Per24actfor[i]<-nrow(RSTday%>%dplyr::filter(RSTstate=="F"))/dur
    prox.sumsel.vessels.uniID$RSTnrows[i]<-dur
    
    if (dur>1){
    prox.sumsel.vessels.uniID$RST_domstate_24hr[i]<-names(which.max(table(RSTday$RSTstate)))
    }
    if (dur==1){
      prox.sumsel.vessels.uniID$RST_domstate_24hr[i]<-RSTday$RSTstate
    }
    
    if (info$species!="STAL"){
    RST2hr<-RSTday%>%filter(datetime>=info$tmin_3-(60*2*60))
    prox.sumsel.vessels.uniID$RST_domstate_2hr[i]<-names(which.max(table(RST2hr$RSTstate)))
    }
  }
  if (info$time3_min==0){
    
    RSTday<-RST%>%filter(datetime<=info$tmin_30) #one day ahead of encounter
    dur<-nrow(RSTday)
    
    prox.sumsel.vessels.uniID$RST_Per24rest[i]<-nrow(RSTday%>%dplyr::filter(RSTstate=="R"))/dur
    prox.sumsel.vessels.uniID$RST_Per24transit[i]<-nrow(RSTday%>%dplyr::filter(RSTstate=="T"))/dur
    prox.sumsel.vessels.uniID$RST_Per24actfor[i]<-nrow(RSTday%>%dplyr::filter(RSTstate=="F"))/dur
    prox.sumsel.vessels.uniID$RSTnrows[i]<-dur
    
    #dominate state 24hr ahead 
    if(is.na(unique(RSTday$RSTstate))==TRUE & length(unique(RSTday$RSTstate))==1){next}
    
    if (dur>1){
    prox.sumsel.vessels.uniID$RST_domstate_24hr[i]<-names(which.max(table(RSTday$RSTstate)))
    }
    if (dur==1){
      prox.sumsel.vessels.uniID$RST_domstate_24hr[i]<-RSTday$RSTstate
    }
    
    
    #dominate state 2hr ahead   
    if (info$species!="STAL"){
    RST2hr<-RSTday%>%filter(datetime>=info$tmin_30-7200)
    prox.sumsel.vessels.uniID$RST_domstate_2hr[i]<-names(which.max(table(RST2hr$RSTstate)))
    }
    } 
}

#prox.sumsel.vessels.uniID%>%filter(species=="STAL")
#combine prox.sumsel.vessels.uniID with EEZ, Longhurst & FAO of first encounter location
#also pulls out time of first encounter or first interaction and calculates solar zenith

prox.sumsel.vessels.uniID$flight.oid<-NA
prox.sumsel.vessels.uniID$deploy.count<-NA
prox.sumsel.vessels.uniID$Territory1<-NA
prox.sumsel.vessels.uniID$Longhurst<-NA
prox.sumsel.vessels.uniID$LonghurstCode<-NA
prox.sumsel.vessels.uniID$FID<-NA
prox.sumsel.vessels.uniID$F_CODE<-NA
prox.sumsel.vessels.uniID$colony<-NA
prox.sumsel.vessels.uniID$sunalt<-NA
prox.sumsel.vessels.uniID$DayNight<-NA
prox.sumsel.vessels.uniID$DayNightDusk<-NA

for (i in 1: nrow(prox.sumsel.vessels.uniID)){
  info<-prox.sumsel.vessels.uniID[i,]
  a<-alltracks%>%filter(ID.x==info$birdID)%>%
    filter(datetime>=info$tmin_30)%>%
    filter(datetime<=info$tmax_30)
  prox.sumsel.vessels.uniID$nrowBird_within30[i]<-nrow(a)
  
  if(nrow(a)==0){next}
  
  if (info$species=="STAL"){
    prox.sumsel.vessels.uniID$flight.oid[i]<-a$flight.oid[1]
    prox.sumsel.vessels.uniID$deploy.count[i]<-a$deploy.count[1]
    prox.sumsel.vessels.uniID$Territory1[i]<-as.character(a$Territory1[1])
      if(is.na(a$Territory1[1])==TRUE){prox.sumsel.vessels.uniID$Territory1[i]<-as.character(unique(a$Territory1)[2])}
    
    prox.sumsel.vessels.uniID$Longhurst[i]<-as.character(a$ProvDescr[1])
      if(is.na(a$ProvDescr[1])==TRUE){prox.sumsel.vessels.uniID$Longhurst[i]<-as.character(unique(a$ProvDescr)[2])}
    
    prox.sumsel.vessels.uniID$LonghurstCode[i]<-as.character(a$ProvCode[1])
      if(is.na(a$ProvCode[1])==TRUE){prox.sumsel.vessels.uniID$LonghurstCode[i]<-as.character(unique(a$ProvCode)[2])}
    
    prox.sumsel.vessels.uniID$FID[i]<-as.character(a$FID[1])
      if(is.na(a$FID[1])==TRUE){prox.sumsel.vessels.uniID$FID[i]<-as.character(unique(a$FID)[2])}
    
    prox.sumsel.vessels.uniID$F_CODE[i]<-as.character(a$F_CODE[1])
      if(is.na(a$F_CODE[1])==TRUE){prox.sumsel.vessels.uniID$F_CODE[i]<-as.character(unique(a$F_CODE)[2])}
    
    prox.sumsel.vessels.uniID$colony[i]<-as.character(a$colony[1])
      if(is.na(a$colony[1])==TRUE){prox.sumsel.vessels.uniID$colony[i]<-as.character(unique(a$colony)[2])}
  }
  
  if (info$species=="LAAL" | info$species=="BFAL"){
    prox.sumsel.vessels.uniID$Territory1[i]<-as.character(a$Territory1[1])
    prox.sumsel.vessels.uniID$Longhurst[i]<-as.character(a$ProvDescr[1])
    prox.sumsel.vessels.uniID$LonghurstCode[i]<-as.character(a$ProvCode[1])
    prox.sumsel.vessels.uniID$FID[i]<-as.character(a$FID[1])
    prox.sumsel.vessels.uniID$F_CODE[i]<-as.character(a$F_CODE[1])
    prox.sumsel.vessels.uniID$colony[i]<-as.character(a$colony[1])
  }
  
# Solar Altitude Calculations -----------------------------------------------
  
  a$lon<-unwrap360(a$lon360)
  
  locs <- SpatialPoints(a[,c("lon","lat")], proj4string=CRS("+proj=longlat +datum=WGS84"))
  #head(locs)
  #str(a$datetime)
  DT_seq <- as.POSIXct(a$datetime, tz="UTC")
  SZ<-solarpos(locs, DT_seq, POSIXct.out=TRUE)
  #solarpos returns a matrix with the solar azimuth (in degrees from North), and elevation.
  a$sunalt<-SZ[,2]
  
  a$DayNight<-NA
  #nautical twilight is at 102 
  a$DayNight[a$sunalt<(-12)]<-"N"
  a$DayNight[a$sunalt>(-12)]<-"D"
  a$DayNight[a$sunalt==(-12)]<-"D"
  
  a$DayNightDusk<-NA
  #nautical twilight is at 102 
  a$DayNightDusk[a$sunalt<(-12)]<-"N"
  a$DayNightDusk[a$sunalt>(-12) & a$sunalt<0]<-"K"
  a$DayNightDusk[a$sunalt==0]<-"D"
  a$DayNightDusk[a$sunalt>0]<-"D"
  
  if (info$time3_min==0){prox.sumsel.vessels.uniID$sunalt[i]<-a$sunalt[1];
  prox.sumsel.vessels.uniID$DayNight[i]<-a$DayNight[1]
  prox.sumsel.vessels.uniID$DayNightDusk[i]<-a$DayNightDusk[1]}
  
  if (info$time3_min>0){
      b<-a%>%filter(datetime>=info$tmin_3)
    
      prox.sumsel.vessels.uniID$sunalt[i]<-b$sunalt[1];
      prox.sumsel.vessels.uniID$DayNight[i]<-b$DayNight[1]
      prox.sumsel.vessels.uniID$DayNightDusk[i]<-b$DayNightDusk[1]}
  
}

# there were 5 of these, but they are omited from the dataset since 
# the bird was never observed within 30km
b<-prox.sumsel.vessels.uniID%>%dplyr::filter(is.na(Longhurst)==TRUE)

#month of encounter
prox.sumsel.vessels.uniID$month<-month(prox.sumsel.vessels.uniID$tmin_30)

#quick check of Day Night by group
LAAL<-prox.sumsel.vessels.uniID%>%dplyr::filter(species=="LAAL")
BFAL<-prox.sumsel.vessels.uniID%>%dplyr::filter(species=="BFAL")
hist(BFAL$month)

ggplot()+
  geom_bar(data=prox.sumsel.vessels.uniID%>%
             filter(is.na(DayNight)==FALSE),
           aes(x=DayNight,fill=species))+
  facet_wrap(~species+interactionYN,scales = "free")

ggplot()+
  geom_bar(data=prox.sumsel.vessels.uniID%>%
             filter(is.na(DayNightDusk)==FALSE),
           aes(x=DayNightDusk,fill=species))+
  facet_wrap(~species+interactionYN,scales = "free")

ggplot()+
  geom_boxplot(data=prox.sumsel.vessels.uniID%>%
             filter(is.na(DayNightDusk)==FALSE),
           aes(y=sunalt,x=species))+
  facet_wrap(~species+interactionYN,scales = "free")

ggplot()+
  geom_boxplot(data=prox.sumsel.vessels.uniID,
               aes(y=log(boatdays),x=species))


# Add Environmental Variables ---------------------------------------------
library(stringr)
locs_envi<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_winduv_bathy.rds"))
PT<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_Pertrip.rds"))
colnames(PT)

colnames(locs_envi)


locs_envi$datetime1<-as.POSIXct(locs_envi$datetime1)
locs_envi1<-cbind(locs_envi,PT%>%dplyr::select(pertrip,pkey)%>%
                    mutate(pkey1=pkey)%>%
                    dplyr::select(-pkey))

matchR<-str_which(as.character(locs_envi1$pkey),as.character(locs_envi1$pkey1))
summary(matchR)

summary(locs_envi1$pertrip)

prox.sumsel.vessels.uniID$wind_v<-NA
prox.sumsel.vessels.uniID$wind_u<-NA
prox.sumsel.vessels.uniID$depth<-NA
prox.sumsel.vessels.uniID$bathyCat<-NA

prox.sumsel.vessels.uniID$env_datetime<-NA
prox.sumsel.vessels.uniID$env_lat<-NA
prox.sumsel.vessels.uniID$env_lon<-NA
prox.sumsel.vessels.uniID$tdiff_env<-NA

prox.sumsel.vessels.uniID$perTrip<-NA
prox.sumsel.vessels.uniID$TripNum<-NA

for (i in 1: nrow(prox.sumsel.vessels.uniID)){
  info<-prox.sumsel.vessels.uniID[i,]
  le<-locs_envi1%>%filter(ID==info$birdID)
  
  #when an intercation occurs use that min time for location of environment (bird location)
  if (info$time3_min>0){
    idx<-which(abs(le$datetime-info$tmin_3) == min(abs(le$datetime - info$tmin_3)))
    le_pt<-le[idx[1],]
    tdiff<-info$tmin_3-le_pt$datetime
  }
  
  #if only an encounter occurs use that min time for location of environment (bird location)
  if (info$time3_min==0){
    idx<-which(abs(le$datetime-info$tmin_30) == min(abs(le$datetime - info$tmin_30)))
    le_pt<-le[idx[1],]
    tdiff<-info$tmin_30-le_pt$datetime
  }
  
  prox.sumsel.vessels.uniID$wind_v[i]<-le_pt$wind_v
  prox.sumsel.vessels.uniID$wind_u[i]<-le_pt$wind_u
  prox.sumsel.vessels.uniID$depth[i]<-le_pt$depth
  prox.sumsel.vessels.uniID$bathyCat[i]<-le_pt$bathyCat
  
  prox.sumsel.vessels.uniID$env_datetime[i]<-le_pt$datetime
  prox.sumsel.vessels.uniID$env_lat[i]<-le_pt$lat
  prox.sumsel.vessels.uniID$env_lon[i]<-le_pt$lon
  prox.sumsel.vessels.uniID$tdiff_env[i]<-tdiff
  prox.sumsel.vessels.uniID$perTrip[i]<-le_pt$pertrip
  prox.sumsel.vessels.uniID$TripNum[i]<-le_pt$TripNum
}

prox.sumsel.vessels.uniID$env_datetime<-as.POSIXct(prox.sumsel.vessels.uniID$env_datetime, tz="UTC",origin="1970-01-01")
head(prox.sumsel.vessels.uniID)



# add additional environmental variables (SST & chia) ---------------------
dat<-prox.sumsel.vessels.uniID

#set up data for rerdap xtracto
tpos <- as.POSIXct(dat$env_datetime)
xpos <- dat$env_lon
ypos <- dat$env_lat

#if need altitude position
zpos <- rep(0, length(xpos))


#if need to change longitude (for MODIS chla)
xpos_2 <- ifelse(xpos > 180, -360 + xpos, xpos)

#for MODIS chla
#"ERDDAP datasets bounds -179.97917 179.97917"
#so have to change max and min values to be within dataset
xpos_3 <- ifelse(xpos < -179.97917, -179.97917, xpos) 
xpos_3 <- ifelse(xpos_3 > 179.97917, 179.97917, xpos_3)


summary(xpos_3)


#Use rerddap 
#https://rmendels.github.io/UsingrerddapXtracto.nb.html


#install.packages("devtools")
#devtools::install_github("ropensci/rerddap", force = TRUE)
#devtools::install_github("rmendels/rerddapXtracto")

library("rerddapXtracto")
library("ggplot2")
library("plotdap")
library("sf")

require("rerddap")
require("rerddapXtracto")


#chla
#lon needs to be between -180 and 180 so use xpos_3
#also remove zpos info because dataset has no altitude variable.
urlbase <- 'http://upwell.pfeg.noaa.gov/erddap'
chlInfo <- rerddap::info('erdMH1chla8day') 
chl8day <- rxtracto(chlInfo, parameter = 'chlorophyll', 
                    xcoord = xpos_3, ycoord = ypos, 
                    tcoord = tpos, xlen = 0, ylen = 0,verbose = TRUE)

#
dat$chla <- chl8day$`mean chlorophyll`

#SST
urlbase <- 'http://upwell.pfeg.noaa.gov/erddap'
SSTInfo <- rerddap::info('erdMBsstd8day') # SST, Aqua MODIS, NPP, 0.025 degrees, Pacific Ocean, Daytime, 2006-present (8 Day Composite)
sst8day <- rxtracto(SSTInfo, parameter = 'sst', 
                    xcoord = xpos, ycoord = ypos, 
                    tcoord = tpos, zcoord = zpos, xlen = 0, ylen = 0)

dat$sst <- sst8day$`mean sst`


#save as needed
dt<-Sys.Date()
saveRDS(dat,paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_",dt,".rda"))  
write.csv(dat,paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_",dt,".csv"))   
names(dat)
dat1<-dat%>%dplyr::select(eventID,TripNum)

saveRDS(dat1,paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_TripNum_",dt,".rda"))  

