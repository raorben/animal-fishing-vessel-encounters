library(ggplot2)
#library(plyr) 
library(dplyr)
library(mgcv)
library(gbm)
library(dismo)
library(raster)
library(PresenceAbsence)

library(grid)
library(gridExtra)
library(corrplot)
library(RColorBrewer)
library(pdp)
library(ape)

#These files ready to go for BRT models based on 27Sep2019: GlobalFishingWatch\Analysis\Models\EncTOint
#"albie_Enc2Int.RData"
#"STAL_enc.RData"
#"LAAL_enc.RData"
#"STAL_enc_int.RData"

#RACHAEL
if(Sys.info()[7]=="rachaelorben") {
  GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/"
  userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'}
if(Sys.info()[7]=="torresle") {
  GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/"
  userdir<-'C:/Users/leigh.torres/Dropbox (HMSC - OSU)/seabirds/GlobalFishingWatch'
  setwd('C:\\Users\\leigh.torres\\Dropbox (HMSC - OSU)\\seabirds\\GlobalFishingWatch\\Analysis\\Models\\EncTOint')
}
if(Sys.info()[7]=="Leigh Torres") { #if at home
  GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/"
  userdir<-'C:/Users/Leigh Torres/Dropbox (HMSC - OSU)/seabirds/GlobalFishingWatch'
  #at home
  setwd('C:\\Users\\torres_l\\Dropbox (HMSC - OSU)\\seabirds\\GlobalFishingWatch\\Analysis\\Models\\EncTOint')
} 

#can just load albie_Enc2Int.RData and skip to modeling: now includes all added dat including fishing density and updated territory data. 
#
#this file has updated EEZvsHighSeas, fisheries management factors
load(paste0(userdir,"/Analysis/compileddata/albie_Enc2Int_2019-09-24.RData"))
#this hes 60km fishdensity
fd<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_lowresDen_60km2019-09-27_interpinfo.rda"))
FD<-fd%>%dplyr::select(eventID,fishingden_60km,EncounterData,InteractionTimeDiff,EncounterDataPer)
dat<-left_join(dat,FD,by="eventID")
#after loading go to section BRT models


#setwd('C:\\Users\\leigh.torres\\Dropbox (HMSC - OSU)\\seabirds\\GlobalFishingWatch\\Analysis\\Models\\EncTOint')
#at home
#setwd('C:\\Users\\Leigh Torres\\Dropbox (HMSC - OSU)\\seabirds\\GlobalFishingWatch\\Analysis\\Models\\EncTOint')

#skip now becasue of Rachael's code below:
#load("C:/Users/leigh.torres/Dropbox (HMSC - OSU)/seabirds/GlobalFishingWatch/Analysis/Models/EncTOint/albie_Enc2Int.RData")
#dat <- readRDS("GFW_Albie_EncounterInteraction.rda") 

#dat <- readRDS("GFW_Albie_EncounterInteraction.rda") 
#dat1<-dat[1,]
#str(dat1)




# Data prep ---------------------------------------------------------------

#to get original column order

#load("C:/Users/leigh.torres/Dropbox (HMSC - OSU)/seabirds/GlobalFishingWatch/Analysis/Models/EncTOint/albie_Enc2Int.RData")
#load("C:/Users/Leigh Torres/Dropbox (HMSC - OSU)/seabirds/GlobalFishingWatch/Analysis/Models/EncTOint/albie_Enc2Int.RData") #if at home
load(paste0(userdir,"/Analysis/Models/EncTOint/albie_Enc2Int.RData")) 
dat1<-dat[1,]
str(dat1)

#This is updated data with fishing density info.
#dat<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_lowresDen_2019-05-05.rda"))  

dat$log_chla<-log(dat$chla)
names(dat)

#calculate wind speed from u and v
#sqrt(u^2+v^2)
dat$wind_sp <- sqrt((dat$wind_u*dat$wind_u)+(dat$wind_v*dat$wind_v))

#have to change NaN to NA
dat$wind_sp[is.nan(dat$wind_sp)] <- NA
dat$sst[is.nan(dat$sst)] <- NA
dat$log_chla[is.nan(dat$log_chla)] <- NA

dat$Territory1[is.na(dat$Territory1)==TRUE]<-"HighSeas"
dat$Territory1[dat$Territory1=="HighSeas" & dat$lat>50]<-"Alaska"
dat$Territory1[dat$eventID=="330.0"]<-"Russia"
dat$Territory1[dat$eventID=="714.0"]<-"Russia"

#adds in new factors: EEZ vs. HighSeas  / EEZ + FAOs
dat$EEZHS<-as.character(dat$Territory1)
dat$EEZHS[dat$EEZHS!="HighSeas"]<-"EEZ"

dat$FishManage<-as.character(dat$Territory1)
dat$FishManage[dat$EEZHS=="HighSeas"]<-dat$F_CODE[dat$EEZHS=="HighSeas"]

ggplot()+
  geom_point(data=dat,aes(x=lon360,y=lat, color=FishManage))+facet_wrap(~species)
ggplot()+
  geom_point(data=dat,aes(x=lon360,y=lat, color=Territory1))+facet_wrap(~species)
ggplot()+
  geom_histogram(data=dat,aes(x=Territory1),stat="count")+facet_wrap(~species)
nrow(dat%>%filter(species=="STAL")%>%filter(Territory1=="HighSeas"))#18 high seas encounters-2 inters (11%)<-low
nrow(dat%>%filter(species=="LAAL")%>%filter(Territory1=="HighSeas"))#30 high seas encounters-2 inters (30%)<-ave.

#turn characters into factors for model
dat$DayNightDusk <- as.factor(dat$DayNightDusk)
dat$flag <- as.factor(dat$flag)
dat$geartypeCLASS <- as.factor(dat$geartypeCLASS)
dat$RST_domstate_24hr <- as.factor(dat$RST_domstate_24hr)
dat$RST_domstate_2hr <- as.factor(dat$RST_domstate_2hr)
dat$mmsi <- as.factor(dat$mmsi)
dat$bathyCat <- as.factor(dat$bathyCat)
dat$month <- as.factor(dat$month)
dat$FID <- as.factor(dat$FID)
dat$LonghurstCode <- as.factor(dat$LonghurstCode)
dat$Territory1 <- as.factor(dat$Territory1)
dat$colony <- as.factor(dat$colony)
dat$species <- as.factor(dat$species)
dat$birdID <- as.factor(dat$birdID)
dat$EEZHS<- as.factor(dat$EEZHS)
dat$FishManage<-as.factor(dat$FishManage)

#convert 'interactionYN' to binomial 1 or 0.
dat$int <- NA
dat$int[dat$interactionYN=="Y"] <- 1
dat$int[dat$interactionYN=="N"] <- 0

#make env_datetime into posix
dat$env_datetime_new <- as.POSIXct(dat$env_datetime, origin="1970-01-01")

#get rid of levels in flag that have no data
dat$flag <- factor(dat$flag)

#transform a few variables that show skewed distributions
#for RST_24Perrest and RST_per24trasnit that are both percentanges, I used a logit transofrmation (http://strata.uga.edu/8370/rtips/proportions.html)
#only problem is that values of 0% get a transformed value of -inf. so, first I added 0.001 to all values of 0.
#the logit function
logitTransform <- function(p) { log(p/(1-p)) }
#
test <- dat$RST_Per24rest
test <- ifelse(test == 0, 0.001, test)
dat$RST_per24rest_trans <- logitTransform(test)
#
test <- dat$RST_Per24transit
test <- ifelse(test == 0, 0.001, test)
dat$RST_per24trans_trans <- logitTransform(test)

#log depth
depth <- ifelse(dat$depth >= 0, NA, dat$depth) #get rid of values equal to or greater than 0
dat$depth_log <- log(depth*-1) #multiply by -1 (to turn all values positive) and then log

#log time3_min
dat$log_time3_min <- log(dat$time3_min) #multiply by -1 (to turn all values positive) and then log
dat$log_time3_min <- ifelse(dat$log_time3_min == -Inf, 0, dat$log_time3_min) #tunr -inf values to 0

#log time30_min
dat$log_time30_min <- log(dat$time30_min) #multiply by -1 (to turn all values positive) and then log
dat$log_time30_min <- ifelse(dat$log_time30_min == -Inf, 0, dat$log_time30_min) #tunr -inf values to 0


#make simplified gear type variable
dat$boattype_simpfact<-as.character(dat$geartypeCLASS)
dat$boattype_simpfact[dat$geartypeCLASS!="Longliners"& dat$geartypeCLASS!="Trawlers"]<-"Other"
dat$boattype_simpfact[dat$geartypeCLASS=="Unknown"]<-"Unknown"
dat$boattype_simpfact[is.na(dat$geartypeCLASS)==TRUE]<-"Unknown"
dat$boattype_simpfact<-as.factor(dat$boattype_simpfact)
summary(dat$boattype_simpfact)

#make simplified flag variable
dat$flag_simpfact<-as.character(dat$flag)
dat$flag_simpfact[dat$flag =="JPN" | dat$flag=="KHM" | dat$flag =="KNA" | dat$flag=="TWN"| dat$flag == "VUT"]<-"Other"
dat$flag_simpfact<-as.factor(dat$flag_simpfact)
summary(dat$flag_simpfact)

#to tabulate by two factors
flagclass<-dat%>%group_by(flag,geartypeCLASS)%>%summarise(n=n())

#log fishinghours within 120km
dat$log_fishinghrs_120km<-log(dat$fishinghrs_120km)

#forcing column order to be the same 
dat1$env_datetime<-dat$env_datetime[1]
dat<-bind_rows(dat1,dat)
dat<-dat[2:2501,]

dat$Territory1 <- as.factor(dat$Territory1)



# are varaibles correlated? -----------------------------------------------
#are varaibles correlated?
#must limit to continuous (not categorical) varaibles
my_data <- dat[, c(20,21,58,59,27,31,60,51,52,56,57,75,76)]
# print the first 6 rows
head(my_data, 6)
res <- cor(my_data, use = "complete.obs")
round(res, 2)
#looks like only engine power and length are correlated (rho=0.90)

#depth and bathyCat correlated (obviously)
ggplot(dat, aes(x=bathyCat, y=depth, fill=bathyCat)) + geom_boxplot() +
  guides(fill=FALSE)

#also correlation between vessel length and type of gear
ggplot(dat, aes(x=geartypeCLASS, y=length, fill=geartypeCLASS)) + geom_boxplot() +
  guides(fill=FALSE)

#also some correlation between Longhurst code and depth
ggplot(dat, aes(x=LonghurstCode, y=depth, fill=LonghurstCode)) + geom_boxplot() +
  guides(fill=FALSE)

#slight (min) correlation between Month and deploy.count (age) in STAL data
ggplot(dat%>%filter(species=="STAL"), aes(x=month, y=deploy.count, fill=month)) + 
  geom_boxplot() +
  guides(fill=FALSE)

#no real correlation between Month and sst in STAL data
ggplot(dat%>%filter(species=="STAL"), aes(x=month, y=sst, fill=month)) + 
  geom_boxplot() +
  guides(fill=FALSE)

#no correlation between RST_domstate_2hr and RST_Per24rest in STAL data
ggplot(dat%>%filter(species!="STAL"), aes(x=RST_domstate_2hr, y=RST_Per24rest, fill=RST_domstate_2hr)) + 
  geom_boxplot() +
  guides(fill=FALSE)

#relationship between boat length and flag nation: USA and CAn boats smaller
ggplot(dat, aes(x=flag, y=length, fill=flag)) + geom_boxplot() +
  guides(fill=FALSE)

#no clear relationship between gear type and vessel length
ggplot(dat, aes(x=boattype_simpfact, y=length, fill=boattype_simpfact)) + geom_boxplot() +
  guides(fill=FALSE)

ggplot(dat%>%filter(species=="STAL"), aes(x=interactionYN, y=depth, fill=int)) + geom_boxplot() +
  guides(fill=FALSE)

ggplot(dat, aes(x=birdID, y=time3_min, fill=birdID)) + geom_boxplot() +
  guides(fill=FALSE)

#relationship between deploy.count(age) and birdID for STALs
ggplot(dat%>%filter(species=="STAL"), aes(x=birdID, y=deploy.count, fill=birdID)) + 
  geom_boxplot() +
  guides(fill=FALSE)

#relationship between depth and DayNightDusk in STAL_enc data? Not much
ggplot(STAL_enc, aes(x=DayNightDusk, y=depth_log, fill=DayNightDusk)) + geom_boxplot() +
  guides(fill=FALSE)

#relationship between log_fishinghrs_120km and Longhurst in STAL_enc data? 
ggplot(STAL_enc, aes(x=LonghurstCode, y=log_fishinghrs_120km, fill=LonghurstCode)) + geom_boxplot() +
  guides(fill=FALSE)

#relationship between DayNightDusk and log_fishinghrs_120km in STAL_enc data
ggplot(STAL_enc, aes(x=DayNightDusk, y=log_fishinghrs_120km, fill=DayNightDusk)) + geom_boxplot() +
  guides(fill=FALSE)

ggplot(STAL_enc, aes(x=log_fishinghrs_120km, y= depth_log)) + geom_point () +
  guides(fill=FALSE)

#save as needed
dt<-Sys.Date()
save(dat, file = paste0("albie_Enc2Int_",dt,".RData"))



#plot distributions by species
ggplot(dat, aes(x=wind_sp)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(species ~ .)

ggplot(dat, aes(x=DayNightDusk)) + geom_histogram(binwidth=.5, colour="black", fill="white", stat="count") + 
  facet_grid(species ~ .)

p <- ggplot(dat%>%filter(species=="STAL"), aes(env_datetime_new, flight.oid))
p + geom_point(aes(colour = factor(birdID)))

####GAM 1#####
# 
#Did not use GAMs becasue too many categorical variables.
#These models ask what factors determine if an encounter turns into an interaction
STAL.gam_m8 <- gam(int ~ s(wind_sp)  + s(depth) + s(flight.oid) + s(RST_Per24rest) + s(RST_Per24transit) + 
                     s(sst) + s(log_chla) + s(length), data = STAL, family = "binomial")

#simplified model wil less factors
STAL.gam1 <- gam(int ~ s(wind_sp) + s(perTrip) + s(depth) + s(flight.oid) + s(RST_Per24rest) + s(RST_Per24transit) + 
                   s(RST_Per24actfor) + s(engine_power) + s(length) + factor(DayNightDusk) + factor(bathyCat) + 
                   factor(geartypeCLASS), data = STAL, family = "binomial")


summary(STAL.gam1)
plot(STAL.gam1, shade = TRUE, scale = 0)
#



# BRT Models --------------------------------------------------------------


names(dat)
#[1] "eventID"              "mmsi"                 "birdID"               "species"             
#[5] "nrow_boat"            "dur_Bird"             "time3_min"            "time30_min"          
#[9] "time80_min"           "zone"                 "nrow_boat.1"          "tmin_30"             
#[13] "tmax_30"              "tmin_3"               "tmax_3"               "date_min"            
#[17] "date_max"             "flag"                 "geartype"             "length"              
#[21] "engine_power"         "geartypeCLASS"        "interactionYN"        "uniID"               
#[25] "RST_Per24rest"        "RST_Per24transit"     "RST_Per24actfor"      "RST_domstate_2hr"    
#[29] "RST_domstate_24hr"    "RSTnrows"             "flight.oid"           "Territory1"          
#[33] "Longhurst"            "LonghurstCode"        "FID"                  "F_CODE"              
#[37] "colony"               "sunalt"               "DayNight"             "DayNightDusk"        
#[41] "nrowBird_within30"    "month"                "wind_v"               "wind_u"              
#[45] "depth"                "bathyCat"             "env_datetime"         "env_lat"             
#[49] "env_lon"              "tdiff_env"            "perTrip"              "wind_sp"             
#[53] "int"                  "env_datetime_new"     "chla"                 "sst"                 
#[57] "log_chla"             "RST_per24rest_trans"  "RST_per24trans_trans" "depth_log"   
#[61] "log_time3_min"        "flag_simpfact"        "log_time30_min"       "boattype_simpfact" 
#[65] "deploy.count"         "boatdays"             "yearT"                "monthb"              
#[69] "dt_name"              "lat"                  "lon"                  "lon360"              
#[73] "mmsiYN"               "num_mmsi"             "fishinghrs_120km"     "log_fishinghrs_120km"
#[77] "EEZHS"                "FishManage"           "fishingden_60km"       "fishingden_60km_log"

#EEZHS = inside a national EEZ or in high seas
#Fishmanage = compilation of spatial fishery management areas (including EEZ, FAOs, US regions X3)
#fishing density has zeros becasue this reflects fishing hours per grid squares, so bird can be 
#overlapping a non-fishing vessel

dat$fishingden_60km_log <- log(dat$fishingden_60km+1) #gotta add 1 so no inf values in dataframe

#try BRTs
#make dataframes of just STALs, and of LAAL and BFLA
STAL <- dat[dat$species == "STAL",]
#LABF <- dat[(dat$species == "LAAL") | (dat$species == "BFAL"),]
LAAL <- dat[(dat$species == "LAAL"),]
#BFAL <- dat[(dat$species == "BFAL"),]




STAL$geartypeCLASS <- droplevels(STAL$geartypeCLASS)
STAL$birdID <- droplevels(STAL$birdID)
STAL$FishManage<-droplevels(STAL$FishManage)
unique(STAL$FishManage)

LAAL$birdID <- droplevels(LAAL$birdID)
LAAL$geartypeCLASS <- droplevels(LAAL$geartypeCLASS)
LAAL$FishManage<-droplevels(LAAL$FishManage)
unique(LAAL$FishManage)

#for models of what drives time within 3 min of vessel
#we want to limit analysis to be of encounters with >5 points within 30 km
ggplot()+
  geom_histogram(data=STAL,aes(x=nrowBird_within30),binwidth=5)
summary(STAL$nrowBird_within30)
nrow(STAL%>%filter(nrowBird_within30<3))
summary(STAL$nrowBird_within30)
nrow(STAL%>%filter(nrow_boat.1<3))

#STAL_enc <- STAL[STAL$nrowBird_within30>=5,] #this is changing Aug 21, 2020 
#to reflect time to closest real location
STAL_enc <- STAL[STAL$nrow_boat.1>=5,]
STAL_Elist<-readRDS(paste0(userdir,"/Analysis/compileddata/STAL_eventlist_disttimegaps.rda"))  
#
#for Leigh's home computer
STAL_Elist<-readRDS("C:/Users/torres_l/Dropbox (HMSC - OSU)/seabirds/GlobalFishingWatch/Analysis/compileddata/STAL_eventlist_disttimegaps.rda")  

(tdiff<-STAL_Elist[[1]][1]) #time difference

events<-STAL_Elist[[1]][[2]] #chooses 3 as time diff allowed; if change first 2 to 1, makes it a 2 hr time diff

STAL_enc<-STAL_enc[STAL_enc$eventID%in%events,] #selects events less than time diff.
#summary(STAL_enc$nrowBird_within30)


#for LAAL
ggplot()+
  geom_histogram(data=LAAL,aes(x=nrowBird_within30),binwidth=5)
summary(LAAL$nrowBird_within30)
nrow(LAAL%>%filter(nrowBird_within30<5))
summary(LAAL$nrowBird_within30)
nrow(LAAL%>%filter(nrow_boat.1<5))

LAAL_enc <- LAAL[LAAL$nrowBird_within30>=5,] #none
LAAL_enc <- LAAL_enc[LAAL_enc$nrow_boat.1>=5,]
summary(LAAL_enc$nrowBird_within30)
#

#LAAL_int <- LAAL[(LAAL$int == 1),] #limit to just interactions, for models looking at time within 3km.
STAL_enc_int <- STAL_enc[(STAL_enc$int == 1),] #limit to just interactions, for models looking at time within 3km.
#LABF_int <- LABF[(LABF$int == 1),] #limit to just interactions, for models looking at time within 3km.

STAL_enc$geartypeCLASS <- droplevels(STAL_enc$geartypeCLASS)
STAL_enc$birdID <- droplevels(STAL_enc$birdID)
STAL_enc$FishManage <- droplevels(STAL_enc$FishManage)

STAL_enc_int$geartypeCLASS <- droplevels(STAL_enc_int$geartypeCLASS)
STAL_enc_int$birdID <- droplevels(STAL_enc_int$birdID)
STAL_enc_int$FishManage <- droplevels(STAL_enc_int$FishManage)

LAAL_enc$geartypeCLASS <- droplevels(LAAL_enc$geartypeCLASS)
LAAL_enc$birdID <- droplevels(LAAL_enc$birdID)
LAAL_enc$FishManage<- droplevels(LAAL_enc$FishManage)
summary(LAAL_enc$FishManage)

STAL_enc_BERS<-STAL_enc%>%dplyr::filter(LonghurstCode=="BERS")
table(STAL_enc_BERS$int)

STAL_enc_CCAL<-STAL_enc%>%dplyr::filter(LonghurstCode=="CCAL")
table(STAL_enc_CCAL$int)

STAL_enc_ALSK<-STAL_enc%>%dplyr::filter(LonghurstCode=="ALSK")
table(STAL_enc_ALSK$int)

ggplot()+
  geom_bar(data=STAL_enc,
           aes(x=month,group=int,fill=as.factor(int)),position="dodge")

ggplot()+
  geom_bar(data=STAL_enc,
           aes(x=month,group=int,fill=as.factor(int)),position="dodge")+
  facet_wrap(~LonghurstCode)

ggplot()+
  geom_bar(data=STAL_enc,
           aes(x=month,group=int,fill=as.factor(int)),position="dodge")+
  facet_wrap(~flag_simpfact)

ggplot()+
  geom_point(data=STAL_enc,
             aes(x=lon360,y=lat,color=as.factor(int)))+facet_wrap(~month)

#to select data by location
#wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}
#birdssel<-STAL_enc%>%dplyr::filter(env_lat>58 & env_lat<63)%>%
#  dplyr::filter(wrap360(env_lon)>175 & wrap360(env_lon)<185)

LAAL_enc$Territory1 <- droplevels(LAAL_enc$Territory1)
STAL_enc$Territory1 <- droplevels(STAL_enc$Territory1)

STAL_enc_int$LonghurstCode <- droplevels(STAL_enc_int$LonghurstCode)

dt<-Sys.Date()
saveRDS(LAAL_enc,paste0(userdir,"/Analysis/compileddata/LAAL_enc_",dt,".rds"))
saveRDS(STAL_enc,paste0(userdir,"/Analysis/compileddata/STAL_enc_",tdiff,"hr_",dt,".rds"))
saveRDS(STAL_enc_int,paste0(userdir,"/Analysis/compileddata/STAL_enc_int_",tdiff,"hr_",dt,".rds"))
