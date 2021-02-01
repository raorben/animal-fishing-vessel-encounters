library(ggplot2)
library(dplyr)
library(tidyr)

#RACHAEL
if(Sys.info()[7]=="rachaelorben") {
  GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/"
  userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'}
if(Sys.info()[7]=="torresle") {
  GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/"
  userdir<-'C:/Users/leigh.torres/Dropbox (HMSC - OSU)/seabirds/GlobalFishingWatch'}

#birds<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_lowresDen_2019-05-05.rda"))  
birds<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_lowresDen_60km2020-08-24_interpinfo.rda"))  

birds$Territory1[is.na(birds$Territory1)==TRUE]<-"HighSeas"
birds$Territory1[birds$Territory1=="HighSeas" & birds$lat>50]<-"Alaska"
birds$Territory1[birds$eventID==330.0]<-"Russia"

birds%>%filter(species=="BFAL")%>%
  group_by(interactionYN,geartypeCLASS)%>%
  summarise(n=n())

df3=birds%>%filter(species=="STAL")%>%
  group_by(birdID,month,LonghurstCode)%>%
  summarise(fishingE=mean(fishinghrs_60km),
            fishingCT=mean(num_mmsi))

df3$LonghurstCode<-as.factor(df3$LonghurstCode)
df3$month<-as.factor(df3$month)
ggplot()+
  geom_point(data=df3%>%dplyr::filter(is.na(LonghurstCode)==FALSE), 
             aes(x=month, y=fishingE,group=LonghurstCode,color=LonghurstCode))+
  geom_smooth(data=df3%>%dplyr::filter(is.na(LonghurstCode)==FALSE), 
              aes(x=month, y=fishingE,group=LonghurstCode,color=LonghurstCode))+
  facet_wrap(~LonghurstCode)

ggplot(data=df3%>%dplyr::filter(is.na(LonghurstCode)==FALSE), aes(x=month, y=fishingCT, fill=LonghurstCode))+
  geom_boxplot()+ 
  scale_fill_brewer(palette="RdBu") + theme_minimal()

names(birds)
a<-birds%>%filter(species=="STAL")%>%
  group_by(birdID,dt_name)%>%
  summarise(n=n_distinct(mmsi))
max(a$n)
a%>%filter(n==40)
a%>%filter(n>10)
aa<-a%>%group_by(birdID)%>%summarise(u=mean(n))
mean(aa$u); sd(aa$u)
hist(a$n)
maxboats<-birds%>%filter(birdID==8236 & dt_name=="2014-02-22")
mean(maxboats$lat)
mean(maxboats$lon)

VT<-birds%>%filter(species=="STAL")%>%
  group_by(geartypeCLASS)%>%
  summarise(Enc=n())
1364/(sum(VT$Enc)-629)
428/1364

VTi<-birds%>%filter(species=="STAL" & interactionYN=="Y")%>%
  group_by(geartypeCLASS)%>%
  summarise(n=n())
428/(sum(VTi$n)-171)


VT<-left_join(VT,VTi)
VT$int_per<-VT$n/VT$Enc
VT


# April & December --------------------------------------------------------
w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("USA","Russia","China","Japan","Mexico","Canada","North Korea","South Korea","Taiwan","Mongolia"),]
wrap360 = function(lon) {
  lon360<-ifelse(lon<0,lon+360,lon)
  return(lon360)
}
ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  geom_point(data=birds%>%filter(species=="STAL")%>%
  filter(month==4 | month==12), aes(x=lon360,y=lat, color=as.factor(month)))+
  coord_fixed(ratio=1.7,xlim = c(140,235),ylim=c(25,60))+
  xlab("Longitude")+
  ylab("Latitude")

ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  geom_point(data=birds%>%filter(species=="STAL")%>%
               filter(month==12)%>%
               filter(Territory1=="Canada"), aes(x=lon360,y=lat, color=as.factor(interactionYN)))+
  coord_fixed(ratio=1.7,xlim = c(225,240),ylim=c(48,52))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(legend.position = 'none')

data=birds%>%filter(species=="STAL")%>%
  #filter(month==12)%>%
  filter(Territory1=="Canada")
data%>%summarize(n=n_distinct(birdID))
data%>%summarize(n=n_distinct(mmsi))
data%>%filter(interactionYN=="Y")%>%summarize(n=n())
6/14
unique(data$mmsi)
nrow(birds%>%filter(species=="STAL")%>%
  filter(month!=12)%>%
  filter(Territory1=="Canada"))
nrow(birds%>%filter(species=="STAL")%>%
         filter(month!=12)%>%
         filter(Territory1=="Canada")%>%
         filter(interactionYN=="Y"))

14/49
7/16
ggplot()+
  geom_bar(data=birds%>%filter(species=="STAL")%>%
             filter(Territory1=="Canada"), aes(x=monthb, color=interactionYN))

#https://www.myshiptracking.com/vessels/zeal-mmsi-316001025-imo-6420513
#https://www.marinetraffic.com/en/ais/details/ships/shipid:380709/mmsi:316005448/imo:7919858/vessel:VIKING_STORM
#https://www.marinetraffic.com/en/ais/details/ships/shipid:380010/mmsi:316001821/imo:7628473/vessel:FROSTI
#https://www.marinetraffic.com/en/ais/details/ships/shipid:899073/mmsi:316003448/imo:8803290/vessel:ARCTIC_OCEAN
#https://www.marinetraffic.com/en/ais/details/ships/shipid:380846/mmsi:316005998/imo:9158173/vessel:OSPREY_NO_1
#https://www.marinetraffic.com/en/ais/details/ships/shipid:381581/mmsi:316012260/imo:8802404/vessel:VIKING_ENTERPRISE
#https://www.vesselfinder.com/vessels/NORTHERN-ALLIANCE-IMO-8714437-MMSI-316022149
#https://www.marinetraffic.com/en/ais/details/ships/shipid:382914/mmsi:316023833/imo:9185188/vessel:RAW_SPIRIT
ggplot()+
  geom_histogram(data=data,aes(x=length))
names(birds)


ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  geom_point(data=birds%>%filter(species=="STAL")%>%
               filter(month==4), aes(x=lon360,y=lat, color=as.factor(interactionYN)))+
  coord_fixed(ratio=1.7,xlim = c(140,235),ylim=c(25,60))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(legend.position = 'none')
data4=birds%>%filter(species=="STAL")%>%
  filter(month==4)
data4%>%summarize(n=n_distinct(birdID))
data4%>%summarize(n=n_distinct(mmsi))
data4%>%filter(interactionYN=="Y")%>%summarize(n=n())
36/79
unique(data4$mmsi)

ggplot()+
  geom_bar(data=data4,aes(x=LonghurstCode, group=geartypeCLASS, fill=geartypeCLASS),
           position="dodge")
names(birds)


# DayNightDusk ------------------------------------------------------------
birds%>%filter(species=="STAL")%>%
  filter(is.na(DayNightDusk)==FALSE)%>%
  group_by(DayNightDusk)%>%
  summarise(n=n())

birds%>%filter(species=="STAL")%>%
  filter(is.na(DayNightDusk)==FALSE)%>%
  group_by(interactionYN,DayNightDusk)%>%
  summarise(n=n())
394/1365 #day interactions
43/160 #dusk
55/206 #night

birds%>%filter(species=="STAL")%>%
  filter(interactionYN=="Y")%>%filter(DayNightDusk=="N")%>%
  group_by(geartypeCLASS)%>%
  summarise(n=n())

#12 Birds (many tracks) at Kaena in 2014, 
#12 birds on Kauai 2014, and 
#12 from Kauai 2016 = 36 total birds prob >5 times as many "tracks"

birds%>%filter(species=="LAAL")%>%
  group_by(birdID,yearT)%>%
  summarise(n=n())


birds%>%filter(species=="LAAL")%>%
  group_by(flag)%>%
  summarise(n=n())
39/(39+3+7+3+4)
birds%>%filter(species=="LAAL")%>%
  group_by(flag, Territory1)%>%
  summarise(n=n())

birds%>%filter(species=="LAAL")%>%
  group_by(Territory1, geartypeCLASS)%>%
  summarise(n=n())
v<-birds%>%filter(species=="LAAL")%>%
  filter(geartypeCLASS=="Unknown")

names(birds)
str(birds$time3_min)
str(birds$species)
birds$Species<-as.factor(birds$species)
birds$birdID<-as.factor(birds$birdID)
birds$tmin_3
library(lme4)
m1<-lmer(time3_min~species+(1|birdID), data=birds%>%filter(time3_min!=0))
summary(m1)
aov(m1)
m1<-lmer(time3_min~species+(1|birdID), data=birds%>%filter(time3_min!=0),REML=FALSE)
m0<-lmer(time3_min~(1|birdID), data=birds%>%filter(time3_min!=0),REML=FALSE)
anova(m0,m1)
coef(m1)


# Interaction Duration mixed effects model --------------------------------
library(nlme)
citation("nlme")
m1<-lme(log(time3_min)~species, random=~1|birdID, data=birds%>%filter(time3_min!=0))
summary(m1)
anova(m1)
plot(m1)

# BFAL --------------------------------------------------------------------
library(Hmisc)
#library(rjags)
#library(prevalence)
#propCI(x=13, n=21)

#In this case, you have binomial distribution, 
#so you will be calculating binomial proportion confidence interval.
#In R, you can use binconf() from package Hmisc

# binomial confidence intervals -------------------------------------------
binconf(x=13, n=21)
#PointEst     Lower     Upper
#0.6190476 0.4087865 0.7924899

#Or you can calculate it yourself:
#p <- 520/1000
#p + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/1000)*p*(1-p))


bfal_s<-birds%>%filter(species=="BFAL")%>%
  dplyr::select(eventID, mmsi, birdID,flag, geartypeCLASS,interactionYN,time3_min,DayNightDusk, date_min, length, Territory1)
bfal_s%>%group_by(birdID,interactionYN)%>%summarise(n=n())

bfal_s$geartypeCLASS[bfal_s$mmsi==431702140]<-"Trawlers"#https://www.marinetraffic.com/en/ais/details/ships/440873000
bfal_s$geartypeCLASS[bfal_s$mmsi==367509670]<-"Longliners"#367509670 KAIA: Halibut IFQ Fleet / Sablefish Fleet
bfal_s$geartypeCLASS[bfal_s$mmsi==368698000]<-"Trawlers"#GOLDEN PISCES: AFA Catcher Vessel Fleet
bfal_s$geartypeCLASS[bfal_s$mmsi==367390410]<-"Trawlers"#ADVANCER: WGOA trawl fleet, 2010. Groundfish Pot Fleet / Western GOA Trawl Fleet 
bfal_s$geartypeCLASS[bfal_s$mmsi==367154110]<-"Trawlers"#OCEAN HUNTER: Vessels active in the Non- AFA BSAI trawl fleet, 2010. 
bfal_s$geartypeCLASS[bfal_s$mmsi==431431000]<-"Trawlers"#SEIYO MARU: IMO:8701363, SEIYO MARU

bfal_s
bfal_s%>%group_by(birdID,interactionYN)%>%summarise(n=n())%>%spread(interactionYN, n)
bfal_s%>%group_by(geartypeCLASS)%>%summarise(n=n())
bfal_s%>%group_by(geartypeCLASS,interactionYN)%>%summarise(n=n())%>%spread(interactionYN, n)
#Encounters EEZ vs. high-seas
bfal_s%>%group_by(Territory1)%>%summarise(n=n())
binconf(x=12, n=21)
binconf(x=9, n=21)#<-high-seas

#Interactions EEZ vs. high-seas
bfal_s%>%filter(interactionYN=="Y")%>%group_by(Territory1)%>%summarise(n=n())
binconf(x=9, n=13)#<-eezs
binconf(x=4, n=13)

bfal_s%>%filter(Territory1=="HighSeas")%>%group_by(interactionYN)%>%summarise(n=n())
4/9
bfal_s%>%filter(Territory1!="HighSeas")%>%group_by(interactionYN)%>%summarise(n=n())
9/11

bfal_s%>%group_by(flag,interactionYN)%>%summarise(n=n())%>%spread(interactionYN, n)
bfal_s%>%group_by(flag)%>%summarise(n=n())
bfal_s%>%group_by(flag,Territory1)%>%summarise(n=n())%>%spread(Territory1, n)
bfal_s%>%group_by(mmsi,geartypeCLASS,flag,interactionYN)%>%summarise(n=n_distinct(birdID))%>%spread(interactionYN, n)

#interaction duration
a<-bfal_s%>%filter(interactionYN=="Y")
mean(a$time3_min)/60; sd(a$time3_min)/60

bfal_s%>%filter(mmsi==440873000)
bfal_s%>%filter(birdID==259817)%>%
  group_by(geartypeCLASS,flag, interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)

bfald<-bfal%>%filter(interactionYN=="Y")%>%filter(DayNightDusk=="N")
unique(bfal$mmsi)

birds%>%filter(species=="BFAL")%>%
  group_by(flag, interactionYN)%>%
  summarise(n=n())

birds%>%filter(species=="BFAL")%>%
  group_by(Territory1, interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)

birds%>%filter(species=="BFAL")%>%
  group_by(DayNightDusk, interactionYN)%>%
  summarise(n=n())
v<-birds%>%filter(species=="BFAL")%>%
  filter(geartypeCLASS=="Unknown")

bts<-bfal_s%>%group_by(mmsi,flag,geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(sumE=sum(N,Y,na.rm=TRUE))%>%mutate(interR=Y/sumE)
bts$interR[is.na(bts$interR)==TRUE]<-0
bts_he<-bts%>%data.frame()
bts_he$mmsi<-as.character(bts_he$mmsi)
binfo<-bfal_s%>%dplyr::select(mmsi, length)%>%distinct()%>%data.frame()
binfo$mmsi<-as.character(binfo$mmsi)
bts_he<-dplyr::left_join(bts_he,binfo,by=c("mmsi"))
birdsboats<-bfal_s%>%group_by(mmsi)%>%
  summarise(nbirds=n_distinct(birdID))
birdsboats$mmsi<-as.character(birdsboats$mmsi)
bts_he<-dplyr::left_join(bts_he,birdsboats,by=c("mmsi"))


#Vessels encountered 10 or more tims by BFAL w/ >50% interaction rates
quartz(height=4,width=6)
ggplot()+
  geom_density2d(data=bts_he,aes(x=length, y=interR*100), linetype=3,color="black",alpha=.5)+
  geom_point(data=bts_he,aes(x=length, y=interR*100, color=geartypeCLASS, shape=flag, size=sumE))+
  geom_hline(yintercept = 50, linetype=2) + 
  scale_color_brewer(palette="Set1",direction = -1)+
  xlab("Vessel Length (m)")+
  ylab("Interactions (%)")+
  labs(shape="Vessel Flag", colour="Vessel Type", size="Number of Encounters")+
  theme_classic()
bts_he%>%filter(interR>0.5)
ggsave(width = 6, height=6.5, paste0(userdir,"/Analysis/PLOTS/BFAL_AllVessels.png"), dpi=600)

a<-(birds%>%filter(interactionYN=="Y")%>%dplyr::select(time3_min))
mean(a$time3_min)/60
# LAAL --------------------------------------------------------------------
binconf(x=20, n=56)

laal_s<-birds%>%filter(species=="LAAL")%>%
  dplyr::select(eventID, mmsi, birdID,flag, geartypeCLASS,interactionYN,time3_min, DayNightDusk, date_min, length, Territory1,perTrip)
length(unique(laal_s$mmsi))

(un<-laal_s%>%filter(geartypeCLASS=="Unknown"))
laal_s$geartypeCLASS[laal_s$mmsi==367411730]<-"Longliners"#Fontier Mariner: BSAI freezer longliner fleet,2010. 
laal_s$geartypeCLASS[laal_s$mmsi==367185050]<-"Longliners" #halibut longline & groundfish pot fleet, 2010. 
laal_s$geartypeCLASS[laal_s$mmsi==367003760]<-"Unknown" #SEA HUNT
laal_s$geartypeCLASS[laal_s$mmsi==367114110]<-"Longliners" #QUEEN DIAMOND; https://www.marinetraffic.com/en/ais/details/ships/367114110
laal_s$geartypeCLASS[laal_s$mmsi==367479170]<-"Longliners" #Captain D; side door; https://www.marinetraffic.com/en/ais/details/ships/shipid:4193976/mmsi:367479170/imo:8847777/vessel:CAPTAIN_D
laal_s$geartypeCLASS[laal_s$mmsi==367128430]<-"Longliners"#SeaMoon; https://www.marinetraffic.com/en/ais/details/ships/shipid:4186726/mmsi:367128430/imo:0/vessel:SEA_MOON_I
laal_s$geartypeCLASS[laal_s$mmsi==367159690]<-"Longliners"# CAPT KEVIN; https://www.marinetraffic.com/en/ais/details/ships/shipid:4031823/mmsi:367159690/imo:8855126/vessel:CAPT_KEVIN
laal_s$geartypeCLASS[laal_s$mmsi==367602740]<-"Longliners"#AOLANI ; https://www.marinetraffic.com/en/ais/details/ships/367602740
laal_s$geartypeCLASS[laal_s$mmsi==412420578]<-"squid_jigger" #RUNDA605; https://www.myshiptracking.com/vessels/runda605-mmsi-412420578-imo-
laal_s$geartypeCLASS[laal_s$mmsi==432906000]<-"Unknown"# SHOSHIN MARU NO.21
laal_s$geartypeCLASS[laal_s$mmsi==368583000]<-"Longliners"#Alaska Mist; BSAI freezer longliner fleet,2010. Sablefish IFQ Fleet, 2010. 
laal_s$geartypeCLASS[laal_s$mmsi==431700850]<-"Unknown"#NO23 CHIDORI MARU
laal_s$geartypeCLASS[laal_s$mmsi==431704510]<-"Unknown"#NO68 GENEI MARU
laal_s
#interaction duration
a<-laal_s%>%filter(interactionYN=="Y")
mean(a$time3_min)/60; sd(a$time3_min)/60

a<-laal_s%>%group_by(birdID)%>%summarise(n=n())
mean(a$n); sd(a$n)
laal_s%>%group_by(birdID,interactionYN)%>%summarise(n=n())%>%spread(interactionYN, n)
laal_s%>%group_by(Territory1,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%
  mutate(sumE=sum(N,Y,na.rm=TRUE))%>%mutate(interR=Y/sumE)

laal_s%>%group_by(geartypeCLASS,interactionYN)%>%summarise(n=n())%>%spread(interactionYN, n)
laal_s%>%group_by(geartypeCLASS)%>%summarise(n=n())

#Encounters EEZ vs. high-seas
laal_s%>%group_by(Territory1)%>%summarise(n=n())
binconf(x=26, n=56)
binconf(x=30, n=56)
#Interactions EEZ vs. high-seas
laal_s%>%filter(interactionYN=="Y")%>%group_by(Territory1)%>%summarise(n=n())
binconf(x=11, n=20)

laal_s%>%group_by(flag,Territory1)%>%summarise(n=n())%>%spread(Territory1, n)
bts<-laal_s%>%group_by(mmsi,geartypeCLASS,flag,interactionYN)%>%summarise(n=n_distinct(birdID))%>%spread(interactionYN, n)

laal_s%>%filter(birdID==1207)
laal_s%>%filter(birdID==1131)
laal_s%>%filter(mmsi==367460990) #finback

bts<-laal_s%>%group_by(mmsi,flag,geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(sumE=sum(N,Y,na.rm=TRUE))%>%mutate(interR=Y/sumE)
bts$interR[is.na(bts$interR)==TRUE]<-0
bts_he<-bts%>%data.frame()
bts_he$mmsi<-as.character(bts_he$mmsi)
binfo<-laal_s%>%dplyr::select(mmsi, length)%>%distinct()%>%data.frame()
binfo$mmsi<-as.character(binfo$mmsi)
bts_he<-dplyr::left_join(bts_he,binfo,by=c("mmsi"))
birdsboats<-laal_s%>%group_by(mmsi)%>%
  summarise(nbirds=n_distinct(birdID))
birdsboats$mmsi<-as.character(birdsboats$mmsi)
bts_he<-dplyr::left_join(bts_he,birdsboats,by=c("mmsi"))

#Vessels encountered 10 or more tims by STAL w/ >50% interaction rates
ggplot()+
  geom_density2d(data=bts_he,aes(x=length, y=interR*100), linetype=3,color="black",alpha=.5)+
  geom_point(data=bts_he,aes(x=length, y=interR*100, color=geartypeCLASS, shape=flag, size=sumE))+
  geom_hline(yintercept = 50, linetype=2) + 
  scale_color_brewer(palette="Set1",direction = -1)+
  xlab("Vessel Length (m)")+
  ylab("Interaction Rate (%)")+
  labs(shape="Vessel Flag", colour="Vessel Type", size="Number of Encounters")+
  theme_classic()
bts_he%>%filter(interR>0.5)
ggsave(width = 6, height=6.5, paste0(userdir,"/Analysis/PLOTS/LAAL_AllVessels.png"), dpi=600)



# STAL --------------------------------------------------------------------
#interactions to encounters
binconf(x=496, n=1735)

stal_s<-birds%>%filter(species=="STAL")
stal_s%>%group_by(birdID,interactionYN)%>%summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(interR=Y/(N+Y))

#Encounters EEZ vs. high-seas
stal_s%>%group_by(Territory1)%>%summarise(n=n())
binconf(x=1735, n=1735)
#Interactions EEZ vs. high seas
binconf(x=718, n=720)
stal_s%>%filter(interactionYN=="Y")%>%group_by(Territory1)%>%summarise(n=n())
stal_s%>%group_by(geartypeCLASS)%>%summarise(n=n())
stal_s%>%group_by(birdID,Territory1)%>%summarise(n=n())%>%spread(Territory1, n)
a<-stal_s%>%group_by(birdID,dt_name)%>%summarise(n=n())
max(a$n)
a[a$n==40,]
stal_s%>%filter(interactionYN=="Y", Territory1=="HighSeas")

bts<-stal_s%>%group_by(mmsi,flag,geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(sumE=sum(N+Y),interR=Y/(N+Y))
bts_he<-bts%>%filter(sumE>5)%>%data.frame()
bts.loc<-stal_s%>%group_by(mmsi,flag,geartypeCLASS)%>%
  summarise(meanlat=mean(env_lat),meanlon=mean(wrap360(env_lon)),
            sdlat=sd(env_lat))
bts_he<-left_join(bts_he, bts.loc%>%ungroup()%>%select(-flag, -geartypeCLASS), by=c("mmsi"="mmsi"))

bts_he$mmsi<-as.character(bts_he$mmsi)
binfo<-stal_s%>%dplyr::select(mmsi, length)%>%distinct()%>%data.frame()
binfo$mmsi<-as.character(binfo$mmsi)
bts_he<-dplyr::left_join(bts_he,binfo,by=c("mmsi"))
birdsboats<-stal_s%>%group_by(mmsi)%>%
  summarise(nbirds=n_distinct(birdID))
birdsboats$mmsi<-as.character(birdsboats$mmsi)
bts_he<-dplyr::left_join(bts_he,birdsboats,by=c("mmsi"))

#interaction duration
a<-stal_s%>%filter(interactionYN=="Y")
mean(a$time3_min)/60; sd(a$time3_min)/60; max(a$time3_min/60)

mean(stal_s$time30_min)/60; sd(stal_s$time30_min)/60; max(stal_s$time30_min/60)

birds%>%ungroup()%>%
  group_by(species)%>%
  summarise(u=mean(time30_min/60),
         sd=sd(time30_min/60),
         mx=max(time30_min/60),
         n=n())

m1<-lm(time30_min~interactionYN+species,data=birds)
summary(m1)

birds%>%ungroup()%>%
  group_by(species,interactionYN)%>%
  summarise(u=mean(time30_min/60),
            sd=sd(time30_min/60),
            mx=max(time30_min/60),
            n=n())
# Figure 2 ----------------------------------------------------------------
bts_he$geartypeCLASS[bts_he$geartypeCLASS=="Fish_factory"]<-"Fish Factory or Reefer"
bts_he$geartypeCLASS[bts_he$geartypeCLASS=="Reefer"]<-"Fish Factory or Reefer"
bts_he$geartypeCLASS[bts_he$geartypeCLASS=="pole_and_line"]<-"Pole & Line"
bts_he$geartypeCLASS[bts_he$geartypeCLASS=="pots_and_traps"]<-"Pots & Traps"

#Vessels encountered 10 or more tims by STAL w/ >50% interaction rates
quartz(height=6.5, width=7.5)
ggplot()+
  geom_density2d(data=bts_he,aes(x=length, y=interR*100), linetype=3,color="black",alpha=.5)+
  geom_point(data=bts_he,
             aes(x=length, y=interR*100, color=geartypeCLASS, shape=flag, size=sumE))+
  geom_hline(yintercept = 50, linetype=2) + 
  scale_color_brewer(palette="Set1",direction = -1)+
  xlab("Vessel Length (m)")+
  ylab("Interactions (%)")+
  labs(shape="Vessel Flag", colour="Vessel Type", size="Number of Encounters")+
  theme_classic()
ggsave(height=6.5, width=7.5, paste0(userdir,"/Analysis/PLOTS/STAL_AllVessels.png"), dpi=600)

nrow(bts_he%>%filter(interR>0.5))


#       mmsi flag geartypeCLASS N Y sumE    interR   length nbirds
#1 273240030  RUS      Trawlers 4 7   11 0.6363636 57.82000      6#YASNYY
#2 273825010  RUS      Trawlers 5 9   14 0.6428571 46.95037      5#DOLOMIT
#3 273828010  RUS      Trawlers 4 6   10 0.6000000 46.82064      5#KLIMOVO 
#4 303686000  USA      Trawlers 3 7   10 0.7000000 85.40000      7#Golden Alaska
#5 338185000  USA    Longliners 5 7   12 0.5833333 37.95732      5#BlueAttu
#6 366297000  USA    Longliners 7 8   15 0.5333333 34.60366      9#CLIPPER ENDEAVOR
#7 366705860  USA      Trawlers 7 8   15 0.5333333 61.79878      6#ALASKA SPIRIT 
#8 367411730  USA       Unknown 6 7   13 0.5384615 46.15707      7#FRONTIER MARINER
#9 367694000  USA      Trawlers 7 8   15 0.5333333 37.42357      7#PACIFIC PRINCE

stal_s%>%group_by(geartypeCLASS,interactionYN)%>%summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))
stal_s%>%group_by(flag,interactionYN)%>%summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))

a<-stal_s%>%group_by(flag,geartypeCLASS,Territory1,interactionYN)%>%summarise(n=n())%>%
  spread(interactionYN, n)%>%
  mutate(total=Y+N,interR=Y/(N+Y))%>%
  filter(is.na(interR)==FALSE)%>%
  filter(total>10)

nrow(stal_s%>%filter(geartypeCLASS=="Trawlers"))
stal_s%>%filter(geartypeCLASS=="Trawlers")%>%
  group_by(flag,geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))
stal_s%>%filter(geartypeCLASS=="Trawlers")%>%
  group_by(flag,geartypeCLASS)%>%
  summarise(n=n())%>%mutate(Per=(n/1364)*100)
stal_s%>%filter(flag=="KNA")

stal_s%>%filter(geartypeCLASS=="Longliners")%>%
  group_by(Territory1,flag,geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))
stal_s%>%filter(geartypeCLASS=="Longliners")%>%
  group_by(Territory1,geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))
stal_s%>%filter(geartypeCLASS=="Longliners")%>%
  group_by(flag,geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))
stal_s%>%filter(geartypeCLASS=="Longliners")%>%
  group_by(flag,geartypeCLASS,interactionYN)%>%
  summarise(n=n(),Vlength=mean(length))
stal_s%>%filter(geartypeCLASS=="Longliners")%>%
  group_by(flag,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))
stal_s%>%filter(Territory1=="Canada")%>%
  filter(month==12)%>%
  group_by(interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))
stal_s%>%filter(Territory1=="Canada")%>%
  filter(month==12)

a<-stal_s%>%group_by(Territory1,geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))

#interaction rate 55%
stal_s%>%filter(birdID==8374)%>%
  group_by(Territory1,geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))

#interaction rate 53%
stal_s%>%filter(birdID==8373)%>%
  group_by(Territory1,geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))

#interaction rate 32%
stal_s%>%filter(birdID==3323)%>%
  group_by(Territory1,geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))

stal_s%>%filter(birdID==8236)%>%filter(dt_name=="2014-02-22")%>%filter(interactionYN=="Y")
stal_s%>%filter(birdID==8236)%>%filter(dt_name=="2014-02-22")%>%
  group_by(Territory1,geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))

stal_s%>%filter(deploy.count>1200)%>%
  group_by(geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))
stal_s%>%filter(deploy.count>1200)%>%
  group_by(birdID,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))

stal_s%>%filter(deploy.count<1200)%>%
  group_by(birdID,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))
stal_s%>%filter(deploy.count<1200)%>%
  group_by(interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))

a<-stal_s%>%group_by(birdID,interactionYN)%>%summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(interR=Y/(N+Y))
b<-stal_s%>%group_by(birdID,interactionYN)%>%filter(geartypeCLASS=="Trawlers")%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(interTrawl=Y/(N+Y))
c<-stal_s%>%group_by(birdID,interactionYN)%>%filter(geartypeCLASS!="Trawlers")%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(interNOTTrawl=Y/(N+Y))
ab<-left_join(a%>%dplyr::select(birdID,interR),b%>%dplyr::select(birdID,interTrawl))
left_join(ab,c%>%dplyr::select(birdID,interNOTTrawl))%>%mutate(diff=interNOTTrawl-interTrawl)
 
b<-stal_s%>%group_by(birdID,date_min)%>%mutate(days=(date_max+1)-date_min)
sum(b$days) 
b<-stal_s%>%group_by(birdID,date_min)%>%summarise(n=n())

stal_s%>%filter(yearT==2014)%>%filter(monthb==12)
names(stal_s)

a<-birds%>%dplyr::filter(time3_min!=0)
M1<-aov(time3_min~species, data=a)
summary(M1)
aov(M1)
plot(M1)


#calculate wind speed from u and v
#sqrt(u^2+v^2)
stal_s$wind_sp <- sqrt((stal_s$wind_u*stal_s$wind_u)+(stal_s$wind_v*stal_s$wind_v))
stal_s<-stal_s%>%mutate(windC=round(wind_sp,0))

a<-stal_s%>%group_by(windC,interactionYN)%>%summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(interR=Y/(N+Y))

ggplot()+
  geom_point(data=a,aes(x=windC, y=interR))

stal_s%>%filter(windC<5)%>%group_by(interactionYN)%>%summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(interR=Y/(N+Y))
stal_s%>%filter(windC>15)%>%group_by(interactionYN)%>%summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(interR=Y/(N+Y))

stal_s%>%filter(Longhurst=="Polar - N. Pacific Epicontinental Province")%>%
  group_by(interactionYN)%>%summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(interR=Y/(N+Y))
stal_s$BoO<-"Okhotsk"
stal_s$BoO[stal_s$lon360>162]<-"Bering"
stal_s$BoO[stal_s$Longhurst!="Polar - N. Pacific Epicontinental Province"]<-NA

ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  geom_point(data=stal_s%>%filter(is.na(BoO)==FALSE), 
             aes(x=lon360,y=lat, color=as.factor(BoO)))+
  coord_fixed(ratio=1.7,xlim = c(140,200),ylim=c(40,65))+
  xlab("Longitude")+
  ylab("Latitude")

stal_s%>%filter(is.na(BoO)==FALSE)%>%
  group_by(BoO,interactionYN)%>%summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(interR=Y/(N+Y))

stal_s%>%filter(is.na(BoO)==FALSE)%>%
  group_by(BoO,flag,interactionYN)%>%summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(interR=Y/(N+Y))

stal_s%>%filter(is.na(BoO)==FALSE)%>%
  group_by(BoO,geartypeCLASS,interactionYN)%>%
  summarise(n=n())%>%
  spread(interactionYN, n)%>%mutate(interR=Y/(N+Y))


# boats -------------------------------------------------------------------
names(birds)
boats<-unique(birds%>%dplyr::select(mmsi, length, engine_power,Territory1,geartypeCLASS,interactionYN,species))

boats%>%filter(species=="STAL")%>%
  #filter(interactionYN=="Y")%>%
  filter(geartypeCLASS=="Longliners")%>%
  group_by(Territory1)%>%
  summarise(n=n(), 
            meanlength=mean(length), 
            sdlength=sd(length), 
            minlength=min(length),
            maxlength=max(length),
            meanengine=mean(engine_power))
  


(boatsenc_19<-birds%>%select(mmsi, species,length, engine_power,Territory1,geartypeCLASS,interactionYN)%>%
    filter(length<19))

(boatsenc_19%>%
  group_by(species,Territory1,geartypeCLASS)%>%
  summarise(n=n()))

(boatsenc_19%>%
    group_by(Territory1)%>%
    summarise(n=n()))

(boatsenc_19%>%
    group_by(Territory1,species)%>%
    summarise(n=n()))


boatsenc_19%>%summarise(n=n(), 
          meanlength=mean(length), 
          sdlength=sd(length), 
          minlength=min(length),
          maxlength=max(length),
          meanengine=mean(engine_power))

birds%>%filter(length<19)%>%
  group_by(species,interactionYN)%>%summarise(n=n())

71/78
  
#LAAL: 4/6 (0.6666667)
#STAL: 13/72 (0.18)


boats%>%summarise(n=n(), 
                  meanlength=mean(length), 
                  sdlength=sd(length), 
                  minlength=min(length),
                  maxlength=max(length),
                  meanengine=mean(engine_power))

a<-boats%>%filter(length<19)

mean(a$length)

LL<-boats%>%
  filter(geartypeCLASS=="Longliners")%>%
  group_by(Territory1, species)

birds%>%
  filter(geartypeCLASS=="Longliners")%>%
  group_by(species, interactionYN, Territory1)%>%
  summarise(n=n())
  

