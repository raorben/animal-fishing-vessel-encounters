#install.packages("/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch/Analysis/scripts/anipaths_0.9.7.tar.gz", repos = NULL)
library(anipaths)
library(dplyr)
library(ggplot2)
library(lubridate)
library(rworldmap)
library(marmap)
wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}
unwrap360<-function(lon360){
  lon<-ifelse(lon360 > 180, -360 + lon360, lon360)
  return(lon)}

# Go find more data from .csvs
# Directory with daily GFW .csv files
if(Sys.info()[7]=="rachaelorben") GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/" ##RAO
if(Sys.info()[7]=="rachaelorben") userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'

#start running the code here. With the directories correctly identified above the code should be pointing to the right files.
alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/AlbatrossData_bytrip.rda"))
boats<-read.csv("/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch/GFW/allyear_vessel_overlap.csv")
prox.sum<-readRDS(paste0(userdir,"/Analysis/compileddata/proxsummary.rda"))
Elocs<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_2019-02-25.rda"))  


#interactionnumbers<-interactionnumbers%>%filter(colony=="MIAT")
boats$dt<-as.character(boats$timestamp)
boats$datetime_GMT<-ymd_hms(boats$dt)
tz(boats$datetime_GMT)<-"GMT"
boats$date<-as.character(date(boats$datetime_GMT))

alllocs$date<-as.character(date(alllocs$datetime))


for (k in 58:nrow(prox.sum)){
  rowI<-prox.sum[k,]
  i<-rowI$interactionNum
  albID=rowI$birdID
  mmsiB=rowI$mmsi
  eventID=rowI$eventID
  dateB=date(rowI$tmin_30)
  data = alllocs%>%filter(ID==albID)%>%filter(date==dateB)
  species<-data$species[1]
  colony<-data$colony[1]
  
  bird<-alllocs%>%filter(ID==albID)%>%filter(date==dateB)
  boat<-boats%>%filter(mmsi==mmsiB)%>%filter(date==dateB)%>%
    mutate(ID=as.character(mmsiB),species="boat",datetime=datetime_GMT)
  if(nrow(boat)<4) next
  if(nrow(bird)<4) next
  birdboat<-bind_rows(bird,boat)
  
  hrs<-max(birdboat$datetime)-min(birdboat$datetime)
  hrs<-as.numeric(hrs)
  n.frames<-round(hrs*6)
  
  birdboat$POSIX <-birdboat$datetime
  birdboat$species<-as.factor(birdboat$species)
  birdboat$lon360<-wrap360(birdboat$lon)
#background <- rworldmap::getMap(resolution = "coarse")

  ml<-unwrap360(min(birdboat$lon360)-.1)
  xl<-unwrap360(max(birdboat$lon360)+.1)
  
  batHR <- getNOAA.bathy((ml), 
                         (xl), 
                         min(birdboat$lat)-.2, 
                         max(birdboat$lat)+.2, 
                         res = 1, keep = FALSE,antimeridian = FALSE); 
  plot(batHR) #for plots
  
  # Make bathy a raster for plotting
  bathy<-marmap::as.raster(batHR)
  #rc <- cut(bathy, breaks= 10)
  #pols <- rasterToPolygons(rc, dissolve=T)
  #plot(pols)
  background <- bathy
  sp::proj4string(background)

  setwd(paste0(userdir,"/Analysis/PLOTS/animations_BoatsnBirds/"))

# library(gganimate) 
# ggplot()+
#   geom_point(data=birdboat,aes(x=lon360,y=lat,group=species,color=species))+
#   labs(title='Day: {format(frame_time, "%b %e")}')+
#   xlab(expression(paste("Longitude (",degree,"W)")))+
#   ylab(expression(paste("Latitude (",degree,"N)")))+
#   theme(legend.position = "none") +
#   transition_time(datetime) +
#   shadow_wake(wake_length = 0.5, alpha = FALSE)+
#   ease_aes('linear')
# anim_save("/Users/rachaelorben/Desktop/WSTC5_photosplots/RLKI_Tracks2.gif", animated_plot, 
#           nframes=360, fps = 10, end_pause = 0, width = 1024, height = 800)

#vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009, ] ## limit attention to 2009

  animate_paths(paths = birdboat, 
              n.frames = n.frames,
              coord = c("lon", "lat"),
              Time.name = "POSIX",
              ID.name = "ID",
              background = background, 
              method="mp4",
              video.name = paste0(species,"_",eventID,"_",albID,"_boat_",mmsiB,"_bathy.mp4"),
              covariate = "species", covariate.colors = c("red","yellow"),
              bg.misc = "raster::scalebar(d = 30, type = 'bar', xy=c(min(birdboat$lon),min(birdboat$lat)),divs = 4,lonlat=TRUE)")
              
}

saveRDS(birdboat,"~/Desktop/BirdBoat_dataexample.rda")
birdboat<-readRDS("~/Desktop/BirdBoat_dataexample.rda")


bird<-alllocs%>%filter(ID==18875|ID==18908|ID==189311| ID==18897)%>%filter(date=="2012-04-18" | date=="2012-04-19")
boat<-boats%>%filter(mmsi==440873000)%>%filter(date=="2012-04-18" | date=="2012-04-19")%>%
  mutate(ID=as.character(mmsi),species="boat",datetime=datetime_GMT)
tracks<-bind_rows(bird,boat)

# make ltraj (adehabitat)
tracks_lt<-adehabitatLT::as.ltraj(xy = cbind((tracks$lon),tracks$lat),
                                  date = tracks$datetime,
                                  id = tracks$ID,
                                  burst = tracks$ID,
                                  typeII = T,slsp="remove",infolocs = tracks,
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# resample at 10 minutes
tracks_lt_redis<-adehabitatLT::redisltraj(l = tracks_lt,u = 5*60,type = "time")
tracks1<-adehabitatLT::ld(tracks_lt_redis) %>% 
  mutate(ID=(as.character(id)))
tracks_i<-left_join(tracks1,tracks,by="ID")

library(gganimate)
ggplot()+
  geom_point(data=tracks_i,aes(x=lon,y=lat,group=ID,color=species))+
  labs(title='Day: {format(frame_time, "%b %e")}')+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme(legend.position = "none") +
  transition_time(date.x) +
  shadow_wake(wake_length = 10, alpha = FALSE)+
  ease_aes('linear')
anim_save("/Users/rachaelorben/Desktop/BFALInter.gif", animated_plot,
          nframes=800, fps = 3, end_pause = 60, width = 1024, height = 800)

ggplot()+
  GeomPath(data=birdboat,aes(x=lon,y=lat,group=ID,color=species))+
  labs(title='Day: {format(frame_time, "%b %e")}')+
  xlab(expression(paste("Longitude (",degree,"W)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme(legend.position = "none") +
  transition_time(datetime) +
  shadow_wake(wake_length = 10, alpha = FALSE)+
  ease_aes('linear')
