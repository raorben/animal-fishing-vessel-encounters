rm(list=ls())
library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table)
library(sf)

unwrap360<-function(lon360){
  lon<-ifelse(lon360 > 180, -360 + lon360, lon360)
  return(lon)}

wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

if(Sys.info()[7]=="rachaelorben") userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'
if(Sys.info()[7]=="rachaelorben") GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/" ##RAO

#bring in GFW Functions
source(paste0(userdir,"/Analysis/scripts/Functions_AlbatrossGFW.R"))

# load GPS tracking data --------------------------------------------------
birds<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_2019-05-05.rda"))  

#birds<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction.rda"))  
#birds$env_datetime <- as.POSIXct(birds$env_datetime, origin="1970-01-01")
birds$yearT<-year(birds$tmin_30)
birds$monthb<-month(birds$tmin_30)
birds$dt_name<-paste0(birds$yearT,"-",padz(birds$monthb,2),"-",padz(day(birds$tmin_30),2))
unique(birds$dt_name)
names(birds)

#buffersize_m=c(90000,135000)
birds$lat<-birds$env_lat
birds$lon<-birds$env_lon
birds$lon360<-wrap360(birds$env_lon)
birds$gridcount<-NULL

#makes an empty grid from which to find out how many grid cells are encompassed by the buffer
NPGrid<-NULL
lat<-seq(from = 0, to = 70,by=0.1)
lon<-seq(from = 120, to = 265, by=0.1)
NPGrid<-expand.grid(lat,lon) 
colnames(NPGrid)<-c("lat","lon")
NPGrid$lon<-unwrap360(NPGrid$lon)
NPGrid$YN<-1
NPGrid.sf <- st_as_sf(NPGrid,coords=c('lon','lat'),remove = F,crs = 4326)%>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))



BIRDS<-NULL
for (yr in 2012:2016){
  print(yr)
  birdssubyr<-birds%>%dplyr::filter(yearT==yr)
  f<-unique(birdssubyr$dt_name)
  
  Files<-list.files(paste0(GFWDataDir,"/fishing_effort_byvessel/daily_csvs/",yr,"/"),pattern = ".csv",full.names = F,recursive = F)

  
  for (i in 1:length(f)){
    Fi<-f[i]
    print(Fi)    
    start.time <- Sys.time()

    Data<-fread(file=paste0(GFWDataDir,"fishing_effort_byvessel/daily_csvs/",yr,"/",Fi,".csv"),stringsAsFactors=FALSE) #35seconds!!!
    Data$lat=(Data$lat_bin)*.10
    Data$lon=(Data$lon_bin)*.10
    head(Data)
    Fpoints <- st_as_sf(Data,coords=c('lon','lat'),remove = F,crs = 4326)%>%
      st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
    #glimpse(Fpoints)
    
    birdy<-birdssubyr%>%dplyr::filter(dt_name==Fi)
    crS<-paste0("+proj=aeqd +lat_0=",birdy$lat," +lon_0=",birdy$lon," +x_0=0 +y_0=0")
    
    for (k in 1:nrow(birdy)){
    bird<-birdy[k,]
    point <- st_as_sf(bird,coords=c('lon','lat'),remove = F,crs = 4326)%>%
       st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
    point_trn<-st_transform(point,crs = crS[k])
    Fpoints_trn<-st_transform(Fpoints,crs=crS[k])
    NPGrid_trn<-st_transform(NPGrid.sf,crs=crS[k])
    
    buf <- st_buffer(point_trn, dist = 60000)%>% #changed to 60km
      st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
    
    #Fpoints_trn_wi<-Fpoints_trn[sf::st_within(Fpoints_trn,buf)%>% lengths > 0,]

    a<-sf::st_contains(buf, Fpoints_trn)
    Data1 <- Data[unlist(a)]
    
    b<-sf::st_contains(buf, NPGrid_trn)
    Data2 <- NPGrid[unlist(b),]
    ct<-nrow(Data2)
    
    #plot(st_geometry(buf),col = 'green',axes = TRUE)
    #plot(st_geometry(Fpoints_trn),col = 'red',axes = TRUE,add=TRUE)
    #plot(st_geometry(point_trn),add=TRUE)
    #plot(st_geometry(buf),col = 'green',add=TRUE)
    #plot(st_geometry(Fpoints_trn[unlist(a),]),col = 'blue',add=TRUE)
    
    #find info for bird-boat event
    birdy$mmsiYN[k]<-birdy$mmsi[k] %in% unique(Data1$mmsi)
    birdy$num_mmsi[k]<-length(unique(Data1$mmsi)) 
    birdy$fishinghrs_60km[k]<-sum(Data1$fishing_hours)
    birdy$gridcount[k]<-ct
  }
  
  BIRDS<-rbind(BIRDS,birdy)    
    }
  }

D<-NULL
for (j in 1:nrow(Data1)){
d<-argosfilter::distance(61.628, Data1$lat[j], 178.3683, Data$lon[j])
D<-rbind(D,d)
}
D

hm<-BIRDS%>%dplyr::filter(mmsiYN=="FALSE")

BIRDS$fishingden_60km<-BIRDS$fishinghrs_60km/BIRDS$gridcount

dt<-Sys.Date()
saveRDS(BIRDS,paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_lowresDen_60km",dt,".rda"))  
BIRDS<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_lowresDen_60km2019-09-27.rda"))  

names(BIRDS)
ggplot()+
  geom_point(data=BIRDS,aes(x=fishinghrs_60km,y=fishingden_60km))

ggplot()+
  geom_histogram(data=BIRDS,aes(x=num_mmsi))
