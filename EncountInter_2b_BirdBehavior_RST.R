# clear workspace and set working directory
rm(list = ls())
dirRST="/Users/rachaelorben/Dropbox/Research/RST/"

# load C function, secondary functions, plotting functions
dyn.load(paste0(dirRST,"RST_residenceRadii.so")) # if using a mac
#dyn.load("RST_residenceRadii.dll") # if using Windows
source(paste0(dirRST,"RST_functions_all.R"))

# load libraries
library(ggplot2) # for plotting with ggplot
library(mapproj) # for calculating map projections
library(readxl)
library(dplyr)
library(lubridate)

if(Sys.info()[7]=="rachaelorben") userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'
alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr.rds"))

#all encounters, but some don't include locations within 30km
prox.sum<-readRDS(paste0(userdir,"/Analysis/compileddata/proxsummary.rda"))
prox.sumsel<-prox.sum%>%dplyr::filter(time30_min>0)
prox.sumsel$date_min<-date(prox.sumsel$tmin_30)
prox.sumsel$date_max<-date(prox.sumsel$tmax_30)
prox.sumsel$uniID<-1:nrow(prox.sumsel)
#saveRDS(prox.sumsel,file=paste0(userdir,"/Analysis/compileddata/proxsummary_uniIDforRST.rda"))

events<-unique(prox.sumsel$eventID)

dataset_long<-NULL
for (k in 1: length(events)){
  Dset<-events[k]
  info<-prox.sumsel%>%filter(eventID==Dset)
  
  if (info$species=="STAL"){
  dataset_t<-alllocs%>%filter(ID==info$birdID) %>%
    filter(datetime>(info$date_min-7))%>%
    filter(datetime<(info$date_max+7))}
  if (info$species!="STAL"){
    dataset_t<-alllocs%>%filter(ID==info$birdID) %>%
      filter(datetime>(info$date_min-2))%>%
      filter(datetime<(info$date_max+2))}
  dataset_t$event.id<-info$eventID
  dataset_t$uniID<-info$uniID
  dataset_long<-rbind(dataset_long,dataset_t)
}
  
  dataset <- data.frame("band" = dataset_long$uniID,
                        "lat" = dataset_long$lat,
                        "lon" = dataset_long$lon360,
                        "datetime" = dataset_long$datetime)
  str(dataset)
  

  dataset$band<-as.numeric(dataset$band)
  dataset_long$bandnumeric<-as.numeric(dataset$band)
  # remove NA lon/lat rows
  dataset <- dataset[!is.na(dataset$lon) & !is.na(dataset$lat), ]
  
  
  # create grid x- and y- coordinates from the longitude/latitude coordinates
  library(mapproj)
  lambert <- mapproject(dataset$lon, dataset$lat, projection = "lambert", parameters = c(mean(dataset$lon), mean(dataset$lat)))
  scale <- haversineDist(min(dataset$lon), min(dataset$lat), max(dataset$lon), max(dataset$lat)) / projectDist(min(lambert$x), min(lambert$y), max(lambert$x), max(lambert$y), 1)
  dataset$x <- lambert$x * scale
  dataset$y <- lambert$y * scale
  plot(dataset$x,dataset$y,'l')
  
  saveRDS(dataset_long,paste0(userdir,"/Analysis/compileddata/RSTprep_alltracks.rds"))  
  dataset_long<-readRDS(paste0(userdir,"/Analysis/compileddata/RSTprep_alltracks.rds"))  
  
  R<- dataset_long%>%filter(uniID==1)
  
  # CALCULATING RESIDENCE VALUES --------------------------------------------
  time_units = "mins" # one of "secs", "mins", "hours", "days", "weeks"
  
  radius = c(.1,seq(1,10,by=1),seq(11,25,by=2),seq(26,100,by=3),seq(105,150,by=5))
  threshold <- rep(0, length(radius))
  
  
  bandIDs <- unique(dataset$band)

  all_tracks = data.frame()
  
  for (i in 1:length(bandIDs)) {
    subdata = dataset[dataset$band == bandIDs[i], ]
    
    subdata$time_diff = as.numeric(subdata$datetime - subdata$datetime[1], units = time_units)
    
    result <- residenceCalc(subdata$x, subdata$y, subdata$time_diff, radius, threshold)
    subdata = cbind(subdata, result)
    all_tracks = rbind(all_tracks, subdata)
  }
  
  
  # Save results of all scales
  MoveResid<-list(all_tracks, radius, threshold, time_units, bandIDs)
  saveRDS(MoveResid,file = (paste0(userdir,"/Analysis/compileddata/RSToutput/allscales.rda")))
  MoveResid<-readRDS(file = (paste0(userdir,"/Analysis/compileddata/RSToutput/allscales.rda")))
  all_tracks<-MoveResid[[1]]
  radius<-MoveResid[[2]]
  threshold<-MoveResid[[3]]
  time_units<-MoveResid[[4]]
  bandIDs<-MoveResid[[5]]
  
  
  library(dplyr)
  tracks<-all_tracks
  #for multiple tracks
  pdf("/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch/Analysis/PLOTS/RST/MultiScale_missing.pdf")
  scalesum<-plotMultiScale(all_tracks, radius) 
  dev.off()
  
  # PLOTTING SCALE OF RESIDUALS & CHOOSING BEST ONE -------------------------
  #if you don't choose radi large enough for the % transit points to be <0.05 then chooseDynScale will return an error
  dynscale<-chooseDynScale(scalesum, radius) #animal/track ids must be numeric
  #write.csv(x = dynscale,file = paste0(dir,"DynScaleResults.csv"))
  detach("package:dplyr", unload=TRUE)
  
  
  # Average species plot ----------------------------------------------------
  sdS<-apply(scalesum, 2, sd, na.rm=TRUE)
  uS<-apply(scalesum, 2, mean, na.rm=TRUE)
  Psd<-sdS[2:(length(radius)+1)]
  Pu<-uS[2:(length(radius)+1)]
  Nsd<-sdS[(length(radius)+2):((length(radius)*2)+1)]
  Nu<-uS[(length(radius)+2):((length(radius)*2)+1)]
  Tsd<-sdS[((length(radius)*2)+2):((length(radius)*3)+1)]
  Tu<-uS[((length(radius)*2)+2):((length(radius)*3)+1)]
  grade<-data.frame(cbind(radius,Pu,Psd,Nu,Nsd,Tu,Tsd))
  
  library(dplyr)
  
  A<-ggplot(grade, aes(radius)) + 
    geom_line(aes(y=Pu), colour="blue") + geom_point(aes(y=Pu), colour="blue")+
    geom_ribbon(aes(ymin=(Pu-Psd), ymax=(Pu+Psd)), alpha=0.1,fill="blue") +
    geom_line(aes(y=Nu), colour="red") + geom_point(aes(y=Nu), colour="red")+
    geom_ribbon(aes(ymin=(Nu-Nsd), ymax=(Nu+Nsd)), alpha=0.1,fill="red") +
    geom_line(aes(y=Tu), colour="black") + geom_point(aes(y=Tu), colour="black")+ 
    geom_ribbon(aes(ymin=(Tu-Tsd), ymax=(Tu+Tsd)), alpha=0.1) +
    geom_vline(xintercept =(mean(dynscale[,3])), size=2, alpha=.5) + 
    xlab("radius")+
    ylab("proportion")+ggtitle(Dset)+
    coord_cartesian(ylim = c(0, 1), xlim =c(0,20))+ 
    theme_classic(base_size = 12, base_family = "")
  ggsave(A, file=paste0(dir,'/RSToutput/ScalePlot_',Dset,'.png'), width = 4,height=4,units = "in")
  
  
  
  # CALCULATING DYNAMICALLY SCALED RESIDENCE VALUES -------------------------
  all_tracksD = data.frame()
  
 # 18, 19, 37, 39, 40, 57, 74, 86, 93, 108, 139, 150, 151,184, 196, 208
  # for each track, calculate residence values

  for (i in 1:length(bandIDs)) {
    subdata = dataset[dataset$band == bandIDs[i], ]
    print(bandIDs[i])
    
    #dynamically choose radius scale using ouput from plotMultiScale & DynScale
    radius = as.numeric(dynscale[dynscale[,1]== bandIDs[i],3])
    threshold = 0 ##MODIFY (optional)
    
    # manipulate time to measure time passed from beginning of trip, in minutes (for residence computation)
    #ref_time <- strptime(subdata$datetime[1], "%Y-%m-%d %H:%M:%S", tz="GMT")
    #subdata_time <- strptime(subdata$datetime, "%Y-%m-%d %H:%M:%S", tz="GMT")
    #subdata$time_diff <- as.numeric(subdata_time - ref_time, units = time_units)
    
    ref_time <- subdata$datetime[1]
    subdata_time <- subdata$datetime
    subdata$time_diff <- as.numeric(subdata_time - ref_time, units = time_units)
    
    subdata$dist_diff = numeric(nrow(subdata))
    for (j in 2:nrow(subdata)) {
      subdata$dist_diff[j] = subdata$dist_diff[j - 1] + projectDist(subdata$x[j], subdata$y[j], subdata$x[j - 1], subdata$y[j - 1], 1)
    }
    
    # call residence metric function and obtain residence distance and time	
    #result <- residenceCalc(subdata$x, subdata$y, subdata$time_diff, radius, threshold)
    
    result <- try(residenceCalc(subdata$x, subdata$y, subdata$time_diff, radius, threshold), silent=TRUE)
    if ('try-error' %in% class(result)) next
    colnames(result)<-cbind("rows","RT","RD","nRT","nRD","res") #Use for choosing radius dynamically -rename column names in result so scale independent
    subdata = cbind(subdata, result)
    
    all_tracksD = rbind(all_tracksD, subdata)
  }
  
  missing<-setdiff(bandIDs,unique(all_tracksD$band))
  # Dynamic Scaling for Radius, save results
  MoveResid_DynScale<-list(all_tracksD, dynscale, threshold, time_units, bandIDs,missing)
  saveRDS(MoveResid_DynScale,file = paste0(userdir,"/Analysis/compileddata/RSToutput/AllTracksInt_DynScale.rda"))
  

for (i in 1:length(missing)){
  Tdata=all_tracks%>%filter(band==missing[i])
  p<-ggplot()+
    geom_path(data=Tdata,aes(x=lon,y=lat))#+
    #geom_point(data=Tdata,aes(x=lon,y=lat,color=species))
  ggsave(p,filename=paste0(userdir,"/Analysis/compileddata/RSToutput/RST_notworking/",missing[i],".png"))
}

# Plots -------------------------------------------------------------------
  MoveResid_DynScale<-readRDS(file = paste0(userdir,"/Analysis/compileddata/RSToutput/AllTracksInt_DynScale.rda"))
  
  for (i in 1:length(bandIDs)) {
  id<-bandIDs[i]
  ra<-as.numeric(dynscale[dynscale[,1]== id,3])
  quartz(width=8, height=4) #use for saving plot - Mac only
  #windows()#for Windows users
  plotTrackResDyn(id, all_tracksD, dynscale, time_units, ps=2)

  if(!dir.exists(file.path(userdir,"/Analysis/PLOTS/RSTplots/"))) dir.create(file.path(userdir,"/Analysis/PLOTS/RSTplots"),recursive = T)
  quartz.save(file=(paste0(userdir,"/Analysis/PLOTS/RSTplots/",id,'_',ra,'.png')), type = "png", device = dev.cur(), dpi = 100)
  dev.off()
  }


# Summarize radii used ----------------------------------------------------
str(MoveResid_DynScale)

  all_tracksD<-MoveResid_DynScale[[1]]
  head(all_tracksD)
  dynscale<-MoveResid_DynScale[[2]]
  head(dynscale)
  dynscale<-as.data.frame(dynscale)
  names(dynscale)
  colnames(dynscale)<-c("id","diameter_km","radius_km")
  bandIDs<-MoveResid_DynScale[[5]]
  head(bandIDs)  
  
  
names(dataset_long)
dl<-dataset_long%>%select(species,bandnumeric)%>%distinct()
dynscale.sp<-left_join(dynscale,dl,by=c("id"="bandnumeric"))
head(dynscale.sp)

dynscale.sp%>%group_by(species)%>%
  summarise(u=mean(radius_km, na.rm = TRUE),
            sd=sd(radius_km, na.rm = TRUE))

dynscale.sp%>%filter(species=="BFAL")%>%
  filter(radius_km>(4.17+2*0.966))
dynscale.sp%>%filter(species=="LAAL")%>%
  filter(radius_km>(3.26+3*0.564))
dynscale.sp%>%filter(species=="STAL")%>%
  filter(radius_km>(13.5+2*5.93))
