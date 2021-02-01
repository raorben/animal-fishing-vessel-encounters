#Compile and error check Albatross GPS data 
#devtools::install_github("dkahle/ggmap")
library(readxl) 
library(stringr)
library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)


library(rgeos)   # for gDistance(...)
library(geosphere)
library(argosfilter)

library(ggmap)
library(maps)
library(mapdata)

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
source("/Users/rachaelorben/Dropbox/Research/RLKIinc/SGRK/Functions/MakeTrip.R")
source("/Users/rachaelorben/Dropbox/Research/RLKIinc/SGRK/Functions/Dist2Colony.R")

# STAL: Read --------------------------------------------------------------
# STALLocs<-read.csv(paste0(userdir,"/GPS_data/STAL/STALfledglings_2014onwards_MTIandSPDfiltered_excluding_deadbirds_and_postfledging_drift_lessthan20kmhr.csv"),fill = T,stringsAsFactors = F,header = T,skipNul = T, sep=",")
# 
# if(Sys.info()[7]=="rachaelorben") HomeDir<-"/Users/rachaelorben/Google Drive/Seabird_Oceanography_Lab/OConnor_pubs/" ##RAO
# stal<-read.table(paste0(HomeDir,'Post_fledging_tracking/data/STALfledglings_all_MTIandSPDfiltered_excluding_deadbirds_and_postfledging_drift_lessthan20kmhr_0.5km_interp_24hr_gaplimit_1hr_timesamp_forR.csv'),
#                  fill = T, stringsAsFactors = F, header = T, skipNul = TRUE, sep=",")
# stal$datetime<-matlab2POS(stal$mday)
# STALLocs<-stal[stal$year>2011,]
# head(STALLocs)                  
# unique(STALLocs$pttid)
# colST<-colnames(STALLocs)
# STALLocs$species<-"STAL"
# STALLocs$colonyID[STALLocs$colonyID==1]<-"Torishima"
# STALLocs$colonyID[STALLocs$colonyID==0]<-"Mukojima"
#   
# data.table::setnames(STALLocs, "colonyID", "colony")
# data.table::setnames(STALLocs, "name", "ID")
# data.table::setnames(STALLocs, "lon180", "lon")
# 
# cols.dont.want <- c("pttid","deploylat", "deploylon","lon360","mday","year",
#                     "month","ptton","pttoff","GPS","VHF","GSM","Other","GPS_schedule","Lat_Deg",
#                     "Lat_Min","Lon_Deg","dist","dist2col","dur","vel",
#                     "azim") 
# 
# STALLocs <- STALLocs[, ! names(STALLocs) %in% cols.dont.want, drop = F]
# STALLocs$TagType<-"MicrowaveTelem"
# STALLocs$datetime<-ymd_hms(STALLocs$datetime,tz="GMT")
# STALLocs$ID<-as.character(STALLocs$ID)
# STALLocs$TripNum<-1
# 
# saveRDS(STALLocs,paste0(userdir,"/GPS_data/STAL2012onward_Alldata.rda"))
STALLocs<-readRDS(paste0(userdir,"/GPS_data/STAL2012onward_Alldata.rda"))

# quartz()
# ggplot(data=STALLocs,aes(x=wrap360(lon),y=lat, group=ID))+
# geom_path(aes(color=as.factor(ID)))+
#   geom_point(aes(color=as.factor(ID)))
# quartz.save(paste0(userdir,'/Analysis/PLOTS_exploritory/STALlocs_2012on.png'))

# BFAL/LAAL 2016 Midway: Read ----------------------------------------------
# deployinfo<-read.csv('/Users/rachaelorben/Google Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/BFAL_LAAL_flightbehavior/ProcessedData/DeployMatrixGPSAltitudeData_Albatrosses_Midway.csv')
# deployinfo$DateTimeDeploy_GMT<-strptime(deployinfo$DateTimeDeploy, format = "%m/%d/%y %H:%M")-(deployinfo$TimeZone*60*60)
# tz(deployinfo$DateTimeDeploy_GMT)<-"GMT"
# deployinfo$DateTimeRetrieve_GMT<-strptime(deployinfo$DateTimeRetrieve, format = "%m/%d/%y %H:%M")-(deployinfo$TimeZone*60*60)
# tz(deployinfo$DateTimeRetrieve_GMT)<-"GMT"
# head(deployinfo)
# str(deployinfo)
# Files16<-list.files(paste0(userdir,"/GPS_data/BFAL_Midway2016_igotu/alt"),pattern = ".csv",full.names = T,recursive = T)
# LOCS<-NULL
# for (j in 1:length(Files16)){
#   Locs<-igotualt_Read(Files16[j])
#   namebird<-Files16[j]
#   a<-sapply(strsplit(Files16[j], split='/', fixed=TRUE), function(x) (x[10]))
#   Colony <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[2]))
#   recapturedate <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[3]))
#   Bird <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[1]))
#   ID <- sapply(strsplit(Bird, split='AL', fixed=TRUE), function(x) (x[2]))
#   info<-deployinfo[deployinfo$ID==as.numeric(ID),]
#   Locs$colony<-Colony
#   Locs$ID<-paste0(1,ID)
#   Locs$DeployYr<-2016
#   Locs$TagType="igotu"
#   Locs$species="BFAL"
#   Locs$ColLat<-info$ColLat
#   Locs$ColLon<-info$ColLon
#   Locs<-Locs[Locs$DateTime>info$DateTimeDeploy_GMT & Locs$DateTime<info$DateTimeRetrieve_GMT,]
#   LOCS<-rbind(LOCS, Locs)
# }
# 
# Files16<-list.files(paste0(userdir,"/GPS_data/LAAL_Midway2016_igotu/alt"),pattern = ".csv",full.names = T,recursive = T)
# for (j in 1:length(Files16)){
#   Locs<-igotualt_Read(Files16[j])
#   namebird<-Files16[j]
#   a<-sapply(strsplit(Files16[j], split='/', fixed=TRUE), function(x) (x[10]))
#   Colony <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[2]))
#   recapturedate <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[3]))
#   Bird <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[1]))
#   ID <- sapply(strsplit(Bird, split='AL', fixed=TRUE), function(x) (x[2]))
#   info<-deployinfo[deployinfo$ID==as.numeric(ID),]
#   Locs$colony<-Colony
#   Locs$ID<-paste0(2,ID)
#   Locs$DeployYr<-2016
#   Locs$TagType="igotu"
#   Locs$species="LAAL"
#   Locs$ColLat<-info$ColLat
#   Locs$ColLon<-info$ColLon
#   Locs<-Locs[Locs$DateTime>info$DateTimeDeploy_GMT & Locs$DateTime<info$DateTimeRetrieve_GMT,]
#   LOCS<-rbind(LOCS, Locs)
# }
# head(LOCS)
# saveRDS(LOCS,paste0(userdir,"/GPS_data/Midway2016_Alldata_trimed.rda"))
# LOCS<-readRDS(paste0(userdir,"/GPS_data/Midway2016_Alldata_trimed.rda"))
# colBL<-colnames(LOCS)
# str(LOCS)
# 
# LOCS<-subset(LOCS,lat!=0)
# a<-LOCS[LOCS$lon>(-125)&LOCS$lon<0,]
# LOCS<-LOCS[LOCS$lon<(-125)|LOCS$lon>0,]
# data.table::setnames(LOCS, "Altitude", "altitude")
# data.table::setnames(LOCS, "DateTime", "datetime")
# data.table::setnames(LOCS, "Index", "oid")
# 
# #birds never left the colony so are removed from dataset
# LOCS<-subset(LOCS,ID!="103")
# LOCS<-subset(LOCS,ID!="209")
# 
# Dist2Col<-NULL
# for(i in 1:length(LOCS$lat)){ 
#   Dist2Col[i]<-distance(lat1 = LOCS$ColLat[i],lat2 = LOCS$lat[i],lon1 = LOCS$ColLon[i], lon2 = LOCS$lon[i])
# }
# LOCS$Dist2Col<-Dist2Col
# 
# LOCSa<-MakeTrip(LOCS,ID="ID",DistCutOff=1.5,Dist2Colony="Dist2Col",NumLocCut=4)
# 
# cols.dont.want <- c("DeployYr","ColLat","ColLon") # if you want to remove multiple columns
# LOCSa <- LOCSa[, ! names(LOCSa) %in% cols.dont.want, drop = F]
# 
# LOCSa<-LOCSa[LOCSa$TripNum>0,]
# saveRDS(LOCSa,paste0(userdir,"/GPS_data/Midway2016_Alldata_trimed_D2Col.rda"))
LOCSa<-readRDS(paste0(userdir,"/GPS_data/Midway2016_Alldata_trimed_D2Col.rda"))

# quartz()
# ggplot(data=LOCSa,aes(x=wrap360(lon),y=lat, group=ID))+
#   geom_path(aes(color=as.factor(TripNum)))+
#   theme(legend.position = "none")+
#   facet_wrap(~ID,scales = "free")
# quartz.save(paste0(userdir,'/Analysis/PLOTS_exploritory/Midway2016locs.png'))

# Midway 2014-2015 --------------------------------------------------------
# MidwayT<-read_excel(paste0(userdir,"/GPS_data/Midway Tracks 2014-2015/all_midway_alb_tracks_1415.xlsx"))
# head(MidwayT)
# 
# data.table::setnames(MidwayT, "latitude", "lat")
# data.table::setnames(MidwayT, "longitude", "lon")
# data.table::setnames(MidwayT, "Datetime", "datetime")
# data.table::setnames(MidwayT, "SortID", "oid")
# data.table::setnames(MidwayT, "Species", "species")
# 
# cols.dont.want <- c("Date", "Day","Month","year","Time","Year") # if you want to remove multiple columns
# MidwayT <- MidwayT[, ! names(MidwayT) %in% cols.dont.want, drop = F]
# a<-MidwayT[MidwayT$ID=="I589",]
# a1<-a[1,]
# a1$lat<-28.210
# a1$lon<-(-177.373)
# a1$datetime<-a1$datetime-30
# MidwayT<-rbind(MidwayT[1:22969,],a1,MidwayT[22970:47690,])
# 
# head(MidwayT)
# MidwayT$colony<-"MIAT"
# MidwayT$TagType<-"igotu"
# MidwayT$species[MidwayT$species=="BFLA"]<-"BFAL"
# colMT<-colnames(MidwayT)
# str(MidwayT)
# tz(MidwayT$datetime)<-"GMT"
# MidwayT$ColLat<-28.210
#   MidwayT$ColLon<-(-177.373)
# 
# Dist2Col<-NULL
# for(i in 1:length(MidwayT$lat)){ 
#     Dist2Col[i]<-distance(lat1 = MidwayT$ColLat[i],lat2 = MidwayT$lat[i],lon1 = MidwayT$ColLon[i], lon2 = MidwayT$lon[i])
# }
# MidwayT$Dist2Col<-Dist2Col
#   
# MidwayTa<-MakeTrip(MidwayT,ID="ID",DistCutOff=1.5,Dist2Colony="Dist2Col",NumLocCut=4)
# MidwayTa<-MidwayTa[MidwayTa$TripNum>0,]
# saveRDS(MidwayTa,paste0(userdir,"/GPS_data/Midway201415_Alldata_trimed_D2Col.rda"))
MidwayTa<-readRDS(paste0(userdir,"/GPS_data/Midway201415_Alldata_trimed_D2Col.rda"))

# quartz()  
# ggplot(data=MidwayTa,aes(x=wrap360(lon),y=lat, group=ID))+
#   geom_path(aes(color=as.factor(TripNum)))+
#   facet_wrap(~ID, scales="free")+
#   theme(legend.position = "none")
# quartz.save(paste0(userdir,'/Analysis/PLOTS_exploritory/Midway2015locs.png'))
# 
# quartz()
# bikemap1 <- get_map(location = c(-177.37,28.21), maptype = "satellite", source = "google", zoom = 13)
# ggmap(bikemap1) + 
#   geom_path(data=MidwayT, aes(color = colony), size = .5)+
#   geom_point(aes(x=-177.373,y=28.210))+
#   geom_point(aes(x=-177.3570,y=28.18842))+
#   facet_wrap(~ID)


# Kauai Tracks -----------------------------------------------------------
#12 Birds (many tracks) at Kaena in 2014, 12 birds on Kauai 2014, and 12 from Kauai 2016 = 36 total birds prob >5 times as many "tracks"
# LAALLocs<-read.csv(paste0(userdir,"/GPS_data/Josh_Adams/LAAL_1.5_trips_annotated.csv"))
#unique(LAALLocs$Deploy_ID)
# head(LAALLocs)
# colnames(LAALLocs)
# 
# data.table::setnames(LAALLocs, "Latitude", "lat")
# data.table::setnames(LAALLocs, "Longitude", "lon")
# data.table::setnames(LAALLocs, "UTC", "datetime")
# data.table::setnames(LAALLocs, "Altitude", "altitude")
# data.table::setnames(LAALLocs, "Deploy_ID", "ID")
# data.table::setnames(LAALLocs, "trip_no", "TripNum")
# 
# LAALLocs$Year<-year(LAALLocs$datetime)
# LAALLocs$TagType<-"eObs"
# LAALLocs$colony<-"Oahu"
# LAALLocs$colony[LAALLocs$Year==2016]<-"Kauai"
# LAALLocs$colony[LAALLocs$ID>1187]<-"Kauai"
# 
# cols.dont.want <- c("oldDeploy_ID")
# LAALLocs <- LAALLocs[, ! names(LAALLocs) %in% cols.dont.want, drop = F]
# str(LAALLocs)
# LAALLocs$ID<-as.character(LAALLocs$ID)
# LAALLocs$datetime<-as.character(LAALLocs$datetime)
# LAALLocs$datetime<-ymd_hms(LAALLocs$datetime)
# tz(LAALLocs$datetime)<-"GMT"
# 
# 
# 
# path.ne.coast <- ("/Users/rachaelorben/Dropbox/Envrion_Vars/ne_10m_coastline")
# fnam.ne.coast <- "ne_10m_coastline.shp"
# dat.coast <- rgdal::readOGR(dsn = path.ne.coast,
#                      layer = tools::file_path_sans_ext(fnam.ne.coast))
# 
# quick.subset <- function(x, domain){
#   x@data$id <- rownames(x@data)
#   x.f = fortify(x, region = "id")
#   x.join <- inner_join(x.f, x@data, by = "id")
#   x.subset <- subset(x.join, x.join$long > domain[1] &
#                        x.join$long < domain[2] &
#                        x.join$lat > domain[3] &
#                        x.join$lat < domain[4])
#   x.subset
# }
# # domain should be a vector of four values: c(xmin, xmax, ymin, ymax)
# 
# # Specify the desired domain
# Kdomain <- c(-160, -159, 20, 23)
# kauai <- quick.subset(dat.coast, Kdomain) # 4871x8
# # Specify the desired domain
# Odomain <- c(-159, -157.5, 21, 22)
# oahu <- quick.subset(dat.coast, Odomain) # 4871x8
# oahuK<-oahu[oahu$long<(-158.25),]
# 
# LAALLocsK<-LAALLocs[LAALLocs$colony=="Kauai",]
# LAALLocsO<-LAALLocs[LAALLocs$colony=="Oahu",]
# 
# ggplot() +
#   geom_path(data = kauai, aes(x = wrap360(long), y = lat, group = group),
#             color = "black", size = 0.25) +
#   geom_path(data = oahu, aes(x = wrap360(long), y = lat, group = group),
#             color = "green", size = 0.25) +
#   geom_path(data = LAALLocsO, aes(x = wrap360(lon), y = lat),
#             color = "blue", size = 0.25) +
#   coord_fixed(ratio=1.7,xlim = c(199.5,202.5),ylim=c(21,22.5))+
#   labs(list(title = "", x = "Longitude", y = "Latitude"))
# 
# ggplot() +
#   geom_path(data = kauai, aes(x = wrap360(long), y = lat, group = group),
#             color = "black", size = 0.25) +
#   geom_path(data = oahu, aes(x = wrap360(long), y = lat, group = group),
#             color = "green", size = 0.25) +
#   geom_path(data = LAALLocsK, aes(x = wrap360(lon), y = lat),
#             color = "blue", size = 0.25) +
#   coord_fixed(ratio=1.7,xlim = c(199.5,202.5),ylim=c(21,22.5))+
#   labs(list(title = "", x = "Longitude", y = "Latitude"))
# 
# 
# Dist2Coast<-NULL
# for (i in 1:nrow(LAALLocsO)){
#   DIST<-NULL
#   for (k in 1:nrow(oahuK)){
#   Dist<-distance(lat1 = oahuK$lat[k],lat2 = LAALLocsO$lat[i],lon1 = oahuK$lon[k], lon2 = LAALLocsO$lon[i])
#   DIST<-rbind(DIST,Dist)
#   }
#   D<-min(DIST)
#   Dist2Coast<-rbind(Dist2Coast,D)
# }
# LAALLocsO$Dist2Coast<-Dist2Coast
# 
# 
# Dist2Coast<-NULL
# for (i in 1:nrow(LAALLocsK)){
#   DIST<-NULL
#   for (k in 1:nrow(kauai)){
#     Dist<-distance(lat1 = kauai$lat[k],lat2 = LAALLocsK$lat[i],lon1 = kauai$lon[k], lon2 = LAALLocsK$lon[i])
#     DIST<-rbind(DIST,Dist)
#   }
#   D<-min(DIST)
#   Dist2Coast<-rbind(Dist2Coast,D)
# }
# LAALLocsK$Dist2Coast<-Dist2Coast
# LAALLocsA<-rbind(LAALLocsK,LAALLocsO)
# 
# DUPES<-data.frame()
# ID<-unique(LAALLocsA$ID)
# LAALLocsAA<-data.frame()
# for (i in ID){
#   #print(i)
#   alllocsb<-LAALLocsA[LAALLocsA$ID==i,]
#   #print(nrow(alllocsb))
#   dupes<-which(duplicated(alllocsb$datetime))
#   alllocsbb<-alllocsb[-(dupes), ]
#   if (length(dupes)==0){alllocsbb<-alllocsb}
#   #print(nrow(alllocsbb))
#   LAALLocsAA<-rbind(LAALLocsAA,alllocsbb)
#   DUPES<-c(DUPES,t(dupes))
# }
# 
# 
# data.table::setnames(LAALLocsA, "TripNum","trip_no")
# LAALLocsa<-MakeTrip(LAALLocsA,ID="ID", DistCutOff=2,Dist2Colony="Dist2Coast",NumLocCut=3)
# LAALLocsa<-LAALLocsa%>%filter(TripNum!=0)
# # saveRDS(LAALLocsa,paste0(userdir,"/GPS_data/LAAL_Adams_Dist2Coast.rda"))
# LAALLocsa<-readRDS(paste0(userdir,"/GPS_data/LAAL_Adams_Dist2Coast.rda"))
# unique(LAALLocsa$colony)
# colnames(LAALLocsa)
# 
# ggplot() +
#   geom_path(data = kauai, aes(x = wrap360(long), y = lat, group = group),
#             color = "black", size = 0.25) +
#   geom_path(data = oahu, aes(x = wrap360(long), y = lat, group = group),
#             color = "green", size = 0.25) +
#   geom_path(data = LAALLocsa%>%filter(colony=="Oahu")%>%filter(TripNum==0), 
#             aes(x = wrap360(lon), y = lat,group=TripNum,color=as.factor(TripNum)),
#             size = 0.25) +
#   coord_fixed(ratio=1.7,xlim = c(199.5,202.5),ylim=c(21,22.5))+
#   labs(list(title = "", x = "Longitude", y = "Latitude"))+facet_wrap(~ID)
# data = LAALLocsa%>%filter(colony=="Oahu")%>%filter(TripNum==0)
# # quartz()
# # bikemap1 <- get_map(location = c(-159.35,22.2), maptype = "satellite", source = "google", zoom = 13)
# # ggmap(bikemap1) + 
# # geom_path(data=LAALLocsa, aes(color = ID), size = .2)
# # 
# # quartz()
# # bikemap1 <- get_map(location = c(-158.3,21.58), maptype = "satellite", source = "google", zoom = 11)
# # ggmap(bikemap1) + 
# #   geom_point(data=LAALLocsO[LAALLocsO$Dist2Coast<10,], aes(color = Dist2Coast), size = .2)+
# #   geom_point(data = oahu, aes(x = long, y = lat, group = group),color = "white", size = 0.25)+
# #   geom_point(data = oahuK, aes(x = long, y = lat, group = group),color = "red", size = 0.25)
# # 
# # quartz()
# # bikemap2 <- get_map(location = c(-159,42), maptype = "satellite", source = "google", zoom = 4)
# # ggmap(bikemap2) + 
# #   geom_path(data=LAALLocsa, aes(color = as.factor(TripNum)), size = .3)+facet_wrap(~ID)
# # 
# # quartz()
# # bikemap1 <- get_map(location = c(-159.35,22.226), maptype = "satellite", source = "google", zoom = 13)
# # ggmap(bikemap1) + 
# #   geom_path(data=LAALLocs[LAALLocs$ID=="1211",], aes(color = as.factor(TripNum)), size = .5)+
# #   facet_wrap(~TripNum)+theme(legend.position="none")
# # 
# # 
# # quartz.save(paste0(userdir,'/Analysis/PLOTS_exploritory/KauaiOahulocs_bytrip.png'))
# # 
# quartz()
# ggplot(data=LAALLocsa,aes(x=wrap360(lon),y=lat, group=ID))+
#    geom_point(aes(color=(Dist2Coast)))+
#    facet_wrap(~ID,scales = "free")
#  #quartz.save(paste0(userdir,'/Analysis/PLOTS_exploritory/KauaiOahulocs.png'))
# 
# quartz()
# ggplot(data=LAALLocsa,aes(x=wrap360(lon),y=lat, group=ID))+
#   geom_path(aes(color=as.factor(TripNum)))+
#   facet_wrap(~ID)
# 

# Tern 2012 ---------------------------------------------------------------
# Files12<-list.files(paste0(userdir,"/GPS_data/Tern_GPS_2012"),pattern = ".csv",full.names = T,recursive = T)
# 
# LOCS<-NULL
# for (j in 1:length(Files12)){
#   Locs<-read.csv(Files12[j])
#   namebird<-Files12[j]
#   a<-sapply(strsplit(Files12[j], split='/', fixed=TRUE), function(x) (x[9]))
#   Colony <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[2]))
#   recapturedate <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[5]))
#   ID <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[4]))
#   species <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[1]))
#   #info<-deployinfo[deployinfo$ID==as.numeric(ID),]
#   Locs$lc<-3
#   Locs$datetime<-ymd_hms(paste(Locs$Date,Locs$Time),tz="GMT")
#   Locs$filt<-argosfilter::sdafilter(Locs$Latitude, 
#                                          Locs$Longitude, 
#                                          Locs$datetime,  
#                                          Locs$lc,  
#                                          vmax = 45, 
#                                          ang = c(15, 25), #defaults
#                                          distlim = c(5000, 10000)) #2x defaults since WEGU can fly
#   Locs$colony<-Colony
#   Locs$IDa<-paste0(1,ID)
#   Locs$ID<-j
#   Locs$DeployYr<-2012
#   Locs$TagType="igotu"
#   Locs$species=species
#   Locs$ColLat<-23.870
#   Locs$ColLon<-(-166.284)
#   LOCS<-rbind(LOCS, Locs)
# }
# #LOCS<-LOCS[,3:19]
# data.table::setnames(LOCS, "Longitude", "lon")
# data.table::setnames(LOCS, "Latitude", "lat")
# data.table::setnames(LOCS, "Altitude", "altitude")
# 
# Dist2Col<-NULL
# for(i in 1:length(LOCS$lat)){
#   Dist2Col[i]<-argosfilter::distance(lat1 = LOCS$ColLat[i],lat2 =LOCS$lat[i],lon1 = LOCS$ColLon[i], lon2 = LOCS$lon[i])
# }
# LOCS$Dist2Col<-Dist2Col
# 
# LOCSTa<-MakeTrip(LOCS,ID="ID",DistCutOff=1.5,Dist2Colony="Dist2Col",NumLocCut=4)
# LOCSTa<-LOCSTa[LOCSTa$TripNum>0,]
# LOCSTa$ID<-as.character(LOCSTa$ID)
# 
# unique(LOCSTa$species)
# quartz()
# ggplot()+
#    geom_path(data=LOCSTa[LOCSTa$filt!="removed",],aes(lon,lat,color=as.factor(TripNum)))+
# geom_point(data=LOCSTa[LOCSTa$filt=="removed",],aes(lon,lat),color="black")+facet_wrap(~ID, scales="free")
# # 
# 
# LOCSTa<-LOCSTa[LOCSTa$filt!="removed",]
# saveRDS(LOCSTa,paste0(userdir,"/GPS_data/TERN_Shaffer_Dist2Coast.rda"))
LOCSTa<-readRDS(paste0(userdir,"/GPS_data/TERN_Shaffer_Dist2Coast.rda"))

# Kure BFAL Oikonos -------------------------------------------------------
# Files_bf<-list.files(paste0(userdir,"/GPS_data/Oikonos BFAL data_Torres-Orben Collaboration"),pattern = "SPD.csv",full.names = T,recursive = T)
# 
# LOCS<-NULL
# for (j in 1:length(Files_bf)){
#   Locs<-read.csv(Files_bf[j])
#   namebird<-Files_bf[j]
#   a<-sapply(strsplit(Files_bf[j], split='/', fixed=TRUE), function(x) (x[10]))
#   Colony <- "KURE"
#   #recapturedate <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[5]))
#   ID <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[2]))
#   x <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[3]))
#   ID<-paste0(ID,x)
#   species <- sapply(strsplit(a, split='_', fixed=TRUE), function(x) (x[1]))
#   #info<-deployinfo[deployinfo$ID==as.numeric(ID),]
#   Locs$colony<-Colony
#   Locs$ID<-ID
#   Locs$DeployYr<-min(year(Locs$UTC))
#   Locs$TagType<-"eObs"
#   Locs$species<-species
#   Locs$ColLat<- 28.392533
#   Locs$ColLon<-(-178.293552)
#   Locs$datetime1<-strptime(as.character(Locs$UTC), format = "%Y-%m-%d %H:%M:%S", tz="GMT")
#   LOCS<-rbind(LOCS, Locs)
# }
# colnames(LOCS)
# data.table::setnames(LOCS, "longitude", "lon")
# data.table::setnames(LOCS, "latitude", "lat")
# data.table::setnames(LOCS, "height.above.ellipsoid", "altitude")
# str(LOCS)
# 
# str(Locs$UTC)
# 
# LOCS$lon360<-wrap360(LOCS$lon)
# Dist2Col<-NULL
# for(i in 1:length(LOCS$lat)){
#   Dist2Col[i]<-argosfilter::distance(lat1 = LOCS$ColLat[i],lat2 =LOCS$lat[i],lon1 = LOCS$ColLon[i], lon2 = LOCS$lon[i])
# }
# LOCS$Dist2Col<-Dist2Col
# LOCS$datetime1<-as.POSIXct(LOCS$datetime1)
# LOCS$datetime<-as.POSIXct(LOCS$datetime1)
# summary(LOCS)
# str(LOCS)
# 
# LOCS_kure<-MakeTrip(LOCS,ID="ID",DistCutOff=1.5,Dist2Colony="Dist2Col",NumLocCut=4)
# 
# LOCS_kure<-LOCS_kure[LOCS_kure$TripNum>0,]
# LOCS_kure$ID<-as.character(LOCS_kure$ID)
# 
# quartz()
# ggplot()+
#   geom_path(data=LOCS_kure,aes(lon360,lat,group=ID,color=as.factor(ID)))+facet_wrap(~ID)
# 
# saveRDS(LOCS_kure,paste0(userdir,"/GPS_data/Kure_Oikonos_Dist2Coast.rda"))
LOCS_kure<-readRDS(paste0(userdir,"/GPS_data/Kure_Oikonos_Dist2Coast.rda"))

# Join data ---------------------------------------------------------------
locs1<-bind_rows(STALLocs,LOCSa)
locs2<-bind_rows(locs1,MidwayTa)
locs3<-bind_rows(locs2,LAALLocsa)
locs4<-bind_rows(locs3,LOCSTa)
locs5<-bind_rows(locs4,LOCS_kure)

#reorder columns to make more sense:
alllocs<-locs5[c("oid","ID","species","colony","datetime","lat","lon",
        "TagType", "altitude","Speed","Course","Distance","SatNum",
        "flightyr","flight.oid","day.count","deploy.count",
        "tripSt","tripStComp","tripEnd","tripEndComp","TripNum","duration.hrs",
        "Dist2Col","Dist2Coast")]
saveRDS(alllocs,paste0(userdir,"/Analysis/compileddata/AlbatrossData_bytrip.rda"))
alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/AlbatrossData_bytrip.rda"))

w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("USA","Russia","China","Japan","Mexico","Canada","North Korea","South Korea","Taiwan","Mongolia"),]

quartz()
ggplot()+
  geom_polygon(data=w2hr_sub,
               aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  geom_path(data=alllocs,aes(wrap360(lon),lat,group=ID,color=as.factor(ID)))+
  theme(legend.position = "none")
ggplot()+
  geom_path(data=alllocs%>%filter(species!="STAL"),aes(wrap360(lon),lat,group=ID,color=as.factor(ID)))+
  theme(legend.position = "none")
ggplot()+
  geom_polygon(data=w2hr_sub,
               aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  geom_path(data=alllocs%>%filter(species!="STAL"),
            aes(wrap360(lon),lat,group=ID,color=as.factor(colony)))+
  theme(legend.position = "none")+facet_wrap(~colony)+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))

quartz()
ggplot()+
  geom_path(data=alllocs%>%filter(colony=="Oahu" |colony=="Kauai"),aes(wrap360(lon),lat,group=ID,color=as.factor(colony)))

# Track Stats -------------------------------------------------------------
#time difference between locations, by group
alllocs$month<-month(alllocs$datetime)
alllocs <- alllocs %>% group_by(species,ID) %>% arrange(datetime) %>%
  mutate(tdiffm = as.numeric(datetime-dplyr::lag(datetime), units = 'mins'))

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

Bsum<-alllocs %>% group_by(species,ID,colony) %>% summarise(starttime=min(datetime),
                                          endtime=max(datetime),
                                          tripnum=max(TripNum),
                                          locnum=n_distinct(lat),
                                          tdiff_median=specify_decimal(median(tdiffm,na.rm=TRUE),1),
                                          tdiff_mean=specify_decimal(mean(tdiffm,na.rm=TRUE),2),
                                          tdiff_sd=specify_decimal(sd(tdiffm,na.rm=TRUE),2),
                                          Months_no = n_distinct(month))
Bsum$duration<-specify_decimal(difftime(Bsum$endtime,Bsum$starttime, units = "days"),2)
write.csv(Bsum,paste0(userdir,"/Analysis/BirdSummary.csv"))

Bsum$dur<-as.numeric(gsub(x = Bsum$duration,pattern = " days",replacement = ""))

Bsum$year<-year(Bsum$starttime)
for (i in 1:nrow(Bsum)){
if (Bsum$year[i]==2011){Bsum$starttime[i]<-"2012-01-01 00:00:00"}
  if (Bsum$year[i]==2011){Bsum$duration[i]<-specify_decimal(difftime(Bsum$endtime[i],Bsum$starttime[i], units = "days"),2)}
  if (Bsum$year[i]==2011){Bsum$year[i]<-2012}
}

BsumC<-Bsum %>% group_by(species,year,colony) %>% summarise(dur_mean=mean(dur,na.rm=TRUE),
                                                      dur_sd=sd(dur,na.rm=TRUE),
                                                      n=n(),
                                                      stdte=min(starttime),
                                                      enddte=max(endtime),
                                                      mtdiff=max(tdiff_median))
write.csv(BsumC,paste0(userdir,"/Analysis/ColonySummary_year.csv"))

Tsum<-alllocs %>% group_by(species,ID,colony,TripNum) %>% summarise(starttime=min(datetime, na.rm=TRUE),
                                                            endtime=max(datetime, na.rm=TRUE),
                                                            locnum=n_distinct(lat),
                                                            Months_no = n_distinct(month))
Tsum$duration<-specify_decimal(difftime(Tsum$endtime,Tsum$starttime, units = "days"),2)
write.csv(Tsum,paste0(userdir,"/Analysis/BirdTripSummary.csv"))

Dayssum<-alllocs %>% 
  group_by(species,ID,colony,day=round_date(datetime,unit = "day")) %>% 
  summarise(JanD=sum(month == 1), FebD=sum(month == 2),MarD=sum(month == 3),
AprD=sum(month == 4),MayD=sum(month == 5),JunD=sum(month == 6),JulD=sum(month == 7),
AugD=sum(month == 8))

Dayssum<-alllocs %>% 
  group_by(species,ID,colony,month,day=round_date(datetime,unit = "day"))%>% 
  tally()

Dayssum$month<-as.integer(Dayssum$month)
a<-ggplot(Dayssum, aes(month)) +
  geom_histogram(bins=12,binwidth = 1)+
  theme_classic()+scale_x_continuous(breaks=c(2,4,6,8,10,12))+
  ylab("Number of Days")+
  facet_wrap(~species)

Birdsum<-Dayssum %>% group_by(species,ID,month)%>% tally()
b<-ggplot(Birdsum, aes(month)) +
  geom_histogram(bins=12,binwidth = 1)+
  theme_classic()+scale_x_continuous(breaks=c(2,4,6,8,10,12))+
  ylab("Number of Birds")+
  facet_wrap(~species)

library(gridExtra) 
quartz()
plotall<-grid.arrange(a,b)
ggsave(plot = plotall,paste0(userdir,"/Analysis/PLOTS/GPS_datacoverage_2012to2016.jpeg"))



alllocs%>%group_by(species)%>%summarise(n=n(),nindividual=n_distinct(ID))
Bsum%>%group_by(species)%>%
  summarise(meanDur=(mean(duration(duration,units="days")))/86400,
            stDDur=(sd(duration(duration,units="days")))/86400,
            sumDur=sum(duration(duration,units="days"))/86400)

# PLOT all data -----------------------------------------------------------
#Landmask in Adehabitat
library(sp)
library(rgdal)
library(raster)
library(adehabitatHR)
library(SDMTools)
library(maptools)
library(marmap)
library(geoR)
library(adehabitatMA)


# 
# aleu <- getNOAA.bathy(118, -110, 15, 67, resolution = 4,antimeridian = TRUE) #resolution 1.85km*27=49.9km2
# 
# # Make bathy a raster for plotting
# bathy<-as.raster(aleu)
# # change the projection from 0-360 to -180 - 180
# 
# # There is an anoying line at 180°, use aggragate to downsample the raster to remove line
# bathy1<-aggregate(bathy,fun="mean",fact=2)
# 
# # extract the coordinates
# bathy1$V1<-coordinates(bathy1)[,1]
# bathy1$V2<-coordinates(bathy1)[,2]# get world2 data into a format for ggplot (world2 is lon 0-360)
# 
# # make dataframe for ggploting
# bathy2<-data.frame(V1=bathy1$V1@data@values,V2=bathy1$V2@data@values,Depth=bathy1$layer@data@values)
# 
# # subset to make it saller for faster plotting
# bathy2<-bathy2[complete.cases(bathy2),]
# #bathy2<-filter(bathy2,Depth<100)
# bathy2$Depth[bathy2$Depth>0]<-0
# saveRDS(bathy2,paste0(userdir,"/Analysis/compileddata/Bathymetryforggplot.rda"))
library(viridis)
pal1 <- viridis_pal(option = "magma")(5)

bathy2<-readRDS(paste0(userdir,"/Analysis/compileddata/Bathymetryforggplot.rda"))

w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("USA","Russia","China","Japan","Mexico","Canada","North Korea","South Korea","Taiwan","Mongolia"),]

quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("blue", "steelblue2","lightblue"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  geom_path(data = alllocs[alllocs$species=="STAL",], aes(x = wrap360(lon), y = lat, group = ID),  color=pal1[2], size = 0.1) +
  geom_path(data = alllocs[alllocs$species=="LAAL",], aes(x = wrap360(lon), y = lat, group = ID), color=pal1[3], size = 0.1) +
  geom_path(data = alllocs[alllocs$species=="BFAL",], aes(x = wrap360(lon), y = lat, group = (ID)), color=pal1[5], size = 0.1) +
  geom_point(aes(x=130,y=64),colour=pal1[2], size = 4)+
  geom_point(aes(x=130,y=62),colour=pal1[3], size = 4)+
  geom_point(aes(x=130,y=60),colour=pal1[5], size = 4)+
  annotate("text",x=wrap360(133),y=64,label="STAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(133),y=62,label="LAAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(133),y=60,label="BFAL",color="white",size=3,hjust = 0)+
  geom_point(aes(x=142.1243,y=27.6835),colour="darkblue",shape=17,size=1,show.legend=T)+#Torishima
  geom_point(aes(x=140.3073,y=30.47617),colour="darkblue",shape=17,size=1,show.legend=T)+#Mukukima
  geom_point(aes(x=(wrap360(-178.293552)),y=28.392533),colour="darkblue",shape=17,size=1,show.legend=T)+#Kure
  geom_point(aes(x=wrap360(-159.518980),y=22.207736),colour="darkblue",shape=17,size=1,show.legend=T)+#Kauai
  geom_point(aes(x=wrap360(-158.277646),y= 21.554699),colour="darkblue",shape=17,size=1,show.legend=T)+#Oahu
  geom_point(aes(x=(wrap360(-177.373)),y=28.210),colour="darkblue",shape=17,size=1,show.legend=T)+#Midway
  geom_point(aes(x=(wrap360(-166.284)),y=23.870),colour="darkblue",shape=17,size=1,show.legend=T)+#Tern
  annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.justification=c(1,0), legend.position=c(1,0), legend.direction ="horizontal",
        legend.text = element_text(size=6), legend.background = element_rect(fill = "transparent"),
        legend.title=element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_2012to2016.jpeg"), dpi=600)



quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("blue", "steelblue2","lightblue"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(species=="LAAL")%>%filter(colony=="Oahu"), 
      aes(x = wrap360(lon), y = lat, group = ID), color=pal1[3], size = 0.3) +
   geom_point(aes(x=130,y=64),colour=pal1[2], size = 4)+
  geom_point(aes(x=130,y=62),colour=pal1[3], size = 4)+
  geom_point(aes(x=130,y=60),colour=pal1[5], size = 4)+
  annotate("text",x=wrap360(133),y=64,label="STAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(133),y=62,label="LAAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(133),y=60,label="BFAL",color="white",size=3,hjust = 0)+
  geom_point(aes(x=142.1243,y=27.6835),colour="darkblue",shape=17,size=1,show.legend=T)+#Torishima
  geom_point(aes(x=140.3073,y=30.47617),colour="darkblue",shape=17,size=1,show.legend=T)+#Mukukima
  geom_point(aes(x=(wrap360(-178.293552)),y=28.392533),colour="darkblue",shape=17,size=1,show.legend=T)+#Kure
  geom_point(aes(x=wrap360(-159.518980),y=22.207736),colour="darkblue",shape=17,size=1,show.legend=T)+#Kauai
  geom_point(aes(x=wrap360(-158.277646),y= 21.554699),colour="darkblue",shape=17,size=1,show.legend=T)+#Oahu
  geom_point(aes(x=(wrap360(-177.373)),y=28.210),colour="darkblue",shape=17,size=1,show.legend=T)+#Midway
  geom_point(aes(x=(wrap360(-166.284)),y=23.870),colour="darkblue",shape=17,size=1,show.legend=T)+#Tern
  annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.justification=c(1,0), legend.position=c(1,0), legend.direction ="horizontal",
        legend.text = element_text(size=6), legend.background = element_rect(fill = "transparent"),
        legend.title=element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_LAAL_Oahu.png"), dpi=300)

names(alllocs)
alllocs$year<-year(alllocs$datetime)
a<-alllocs%>%filter(species=="STAL")%>%filter(year>2011)
unique(a$ID)
a%>%group_by(ID)%>%summarise(n=n(),maxDate=max(datetime))


quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("blue", "steelblue2","lightblue"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(ID==259817), 
            aes(x = wrap360(lon), y = lat, group = ID), color=pal1[3], size = 0.3) +
  geom_point(aes(x=130,y=64),colour=pal1[2], size = 4)+
  geom_point(aes(x=130,y=62),colour=pal1[3], size = 4)+
  geom_point(aes(x=130,y=60),colour=pal1[5], size = 4)+
  annotate("text",x=wrap360(133),y=64,label="STAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(133),y=62,label="LAAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(133),y=60,label="BFAL",color="white",size=3,hjust = 0)+
  geom_point(aes(x=142.1243,y=27.6835),colour="darkblue",shape=17,size=1,show.legend=T)+#Torishima
  geom_point(aes(x=140.3073,y=30.47617),colour="darkblue",shape=17,size=1,show.legend=T)+#Mukukima
  geom_point(aes(x=(wrap360(-178.293552)),y=28.392533),colour="darkblue",shape=17,size=1,show.legend=T)+#Kure
  geom_point(aes(x=wrap360(-159.518980),y=22.207736),colour="darkblue",shape=17,size=1,show.legend=T)+#Kauai
  geom_point(aes(x=wrap360(-158.277646),y= 21.554699),colour="darkblue",shape=17,size=1,show.legend=T)+#Oahu
  geom_point(aes(x=(wrap360(-177.373)),y=28.210),colour="darkblue",shape=17,size=1,show.legend=T)+#Midway
  geom_point(aes(x=(wrap360(-166.284)),y=23.870),colour="darkblue",shape=17,size=1,show.legend=T)+#Tern
  annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.justification=c(1,0), legend.position=c(1,0), legend.direction ="horizontal",
        legend.text = element_text(size=6), legend.background = element_rect(fill = "transparent"),
        legend.title=element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_LAAL_Oahu.png"), dpi=300)

alllocs$month<-month(alllocs$datetime)
alllocs%>%filter(species=="STAL")%>%group_by(ID)%>%
  summarise(min.dc=min(deploy.count),
            col=min(colony))
(s.flege<-alllocs%>%filter(species=="STAL")%>%filter(ID>8000)%>%
  group_by(colony, ID)%>%
  summarise(minT=min(datetime),
            maxT=max(datetime))%>%
  mutate(dur=maxT-minT,
         monthMax=month(maxT)))
s.flege%>%group_by(colony)%>%summarise(meandur=mean(dur),
                                       sddur=sd(dur))
#
(s.flege<-alllocs%>%filter(species=="STAL")%>%filter(ID<8000)%>%
    group_by(colony, ID)%>%
    summarise(minT=min(datetime),
              maxT=max(datetime),
              max.dc=max(deploy.count))%>%
    mutate(dur=maxT-minT,
           monthMax=month(maxT)))
s.flege$dur<-s.flege$dur/86400
s.flege$age<-s.flege$max.dc/365
s.flege%>%group_by(colony)%>%summarise(meandur=mean(dur),
                                       sddur=sd(dur))
