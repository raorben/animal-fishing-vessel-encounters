library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(sp)
library(adehabitatLT)
library(sf)
library(parallel)
library(doParallel)

wrap360 = function(lon) {
  lon360<-ifelse(lon<0,lon+360,lon)
  return(lon360)
}

unwrap360<-function(lon360){
  lon<-ifelse(lon360 > 180, -360 + lon360, lon360)
  return(lon)}

matlab2POS = function(x,tz = "UTC") {
  days = x - 719529 #719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 #86400 seconds in a day
  return(as.POSIXct(secs,origin = "1970-1-1",tz = tz))#returns POSIXct object
}

if(Sys.info()[7]=="rachaelorben") {userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'}

###reads in functions in function folder....
source(paste0(userdir,"/Analysis/scripts/Functions_AlbatrossGFW.R"))

alltracks<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr.rds"))
colnames(alltracks)
alltracks$oid<-1:nrow(alltracks)
alltracks$lon<-unwrap360(alltracks$lon360)

ggplot()+
  geom_path(data=alltracks,aes(x=wrap360(lon),y=lat,group=UniID))
ggplot()+
  geom_point(data=alltracks,aes(x=wrap360(lon),y=lat,group=UniID))
#pull out distinct locations with boats <=30km ***much fewer bird points!!
alltracks_short<-alltracks%>%
  dplyr::select(oid,lat,lon)
head(alltracks_short)

#http://www.marineregions.org/downloads.php World EEZ v10 (2018-02-21, 119 MB) 
nc <- st_read(dsn = "/Users/rachaelorben/Dropbox/Envrion_Vars/World_EEZ_v10_20180221/eez_v10.shp")
plot(nc)
st_crs(nc)

nc_360<-nc%>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

nc_360s<-st_simplify(nc_360, dTolerance = .1) #~6nm smoother / ~11km

ggplot()+
  geom_polygon(data=nc_360s,aes(x=x_1,y=y_1))

max(alltracks$lon360); min(alltracks$lon360)
unwrap360(244.2935)

##make bounding box for tracking data - eventually automate xmin/max & ymin/max
x_coord <- c(130, -110, -110, 130, 130)
y_coord <- c(18, 18, 65, 65,18)
polygon <-  cbind(x_coord, y_coord) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 4326, check_ring_dir = TRUE) %>%
  st_sf() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

selIDX<-st_intersects(nc_360s, polygon, sparse = FALSE) #finds which eezs are inside the box
nc_360_NP<-nc_360s%>%dplyr::filter(selIDX==TRUE) #selects the eezs in the box
nc_360_NP<-st_buffer(nc_360_NP, 0.0)#works!!!! needed to make the st_intersection code work - not the ideal fix
st_is_valid(nc_360_NP) #needs to be all TRUE

nc_360_NP_crop<-st_intersection(nc_360_NP, polygon) #cuts the eezs inside the box to the box
plot(st_geometry(nc_360_NP_crop))

# convert points into sf object
colnames(alltracks_short)

alltracks_short$Xoid<-1:nrow(alltracks_short)
points <- st_as_sf(alltracks_short,coords=c('lon','lat'),remove = F,crs = 4326)%>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
glimpse(points)

colnames(nc_360_NP_crop)
nc_360_NP_cropS<-nc_360_NP_crop%>%dplyr::select(GeoName,Territory1) #shortens the information that will be attached to bird data

seqIDX<-seq(from = 1,to = nrow(alltracks_short),by = 10000) #this can be changed, larger chunks seemed to much for my computer
seqIDX[length(seqIDX)]<-nrow(alltracks_short) #makes the last selection of points end with the data
no_cores <- detectCores() - 1

bird_eez<-NULL
for (j in 1:(length(seqIDX)-1)){
  cl <- makeCluster(no_cores) #calls the clusters
  registerDoParallel(cl) #somehow registers the parallel backend with the foreach package.
  print(seqIDX[j])
  print(Sys.time())
  x <- foreach(i = seqIDX[j]:seqIDX[j+1],
               .combine=rbind,
               .packages=c('sf','dplyr')) %dopar% {st_intersection(nc_360_NP_cropS,points[i,])}
  
  bird_eez<-rbind(bird_eez,x)
  stopCluster(cl)
}

eez_df<-fortify(bird_eez)#makes sf into a dataframe 

saveRDS(eez_df,paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_EZZ_temp.rds"))
alltracks_eez<-left_join(alltracks,eez_df%>%dplyr::select(oid,GeoName,Territory1),by="oid")
saveRDS(alltracks_eez,paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_EZZ.rds"))
alltracks_eez<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_EZZ.rds"))


quartz()
ggplot()+
  geom_point(data=alltracks_eez,aes(x=lon360,y=lat,color=Territory1))



# Longhurst ---------------------------------------------------------------
nc <- st_read(dsn = "/Users/rachaelorben/Dropbox/Envrion_Vars/LonghurstEcoregions/longhurst_v4_2010/Longhurst_world_v4_2010.shp")
st_crs(nc)
plot(st_geometry(nc))

nc_360s<-nc%>%st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
#nc_360s<-st_simplify(nc_360, dTolerance = .1) #~6nm smoother / ~11km

##make bounding box for tracking data
x_coord <- c(130, -110, -110, 130, 130)
y_coord <- c(18, 18, 65, 65,18)
polygon <-  cbind(x_coord, y_coord) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 4326, check_ring_dir = TRUE) %>%
  st_sf() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

selIDX<-st_intersects(nc_360s, polygon, sparse = FALSE) #finds which eezs are inside the box
nc_360_NP<-nc_360s%>%dplyr::filter(selIDX==TRUE) #selects the eezs in the box
nc_360_NP<-st_buffer(nc_360_NP, 0.0)#works!!!! needed to make the st_intersection code work - not the ideal fix
st_is_valid(nc_360_NP) #needs to be all TRUE

nc_360_NP_cropS<-st_intersection(nc_360_NP, polygon) #cuts the eezs inside the box to the box
plot(st_geometry(nc_360_NP_cropS))

colnames(nc_360_NP_crop)

seqIDX<-seq(from = 1,to = nrow(alltracks_short),by = 10000) #this can be changed, larger chunks seemed to much for my computer
seqIDX[length(seqIDX)]<-nrow(alltracks_short) #makes the last selection of points end with the data
no_cores <- detectCores() - 1

bird_lh<-NULL
for (j in 1:(length(seqIDX)-1)){
  cl <- makeCluster(no_cores) #calls the clusters
  registerDoParallel(cl) #somehow registers the parallel backend with the foreach package.
  print(seqIDX[j])
  print(Sys.time())
  x <- foreach(i = seqIDX[j]:seqIDX[j+1],
               .combine=rbind,
               .packages=c('sf','dplyr')) %dopar% {st_intersection(nc_360_NP_cropS,points[i,])}
  
  bird_lh<-rbind(bird_lh,x)
  stopCluster(cl)
}

lh_df<-fortify(bird_lh)#makes sf into a dataframe 

saveRDS(lh_df,paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_lh_temp.rds"))
lh_df<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_lh_temp.rds"))
colnames(lh_df)
alltracks_lh<-left_join(alltracks_eez,lh_df%>%dplyr::select(ProvCode,ProvDescr,oid),by="oid")
saveRDS(alltracks_lh,paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_EEZ_lh.rds"))

alltracks_lh<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_EEZ_lh.rds"))

quartz()
ggplot()+
  geom_point(data=alltracks_lh,aes(x=lon360,y=lat,color=ProvCode))
ggsave(paste0(userdir,"/Analysis/PLOTS/LonghurstEcoRegions.png"))



# FAO ---------------------------------------------------------------------

nc <- st_read(dsn = "/Users/rachaelorben/Dropbox/Envrion_Vars/FAO_AREAS/FAO_AREAS.shp")
st_crs(nc)
plot(st_geometry(nc))

nc_360s<-nc%>%st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
#nc_360s<-st_simplify(nc_360, dTolerance = .1) #~6nm smoother / ~11km

##make bounding box for tracking data
x_coord <- c(130, -110, -110, 130, 130)
y_coord <- c(18, 18, 65, 65,18)
polygon <-  cbind(x_coord, y_coord) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 4326, check_ring_dir = TRUE) %>%
  st_sf() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

selIDX<-st_intersects(nc_360s, polygon, sparse = FALSE) #finds which eezs are inside the box
nc_360_NP<-nc_360s%>%dplyr::filter(selIDX==TRUE) #selects the eezs in the box
nc_360_NP<-st_buffer(nc_360_NP, 0.0)#works!!!! needed to make the st_intersection code work - not the ideal fix
st_is_valid(nc_360_NP) #needs to be all TRUE

nc_360_NP_cropS<-st_intersection(nc_360_NP, polygon) #cuts the eezs inside the box to the box
plot(st_geometry(nc_360_NP_cropS))

colnames(nc_360_NP_cropS)

seqIDX<-seq(from = 1,to = nrow(alltracks_short),by = 10000) #this can be changed, larger chunks seemed to much for my computer
seqIDX[length(seqIDX)]<-nrow(alltracks_short) #makes the last selection of points end with the data
no_cores <- detectCores() - 1

bird_fao<-NULL
for (j in 1:(length(seqIDX)-1)){
  cl <- makeCluster(no_cores) #calls the clusters
  registerDoParallel(cl) #somehow registers the parallel backend with the foreach package.
  print(seqIDX[j])
  print(Sys.time())
  x <- foreach(i = seqIDX[j]:seqIDX[j+1],
               .combine=rbind,
               .packages=c('sf','dplyr')) %dopar% {st_intersection(nc_360_NP_cropS,points[i,])}
  
  bird_fao<-rbind(bird_fao,x)
  stopCluster(cl)
}

fao_df<-fortify(bird_fao)#makes sf into a dataframe 

saveRDS(fao_df,paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_EEZ_lh_FAO_temp.rds"))
lh_df<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_EEZ_lh_FAO_temp.rds"))
colnames(fao_df)
alltracks_fao<-left_join(alltracks_lh,fao_df%>%dplyr::select(-lat,-lon,-Xoid,-geometry),by="oid")
saveRDS(alltracks_fao,paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_EEZ_lh_FAO.rds"))

alltracks_lh<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_EEZ_lh_FAO.rds"))

quartz()
ggplot()+
  geom_point(data=alltracks_fao,aes(x=lon360,y=lat,color=as.factor(FID)))
ggsave(paste0(userdir,"/Analysis/PLOTS/FAO_Regions.png"))

