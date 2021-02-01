rm(list=ls())

library(raster)
library(sp)
library(rgdal)
library(lubridate)
library(plyr)
library(compare)
library(dplyr)
library(readr)
library(ggplot2)
library(viridis)

wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

if(Sys.info()[7]=="rachaelorben") userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'

#bring in GFW Functions
source(paste0(userdir,"/Analysis/scripts/Functions_AlbatrossGFW.R"))
# make a raster stack  ------------------------------------------

# list path to seasonal files
files <- list.files(paste0("/Volumes/GoogleDrive/My Drive/GFW fishing effort/daily_geotiffs"), pattern = ".tif",full.names=TRUE,recursive = T)

files12<-subset(files, grepl("2012",files))
files13<-subset(files, grepl("2013",files))
files14<-subset(files, grepl("2014",files))
files15<-subset(files, grepl("2015",files))
files16<-subset(files, grepl("2016",files))
#files17<-subset(files, grepl("2017",files))

# # 2012: stack the list of rasters into a RasterStack object  --------------
adt_list<-NULL
for (i in 1: length(files12)){
dayR <- raster(files12[i])
#   adt_list <- c(adt_list, list(dayR))
# }
# GFW12 <- stack(adt_list)
# saveRDS(GFW12, file = paste0(userdir,"/Analysis/compileddata/GFW12_rasterstack_01.rda"))
# 
# # 2013: stack the list of rasters into a RasterStack object  --------------
# adt_list<-NULL
# for (i in 1: length(files13)){
#   dayR <- raster(files13[i])
#   adt_list <- c(adt_list, list(dayR))
# }
# GFW13 <- stack(adt_list)
# saveRDS(GFW13, file = paste0(userdir,"/Analysis/compileddata/GFW13_rasterstack_01.rda"))
# 
# # 2014: stack the list of rasters into a RasterStack object  --------------
# adt_list<-NULL
# for (i in 1: length(files14)){
#   dayR <- raster(files14[i])
#   adt_list <- c(adt_list, list(dayR))
# }
# GFW14 <- stack(adt_list)
# saveRDS(GFW14, file = paste0(userdir,"/Analysis/compileddata/GFW14_rasterstack_01.rda"))
# 
# # 2015: stack the list of rasters into a RasterStack object  --------------
# adt_list<-NULL
# for (i in 1: length(files15)){
#   dayR <- raster(files15[i])
#   adt_list <- c(adt_list, list(dayR))
# }
# GFW15 <- stack(adt_list)
# saveRDS(GFW15, file = paste0(userdir,"/Analysis/compileddata/GFW15_rasterstack_01.rda"))
# 
# # 2016: stack the list of rasters into a RasterStack object  --------------
# adt_list<-NULL
# for (i in 1: length(files16)){
#   dayR <- raster(files16[i])
#   adt_list <- c(adt_list, list(dayR))
# }
# GFW16 <- stack(adt_list)
# saveRDS(GFW16, file = paste0(userdir,"/Analysis/compileddata/GFW16_rasterstack_01.rda"))
# 

# load GFW raster stack ---------------------------------------------------
GFW12<-readRDS(file = paste0(userdir,"/Analysis/compileddata/GFW12_rasterstack_01.rda"))
GFW13<-readRDS(file = paste0(userdir,"/Analysis/compileddata/GFW13_rasterstack_01.rda"))
GFW14<-readRDS(file = paste0(userdir,"/Analysis/compileddata/GFW14_rasterstack_01.rda"))
GFW15<-readRDS(file = paste0(userdir,"/Analysis/compileddata/GFW15_rasterstack_01.rda"))
GFW16<-readRDS(file = paste0(userdir,"/Analysis/compileddata/GFW16_rasterstack_01.rda"))

# load GPS tracking data --------------------------------------------------
#alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/AlbatrossData_bytrip.rda"))
alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr.rds"))
#alllocs$lon360<-wrap360(alllocs$lon)
alllocs$global_oid<-1:nrow(alllocs)
head(alllocs)
unique(year(alllocs$datetime))
alllocs$yearT<-year(alllocs$datetime)
a<-alllocs[is.na(alllocs$yearT)==TRUE,]
b<-alllocs[(alllocs$yearT)==2011,]
summary(tracks$lon360)
summary(tracks$yearT)
yr=2012
locs<-alllocs%>%dplyr::filter(yearT==yr)
unique(locs$yearT)
#min(alllocs$lon360);max(alllocs$lon360)
#min(alllocs$lat);max(alllocs$lat)

# Prep for extraction -----------------------------------------------------

padz <- function(x, n=max(nchar(x))) gsub(" ", "0", formatC(x, width=n))

# Extract GFWd ---------------------------------------------------------
# next modification, add a statement to allow for multiple buffers to be searched at one time - should be faster

cropNextract<-function(rasterstackG,tracks,yr,buffersize_m,croplim=1){
  #buffer: units are m, buffer is a radius from each point (circle of extraction)
  #croplim: units are degrees, choose a limit large enough to include all of your buffer

locs<-tracks%>%dplyr::filter(yearT==yr)
days<-unique(locs$dt_name)
nData<-data.frame()
for (i in 1:length(days)){
  #for (i in 2:64){
  print(i)
  day<-days[i]
  d<-locs[locs$dt_name==day,]
  d2<-locs[locs$dt_name==day,]

  new.extent <- extent((min(d$lon360)-croplim),(max(d$lon360)+croplim), (min(d$lat)-croplim),(max(d$lat)+croplim))

  id<-which(names(rasterstackG) == d$dt_name[1])
  rday<-rasterstackG[[id]]
  rday<-crop(x = rday, y = new.extent)

  for (k in 1:length(buffersize_m)){
  GFWd <- raster::extract(rday, d[,c("lon360","lat")],buffer=buffersize_m[k],nl = 1.)

  # get the max for each point (buffer) by layer
  GFWdM <- lapply(GFWd, function(x) max(x, na.rm=TRUE))

  d2$GFWdM<-unlist(GFWdM)
  dd<-data.frame(d2$GFWdM)
  colnames(dd)<- paste0("GFWdM_", buffersize_m[k]/1000,"km")
  d<-cbind(d,dd)
  }
  nData<-rbind(nData,d)
}
return(nData)}


# buffer -------------------------------------------------------------
#need to create a weird column to match the names in the RasterStack 
#the names come from the file names
alllocs$dt_name<-paste0("X",year(alllocs$datetime),padz(month(alllocs$datetime),2),padz(day(alllocs$datetime),2))
colnames(alllocs)
buffersize_m=c(1,1000,5000,10000,15000,30000,60000)
croplim=2

locs2012<-cropNextract(rasterstackG=GFW12,tracks=alllocs,yr=2012,buffersize_m,croplim)
saveRDS(locs2012, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2012interactions_HR01.rda"))
#locs2012<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2012interactions_HR01.rda"))

#we are missing May 31, 2013 (noted: 8.30.2017)
alllocs13<-alllocs[alllocs$datetime<as.POSIXct("2013-05-31 00:00:00",tz= "GMT") | alllocs$datetime>as.POSIXct("2013-05-31 23:59:59",tz= "GMT") ,]
locs2013<-cropNextract(rasterstackG=GFW13,tracks=alllocs13,yr=2013,buffersize_m,croplim)
saveRDS(locs2013, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2013interactions_HR01.rda"))
#locs2013<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2013interactions_HR01.rda"))

locs2014<-cropNextract(rasterstackG=GFW14,tracks=alllocs,yr=2014,buffersize_m,croplim)
saveRDS(locs2014, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2014interactions_HR01.rda"))
#locs2014<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2014interactions_HR01.rda"))

locs2015<-cropNextract(rasterstackG=GFW15,tracks=alllocs,yr=2015,buffersize_m,croplim)
saveRDS(locs2015, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2015interactions_HR01.rda"))
#locs2015<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2015interactions_HR01.rda"))

locs2016<-cropNextract(rasterstackG=GFW16,tracks=alllocs,yr=2016,buffersize_m,croplim)
saveRDS(locs2016, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2016interactions_HR01.rda"))
#locs2016<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2016interactions_HR01.rda"))

alllocsG<-rbind(locs2012,locs2013,locs2014,locs2015,locs2016)

#make a nested factor 
alllocsG$fishing<-NA
for (i in 1:nrow(alllocsG)){if (alllocsG$GFWdM_60km[i]==1){alllocsG$fishing[i]<-60}}
for (i in 1:nrow(alllocsG)){if (alllocsG$GFWdM_30km[i]==1){alllocsG$fishing[i]<-30}}
for (i in 1:nrow(alllocsG)){if (alllocsG$GFWdM_15km[i]==1){alllocsG$fishing[i]<-15}}
for (i in 1:nrow(alllocsG)){if (alllocsG$GFWdM_10km[i]==1){alllocsG$fishing[i]<-10}}
for (i in 1:nrow(alllocsG)){if (alllocsG$GFWdM_5km[i]==1){alllocsG$fishing[i]<-5}}
for (i in 1:nrow(alllocsG)){if (alllocsG$GFWdM_1km[i]==1){alllocsG$fishing[i]<-1}}
alllocsG$fishing[is.na(alllocsG$fishing)==TRUE]<-"no boat"
unique(alllocsG$fishing)
alllocsG$fishingF <- factor(alllocsG$fishing, levels = c("1","5","10","15","30","60","no boat"))
alllocsGG <- alllocsG %>% group_by(species,ID) %>% arrange(datetime)
saveRDS(alllocsGG, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_interactions_HR01.rda"))

alllocsG<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_interactions_HR01.rda"))

