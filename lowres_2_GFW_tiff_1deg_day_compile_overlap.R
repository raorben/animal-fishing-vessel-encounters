rm(list=ls())

library(raster)
library(sp)
library(rgdal)
library(lubridate)

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

# # list path to seasonal files
# files <- list.files(paste0("/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/daily_geotiffs"), pattern = ".tif",full.names=TRUE,recursive = T)
# 
# files12<-subset(files, grepl("2012",files))
# files13<-subset(files, grepl("2013",files))
# files14<-subset(files, grepl("2014",files))
# files15<-subset(files, grepl("2015",files))
# files16<-subset(files, grepl("2016",files))
# #files17<-subset(files, grepl("2017",files))
# 
# # 2012: stack the list of rasters into a RasterStack object  --------------
# adt_list<-NULL
# for (i in 1: length(files12)){
#   dayR <- raster(files12[i])
#   adt_list <- c(adt_list, list(dayR))
# }
# GFW12 <- stack(adt_list)
# saveRDS(GFW12, file = paste0(userdir,"/Analysis/compileddata/GFW12_rasterstack_1.rda"))
# 
# # 2013: stack the list of rasters into a RasterStack object  --------------
# adt_list<-NULL
# for (i in 1: length(files13)){
#   dayR <- raster(files13[i])
#   adt_list <- c(adt_list, list(dayR))
# }
# GFW13 <- stack(adt_list)
# saveRDS(GFW13, file = paste0(userdir,"/Analysis/compileddata/GFW13_rasterstack_1.rda"))
# 
# # 2014: stack the list of rasters into a RasterStack object  --------------
# adt_list<-NULL
# for (i in 1: length(files14)){
#   dayR <- raster(files14[i])
#   adt_list <- c(adt_list, list(dayR))
# }
# GFW14 <- stack(adt_list)
# saveRDS(GFW14, file = paste0(userdir,"/Analysis/compileddata/GFW14_rasterstack_1.rda"))
# 
# # 2015: stack the list of rasters into a RasterStack object  --------------
# adt_list<-NULL
# for (i in 1: length(files15)){
#   dayR <- raster(files15[i])
#   adt_list <- c(adt_list, list(dayR))
# }
# GFW15 <- stack(adt_list)
# saveRDS(GFW15, file = paste0(userdir,"/Analysis/compileddata/GFW15_rasterstack_1.rda"))
# 
# # 2016: stack the list of rasters into a RasterStack object  --------------
# adt_list<-NULL
# for (i in 1: length(files16)){
#   dayR <- raster(files16[i])
#   adt_list <- c(adt_list, list(dayR))
# }
# GFW16 <- stack(adt_list)
# saveRDS(GFW16, file = paste0(userdir,"/Analysis/compileddata/GFW16_rasterstack_1.rda"))
# 

# load GFW raster stack ---------------------------------------------------
GFW12<-readRDS(file = paste0(userdir,"/Analysis/compileddata/GFW12_rasterstack_1.rda"))
GFW13<-readRDS(file = paste0(userdir,"/Analysis/compileddata/GFW13_rasterstack_1.rda"))
GFW14<-readRDS(file = paste0(userdir,"/Analysis/compileddata/GFW14_rasterstack_1.rda"))
GFW15<-readRDS(file = paste0(userdir,"/Analysis/compileddata/GFW15_rasterstack_1.rda"))
GFW16<-readRDS(file = paste0(userdir,"/Analysis/compileddata/GFW16_rasterstack_1.rda"))

# load GPS tracking data --------------------------------------------------
alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr.rds"))

#alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/AlbatrossData_bytrip.rda"))
alllocs$lon360<-wrap360(alllocs$lon)
alllocs$global_oid<-1:nrow(alllocs)
unique(year(alllocs$datetime))
alllocs$yearT<-year(alllocs$datetime)
nrow(alllocs[is.na(alllocs$yearT)==TRUE,])
nrow(alllocs[(alllocs$yearT)==2011,])

alllocs<-alllocs[(alllocs$yearT)!=2011,]
alllocs<-alllocs[(alllocs$datetime)>"2012-01-02 00:00:00 GMT",] #removed the first day of 2012 since the raster came out differently
min(alllocs$datetime)

# Prep for extraction -----------------------------------------------------
# Extract GFWd ---------------------------------------------------------

# buffer -------------------------------------------------------------
#need to create a weird column to match the names in the RasterStack 
#the names come from the file names
alllocs$dt_name<-paste0("X",year(alllocs$datetime),padz(month(alllocs$datetime),2),padz(day(alllocs$datetime),2))

buffersize_m=c(3000,30000,80000)
croplim=2

locs2012<-cropNextract(rasterstackG=GFW12,tracks=alllocs,yr=2012,buffersize_m,croplim)
saveRDS(locs2012, file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2012_LR1.rda"))
#locs2012<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2012_LR1.rda"))

#we are missing May 31, 2013 (noted: 8.30.2017)
alllocs13<-alllocs[alllocs$datetime<as.POSIXct("2013-05-31 00:00:00",tz= "GMT") | alllocs$datetime>as.POSIXct("2013-05-31 23:59:59",tz= "GMT") ,]
locs2013<-cropNextract(rasterstackG=GFW13,tracks=alllocs13,yr=2013,buffersize_m,croplim)
saveRDS(locs2013, file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2013_LR1.rda"))
#locs2013<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2013_LR1.rda"))

locs2014<-cropNextract(rasterstackG=GFW14,tracks=alllocs,yr=2014,buffersize_m,croplim)
saveRDS(locs2014, file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2014_LR1.rda"))
#locs2014<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2014_LR1.rda"))

locs2015<-cropNextract(rasterstackG=GFW15,tracks=alllocs,yr=2015,buffersize_m,croplim)
saveRDS(locs2015, file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2015_LR1.rda"))
#locs2015<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2015_LR1.rda"))

locs2016<-cropNextract(rasterstackG=GFW16,tracks=alllocs,yr=2016,buffersize_m,croplim)
saveRDS(locs2016, file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2016_LR1.rda"))
#locs2016<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2016_LR1.rda"))

alllocsG<-rbind(locs2012,locs2013,locs2014,locs2015,locs2016)

#make a nested factor 
alllocsG$fishing<-"no boat"
alllocsG$fishing[alllocsG$GFWdM_80km==1]<-80
alllocsG$fishing[alllocsG$GFWdM_30km==1]<-30
alllocsG$fishing[alllocsG$GFWdM_3km==1]<-3
alllocsG$fishingF <- factor(alllocsG$fishing, levels = c("3","30","80","no boat"))

alllocsG$yearT<-year(alllocsG$datetime)
alllocsG$lon360<-wrap360(alllocsG$lon)



colnames(alllocsG)
alllocsGG <- alllocsG %>% group_by(ID) %>% arrange(datetime)
dt<-Sys.Date()

saveRDS(alllocsGG, file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_LR1_",dt,".rda"))



# proofing plots ----------------------------------------------------------
alllocsG<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_LR1_","2018-11-17",".rda"))

w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("USA","Russia","China","Japan","Mexico","Canada","North Korea","South Korea"),]


quartz(width=12,height=6)
ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray25",color="grey60",size=0.1)+
  geom_path(data=alllocsG,aes(x=lon360, y=lat, group=UniID),
            show.legend = FALSE,color="grey50")+
  geom_point(data=alllocsG[alllocsG$fishingF=="3",],aes(x=lon360,y=lat,color=fishingF))+
  scale_color_viridis(discrete = TRUE)+
  coord_fixed(ratio=1.7,xlim = c(130,240),ylim=c(20,65))+
  theme(legend.position = "right")+facet_wrap(~yearT)+
  theme_classic()
quartz.save(paste0(userdir,"/Analysis/PLOTS/GFW_albatross_1day_LR1_3km.png"), dpi=300)


quartz(width=12,height=6)
ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray25",color="grey60",size=0.1)+
  geom_path(data=alllocsG,aes(x=lon360, y=lat, group=UniID),color="grey50",show.legend = FALSE)+
  geom_point(data=alllocsG[alllocsG$fishingF=="30",],aes(x=lon360,y=lat,color=fishingF))+
  scale_color_viridis(discrete = TRUE)+
  coord_fixed(ratio=1.7,xlim = c(130,240),ylim=c(20,65))+
  theme(legend.position = "right")+facet_wrap(~species)+
  theme_classic()
quartz.save(paste0(userdir,"/Analysis/PLOTS/GFW_albatross_1day_LR1_30km.png"), dpi=300)


# Hourly Interpolated LAAL & BFAL -----------------------------------------
alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_60minbytrip.rds"))

#alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/AlbatrossData_bytrip.rda"))
alllocs$lon360<-wrap360(alllocs$lon)
alllocs$global_oid<-1:nrow(alllocs)
unique(year(alllocs$datetime))
alllocs$yearT<-year(alllocs$datetime)
nrow(alllocs[is.na(alllocs$yearT)==TRUE,])
nrow(alllocs[(alllocs$yearT)==2011,])

alllocs<-alllocs[(alllocs$yearT)!=2011,]
alllocs<-alllocs[(alllocs$datetime)>"2012-01-02 00:00:00 GMT",] #removed the first day of 2012 since the raster came out differently
min(alllocs$datetime)

# Prep for extraction -----------------------------------------------------
# Extract GFWd ---------------------------------------------------------

# buffer -------------------------------------------------------------
#need to create a weird column to match the names in the RasterStack 
#the names come from the file names
alllocs$dt_name<-paste0("X",year(alllocs$datetime),padz(month(alllocs$datetime),2),padz(day(alllocs$datetime),2))

buffersize_m=c(3000,30000,80000)
croplim=2

locs2012<-cropNextract(rasterstackG=GFW12,tracks=alllocs,yr=2012,buffersize_m,croplim)
saveRDS(locs2012, file=paste0(userdir,"/Analysis/compileddata/Hr_LABFAL_Albatross_GFWbuff_2012_LR1.rda"))
#locs2012<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2012_LR1.rda"))

#we are missing May 31, 2013 (noted: 8.30.2017)
alllocs13<-alllocs[alllocs$datetime<as.POSIXct("2013-05-31 00:00:00",tz= "GMT") | alllocs$datetime>as.POSIXct("2013-05-31 23:59:59",tz= "GMT") ,]
locs2013<-cropNextract(rasterstackG=GFW13,tracks=alllocs13,yr=2013,buffersize_m,croplim)
saveRDS(locs2013, file=paste0(userdir,"/Analysis/compileddata/Hr_LABFAL_Albatross_GFWbuff_2013_LR1.rda"))
#locs2013<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2013_LR1.rda"))

locs2014<-cropNextract(rasterstackG=GFW14,tracks=alllocs,yr=2014,buffersize_m,croplim)
saveRDS(locs2014, file=paste0(userdir,"/Analysis/compileddata/Hr_LABFAL_Albatross_GFWbuff_2014_LR1.rda"))
#locs2014<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2014_LR1.rda"))

locs2015<-cropNextract(rasterstackG=GFW15,tracks=alllocs,yr=2015,buffersize_m,croplim)
saveRDS(locs2015, file=paste0(userdir,"/Analysis/compileddata/Hr_LABFAL_Albatross_GFWbuff_2015_LR1.rda"))
#locs2015<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2015_LR1.rda"))

locs2016<-cropNextract(rasterstackG=GFW16,tracks=alllocs,yr=2016,buffersize_m,croplim)
saveRDS(locs2016, file=paste0(userdir,"/Analysis/compileddata/Hr_LABFAL_Albatross_GFWbuff_2016_LR1.rda"))
#locs2016<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatross_GFWbuff_2016_LR1.rda"))

alllocsG<-rbind(locs2012,locs2013,locs2014,locs2015,locs2016)

#make a nested factor 
alllocsG$fishing<-"no boat"
alllocsG$fishing[alllocsG$GFWdM_80km==1]<-80
alllocsG$fishing[alllocsG$GFWdM_30km==1]<-30
alllocsG$fishing[alllocsG$GFWdM_3km==1]<-3
alllocsG$fishingF <- factor(alllocsG$fishing, levels = c("3","30","80","no boat"))

alllocsG$yearT<-year(alllocsG$datetime)
alllocsG$lon360<-wrap360(alllocsG$lon)



colnames(alllocsG)
alllocsGG <- alllocsG %>% group_by(ID) %>% arrange(datetime)
dt<-Sys.Date()

saveRDS(alllocsGG, file=paste0(userdir,"/Analysis/compileddata/Hr_LABFAL_Albatross_GFWbuff_LR1_",dt,".rda"))

