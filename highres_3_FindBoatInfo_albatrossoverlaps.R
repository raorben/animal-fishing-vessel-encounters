library(dplyr)
library(ggplot2)
library(grid)

#for eez extraction
library(sf) #spatial eez
library(lwgeom) #spatial eez
#for eez extraction
library(parallel) 
library(doParallel)
library(foreach) #parallel computing

wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

# Go find more data from .csvs
# Directory with daily GFW .csv files
if(Sys.info()[6]=="rachaelorben") {
  GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/daily_csvs/";
  userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'} ##RAO

alllocsG<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_interactions_HR01.rda"))

#bring in GFW Functions
source(paste0(userdir,"/Analysis/scripts/Functions_AlbatrossGFW.R"))

# extract boat data -------------------------------------------------
#(takes a while)
BOATnALBIES12<-extractBoatInfo(alllocsG=alllocsG%>%filter(fishingF!="no boat"),
                                  yr=2012,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir, resolution="HR")
saveRDS(BOATnALBIES12, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2012interactions_HR01_boatinfo.rda"))


BOATnALBIES13<-extractBoatInfo(alllocsG=alllocsG%>%filter(fishingF!="no boat"),
                                  yr=2013,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir, resolution="HR")
saveRDS(BOATnALBIES13, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2013interactions_HR01_boatinfo.rda"))


BOATnALBIES14<-extractBoatInfo(alllocsG=alllocsG%>%filter(fishingF!="no boat"),
                                  yr=2014,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir, resolution="HR")
saveRDS(BOATnALBIES14, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2014interactions_HR01_boatinfo.rda"))


BOATnALBIES15<-extractBoatInfo_HR(alllocsG=alllocsG_eez%>%filter(fishingF!="no boat"),
                                  yr=2015,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir, resolution="HR")
saveRDS(BOATnALBIES15, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2015interactions_HR01_boatinfo.rda"))


BOATnALBIES16<-extractBoatInfo_HR(alllocsG=alllocsG_eez%>%filter(fishingF!="no boat"),
                                  yr=2016,distlim=80,searchbox=1.5,GFWDataDir=GFWDataDir, resolution="HR")
saveRDS(BOATnALBIES16, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2016interactions_HR01_boatinfo.rda"))


BOATnALBIES12<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2012interactions_HR01_boatinfo.rda"))
BOATnALBIES13<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2013interactions_HR01_boatinfo.rda"))
BOATnALBIES14<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2014interactions_HR01_boatinfo.rda"))
BOATnALBIES15<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2015interactions_HR01_boatinfo.rda"))
BOATnALBIES16<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_2016interactions_HR01_boatinfo.rda"))

#all years
BOATnALBIES<-rbind(BOATnALBIES12,BOATnALBIES13,BOATnALBIES14,BOATnALBIES15,BOATnALBIES16)
saveRDS(BOATnALBIES, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_ALLinteractions_HR01_boatinfo.rda"))


# Add EEZ to potential interaction locations ------------------------------
BOATnALBIES<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_GFWbuff_ALLinteractions_HR01_boatinfo.rda"))

BOATnALBIES_b<-BOATnALBIES%>%dplyr::filter(Dist2Boat<=30) #limits to encounters
colnames(BOATnALBIES_b)

#pulls out distinct locations with boats <=30km ***much fewer bird points!!
BOATnALBIES_b_short<-BOATnALBIES_b%>%
  select(albatross_oid,albatross_lat,albatross_lon)%>%
  dplyr::distinct(albatross_oid,albatross_oid,albatross_lat,albatross_lon)
head(BOATnALBIES_b_short)

#http://www.marineregions.org/downloads.php World EEZ v10 (2018-02-21, 119 MB) 
nc <- st_read(dsn = "/Users/rachaelorben/Dropbox/Envrion_Vars/World_EEZ_v10_20180221/eez_v10.shp")
st_crs(nc)

nc_360<-nc%>%st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
nc_360s<-st_simplify(nc_360, dTolerance = .1) #~6nm smoother / ~11km

##make bounding box for tracking data
x_coord <- c(135, -120, -120, 135, 135)
y_coord <- c(20, 20, 65, 65,20)
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
colnames(BOATnALBIES_b_short)
BOATnALBIES_b_short$Xoid<-1:nrow(BOATnALBIES_b_short)
points <- st_as_sf(BOATnALBIES_b_short,coords=c('albatross_lon','albatross_lat'),remove = F,crs = 4326)%>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
glimpse(points)

nc_360_NP_cropS<-select(nc_360_NP_crop,GeoName,Territory1) #shortens the information that will be attached to bird data

seqIDX<-seq(from = 1,to = nrow(BOATnALBIES_b_short),by = 10000) #this can be changed, larger chunks seemed to much for my computer
seqIDX[length(seqIDX)]<-nrow(BOATnALBIES_b_short) #makes the last selection of points end with the data
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

#combines eez info data used for searching, NA = High Seas
BOATnALBIES_b_shorteez<-left_join(BOATnALBIES_b_short,eez_df,by="albatross_oid") 

#combines each albatross point with a dataframe with boats and birds (multiple boats per bird point)
BOATnALBIES_eez<-left_join(BOATnALBIES_b,BOATnALBIES_b_shorteez,by="albatross_oid") 
x<-bird_eez[duplicated(bird_eez$Xoid)==TRUE,]# how many locations are in two polygons - hopefully not many!!!!!

saveRDS(BOATnALBIES_eez, file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_EEZ_GFWbuff_ALLinteractions_HR01_boatinfo.rda"))

# Summarize extracted data - data checking --------------------------------
library(gridExtra)
library(viridis)
BOATnALBIES_eez<-readRDS(file=paste0(userdir,"/Analysis/compileddata/Albatrossnointer_EEZ_GFWbuff_ALLinteractions_HR01_boatinfo.rda"))
head(BOATnALBIES_eez)
BOATnALBIES_eez$Territory1<-as.character(BOATnALBIES_eez$Territory1)
BOATnALBIES_eez$Territory1[is.na(BOATnALBIES_eez$Territory1)==TRUE]<-"High Seas"
BOATnALBIES_eez$FishingYN<-NA
BOATnALBIES_eez$FishingYN[BOATnALBIES_eez$fishing_hours==0]<-"N"
BOATnALBIES_eez$FishingYN[BOATnALBIES_eez$fishing_hours>0]<-"Y"
BOATnALBIES_eez$Territory1[BOATnALBIES_eez$Territory1=="United States"]<-"US West Coast"

# Daily Encounters
summary(BOATnALBIES_eez)

ggplot()+
  geom_point(data = BOATnALBIES_eez%>%filter(Territory1=="High Seas"), 
             aes(x = wrap360(lon), y = lat, group = albatross_ID),  
             color=pal1[2], size = 0.1)+
  geom_path(data=NPeez,aes(x=wrap360(long),y=lat,group=group),size = 0.3)+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))

quartz()
ggplot()+
  geom_point(data = BOATnALBIES_eez, 
             aes(x = wrap360(lon), y = lat, group = albatross_ID,color=Territory1),size = 0.1)+
  geom_path(data=NPeez,aes(x=wrap360(long),y=lat,group=group),size = 0.3)+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))

quartz()
ggplot()+
  geom_point(data = BOATnALBIES_eez%>%filter(Territory1=="High Seas"), 
             aes(x = wrap360(lon), y = lat, group = albatross_ID,color=Territory1),size = 0.1)+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))

ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  geom_point(data = BOATnALBIES_eez%>%filter(Territory1!="High Seas"), 
             aes(x = wrap360(lon), y = lat, group = Territory1),  
             color=pal1[2], size = 0.1)+
  geom_path(data=NPeez,aes(x=wrap360(long),y=lat,group=group),size = 0.3)+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))


p1<-ggplot()+
  geom_boxplot(data=BOATnALBIES_eez,aes(x=flag,y=fishing_hours))+theme(axis.text.x = element_text(angle=45, hjust=1,size=7))
p2<-ggplot()+
  geom_boxplot(data=BOATnALBIES_eez,aes(x=flag,y=vessel_hours))+theme(axis.text.x = element_text(angle=45, hjust=1,size=7))
p3<-ggplot()+
  geom_boxplot(data=BOATnALBIES_eez,aes(x=geartype ,y=vessel_hours))+theme(axis.text.x = element_text(angle=45, hjust=1,size=7))
p4<-ggplot()+
  geom_boxplot(data=BOATnALBIES_eez,aes(x=geartype ,y=fishing_hours))+theme(axis.text.x = element_text(angle=45, hjust=1,size=7))
p5<-ggplot()+
  geom_histogram(data=BOATnALBIES_eez,aes(x=geartype),stat="count")+facet_wrap(~albatross_species,scales="free_y")+
  theme(axis.text.x = element_text(angle=45, hjust=1,size=7))+  ggtitle("All Fishing Boats")
p6<-ggplot()+
  geom_histogram(data=BOATnALBIES_eez,aes(x=flag),stat="count")+facet_wrap(~albatross_species,scales="free_y")+ 
  ggtitle("All Fishing Boats")+ theme(axis.text.x = element_text(angle=45, hjust=1,size=7))
p7<-ggplot()+
  geom_histogram(data=BOATnALBIES_eez%>%filter(fishing_hours>0),aes(x=geartype),stat="count")+
  facet_wrap(~albatross_species,scales="free_y")+  ggtitle("Boats Activily Fishing")+
  theme(axis.text.x = element_text(angle=45, hjust=1,size=7))
p8<-ggplot()+
  geom_histogram(data=BOATnALBIES_eez%>%filter(fishing_hours>0),aes(x=flag),stat="count")+
  facet_wrap(~albatross_species,scales="free_y")+  ggtitle("Boats Activily Fishing")+
  theme(axis.text.x = element_text(angle=45, hjust=1,size=7))
p9<-ggplot()+
  geom_histogram(data=BOATnALBIES_eez%>%filter(fishing_hours>0),aes(x=flag),stat="count")+facet_wrap(~geartype)


quartz(height=8,width=6)
grid.arrange(p1,p2,p4,p3,ncol=2)
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_2012to2016_HR_BoatEncounters_30km_fishingvsvesselhours_hist.jpeg"), dpi=300)

quartz(height=8,width=10)
grid.arrange(p5,p7,p6,p8,ncol=2)
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_2012to2016_HR_BoatEncounters_30km_countbyFlagGearSpecies_hist.jpeg"), dpi=300)


# Map Gear/Flag 30km encounters---------------------------------------------------------------------
#for plotting
pal1 <- viridis::viridis_pal(option = "magma")(5)
bathy2<-readRDS(paste0(userdir,"/Analysis/compileddata/Bathymetryforggplot.rda"))
NPeez<-readRDS(paste0(userdir,"NP_EEZ_boundaries.rda"))
w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("USA","Russia","China","Japan","Mexico","Canada","North Korea","South Korea","Taiwan","Mongolia"),]

quartz(height=4,width=8)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("blue", "steelblue2","lightblue"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  geom_path(data = alllocsG[alllocsG$species=="STAL",], aes(x = wrap360(lon), y = lat, group = ID),  color=pal1[2], size = 0.1) +
  geom_path(data = alllocsG[alllocsG$species=="LAAL",], aes(x = wrap360(lon), y = lat, group = ID), color=pal1[3], size = 0.1) +
  geom_path(data = alllocsG[alllocsG$species=="BFAL",], aes(x = wrap360(lon), y = lat, group = (ID)), color=pal1[5], size = 0.1) +
  geom_path(data=NPeez%>%filter(lat>15 & lat<70)%>%
              filter(wrap360(long)>120 & wrap360(long)<250),aes(x=wrap360(long),y=lat,group=group),size = 0.3)+
  geom_point(data = BOATnALBIES_eez%>%dplyr::filter(Dist2Boat<=30), aes(x = wrap360(lon), y = lat, group = as.factor(geartype),color = as.factor(geartype)),size = 0.5) +
  geom_point(aes(x=130,y=64),colour=pal1[2], size = 3)+
  geom_point(aes(x=130,y=62),colour=pal1[3], size = 3)+
  geom_point(aes(x=130,y=60),colour=pal1[5], size = 3)+
  annotate("text",x=wrap360(133),y=64,label="STAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(133),y=62,label="LAAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(133),y=60,label="BFAL",color="white",size=3,hjust = 0)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  guides(fill=FALSE)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  theme(legend.title=element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_2012to2016_HR_BoatEncounters_1day_30km_geartype.jpeg"), dpi=300)

library(lubridate)
BOATnALBIES_eez$year<-year(BOATnALBIES_eez$albatross_datetime)
alllocsG$year<-year(alllocsG$datetime)
data = alllocsG%>%filter(species=="LAAL")%>%filter(colony=="MIAT")
LAALindi<-unique(data$ID)
unique(data$year)
data = alllocsG%>%filter(species=="BFAL")%>%filter(colony=="MIAT")
BFALindi<-unique(data$ID)
unique(data$year)

quartz(height=4,width=8)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("blue", "steelblue2","lightblue"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  geom_path(data = alllocsG%>%filter(species=="LAAL")%>%filter(colony=="MIAT"), aes(x = wrap360(lon), y = lat, group = ID), color=pal1[3], size = 0.5) +
  geom_path(data = alllocsG%>%filter(species=="BFAL")%>%filter(colony=="MIAT"), aes(x = wrap360(lon), y = lat, group = (ID)), color=pal1[5], size = 0.5) +
  geom_path(data=NPeez%>%filter(lat>15 & lat<70)%>%
              filter(wrap360(long)>120 & wrap360(long)<250),aes(x=wrap360(long),y=lat,group=group),size = 0.3)+
  geom_point(data = BOATnALBIES_eez%>%filter(albatross_species=="LAAL" | albatross_species=="BFAL")%>%
               filter(year==2015|year==2016)%>%
               dplyr::filter(Dist2Boat<=30), 
             aes(x = wrap360(lon), y = lat, group = as.factor(geartype),color = as.factor(geartype)),
             size = 0.5) +
  #geom_point(aes(x=130,y=64),colour=pal1[2], size = 3)+
  geom_point(aes(x=165,y=42),colour=pal1[3], size = 3)+
  geom_point(aes(x=165,y=40),colour=pal1[5], size = 3)+
  #annotate("text",x=wrap360(133),y=64,label="STAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(160),y=42,label="LAAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(160),y=40,label="BFAL",color="white",size=3,hjust = 0)+
  #scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(160,192),ylim=c(19,45))+
  guides(fill=FALSE)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  theme(legend.title=element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_2012to2016_HR_BoatEncounters_1day_30km_geartype_MIDWAY.jpeg"), dpi=300)

quartz(height=4,width=8)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("blue", "steelblue2","lightblue"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  geom_path(data = alllocsG[alllocsG$species=="STAL",], aes(x = wrap360(lon), y = lat, group = ID),  color=pal1[2], size = 0.1) +
  geom_path(data = alllocsG[alllocsG$species=="LAAL",], aes(x = wrap360(lon), y = lat, group = ID), color=pal1[3], size = 0.1) +
  geom_path(data = alllocsG[alllocsG$species=="BFAL",], aes(x = wrap360(lon), y = lat, group = (ID)), color=pal1[5], size = 0.1) +
  geom_path(data=NPeez,aes(x=wrap360(long),y=lat,group=group),size = 0.3)+
  geom_point(data = BOATnALBIES%>%dplyr::filter(Dist2Boat<=30), aes(x = wrap360(lon), y = lat,color = as.factor(flag)),size = 0.5) +
  geom_point(aes(x=130,y=64),colour=pal1[2], size = 3)+
  geom_point(aes(x=130,y=62),colour=pal1[3], size = 3)+
  geom_point(aes(x=130,y=60),colour=pal1[5], size = 3)+
  geom_point(aes(x=130,y=59),colour="black", size = 2)+
  geom_point(aes(x=130,y=58.5),colour="white", size = 2)+
  annotate("text",x=wrap360(133),y=64,label="STAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(133),y=62,label="LAAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(133),y=60,label="BFAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(133),y=59,label="not fishing",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(133),y=58.5,label="fishing",color="white",size=3,hjust = 0)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  guides(fill=FALSE)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  theme(legend.title=element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_2012to2016_HR_BoatEncounters_1day_30km_flag.jpeg"), dpi=300)


# Figure 1. Map Encounters (daily) fishing vs. not fishing ----------------


Tot<- BOATnALBIES_eez %>%
    filter(Dist2Boat<=30)%>% group_by(Territory1)%>%
  summarise(nTot=n())
TotFN<- BOATnALBIES_eez %>%
  filter(Dist2Boat<=30)%>%
    group_by(Territory1,FishingYN) %>%
    summarise(n=n())
BOATnALBIES_eez_summary<-left_join(TotFN,Tot,by="Territory1")
BOATnALBIES_eez_summary$Percent<-BOATnALBIES_eez_summary$n/BOATnALBIES_eez_summary$nTot


head(BOATnALBIES_eez_summary)
sum(BOATnALBIES_eez_summary$Percent)

terrs<-(unique(BOATnALBIES_eez_summary$Territory1))

pIES<-list()
for (i in 1:7)
  local({
i<-i
    Terr<-terrs[i]
p1<-ggplotGrob(ggplot(data = BOATnALBIES_eez_summary%>%filter(Territory1==Terr), mapping = aes(x = "", y = Percent, fill = FishingYN)) + 
  geom_bar(width = 1, stat = "identity") + 
  scale_y_continuous(breaks = round(cumsum(rev(BOATnALBIES_eez_summary$Percent)), 1)) +
  coord_polar("y", start = 0)+
    scale_fill_manual("legend", values = c("Y" = "white", "N" = "black"))+
  theme_bw() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid   = element_blank(),
    axis.ticks   = element_blank(),
    axis.text    = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    
  ))
pIES[[i]] <<- p1

})
print(pIES[[7]])

# Use the latitude and longitude maxima and minima from the map to calculate the coordinates of each site location on a scale of 0 to 1, within the map panel.

map<-ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("blue", "steelblue2","lightblue"),name="Depth") +
  #geom_path(data = alllocsG[alllocsG$species=="STAL",], aes(x = wrap360(lon), y = lat, group = ID),  color=pal1[2], size = 0.1) +
  #geom_path(data = alllocsG[alllocsG$species=="LAAL",], aes(x = wrap360(lon), y = lat, group = ID), color=pal1[3], size = 0.1) +
  #geom_path(data = alllocsG[alllocsG$species=="BFAL",], aes(x = wrap360(lon), y = lat, group = (ID)), color=pal1[5], size = 0.1) +
  geom_path(data=NPeez,aes(x=wrap360(long),y=lat,group=group),size = 0.3)+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="gray40",color="grey60",size=0.1)+
  #geom_point(data = fortify(BOATnALBIES_eez)%>%dplyr::filter(Dist2Boat<=3), aes(x = wrap360(lon), y = lat,color = as.factor(flag)),size = 0.5) +
  #species labels
  #geom_point(aes(x=128,y=64),colour=pal1[2], size = 3)+
  #geom_point(aes(x=128,y=62),colour=pal1[3], size = 3)+
  #geom_point(aes(x=128,y=60),colour=pal1[5], size = 3)+
  geom_point(aes(x=140,y=64),colour="white", size = 2)+
  geom_point(aes(x=140,y=62),colour="black", size = 2)+
  #annotate("text",x=wrap360(131),y=64,label="STAL",color="white",size=3,hjust = 0)+
  #annotate("text",x=wrap360(131),y=62,label="LAAL",color="white",size=3,hjust = 0)+
  #annotate("text",x=wrap360(131),y=60,label="BFAL",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(143),y=64,label="fishing",color="white",size=3,hjust = 0)+
  annotate("text",x=wrap360(143),y=62,label="not fishing",color="white",size=3,hjust = 0)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  guides(fill=FALSE)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  theme(legend.title=element_blank())
  
terrs
piecoords<-data_frame(terrs<-terrs,
                      lat=c(56.8,48,#Ak, Can
                            15,30,#HI,Jap
                            57,35,#Russ,US
                            30),#NA
                      lon=c(-160,-130,
                            -170,131,
                            165,-125,
                            180))#NA
quartz(height=4,width=8)
map + 
annotation_custom(grob = pIES[[1]], #Alaska
                    xmin = wrap360(piecoords$lon[1]), 
                    xmax = wrap360(piecoords$lon[1])+15, 
                    ymin = piecoords$lat[1], 
                    ymax = piecoords$lat[1]+15)+
  annotation_custom(grob = pIES[[2]],
                    xmin = wrap360(piecoords$lon[2]), 
                    xmax = wrap360(piecoords$lon[2])+15, 
                    ymin = piecoords$lat[2], 
                    ymax = piecoords$lat[2]+15)+
  annotation_custom(grob = pIES[[3]],
                    xmin = wrap360(piecoords$lon[3]), 
                    xmax = wrap360(piecoords$lon[3])+15, 
                    ymin = piecoords$lat[3], 
                    ymax = piecoords$lat[3]+15)+
  annotation_custom(grob = pIES[[4]],
                    xmin = wrap360(piecoords$lon[4]), 
                    xmax = wrap360(piecoords$lon[4])+15, 
                    ymin = piecoords$lat[4], 
                    ymax = piecoords$lat[4]+15)+
  annotation_custom(grob = pIES[[5]],
                    xmin = wrap360(piecoords$lon[5]), 
                    xmax = wrap360(piecoords$lon[5])+15, 
                    ymin = piecoords$lat[5], 
                    ymax = piecoords$lat[5]+15)+
  annotation_custom(grob = pIES[[6]],
                    xmin = wrap360(piecoords$lon[6]), 
                    xmax = wrap360(piecoords$lon[6])+15, 
                    ymin = piecoords$lat[6], 
                    ymax = piecoords$lat[6]+15)+
  annotation_custom(grob = pIES[[7]],
                    xmin = wrap360(piecoords$lon[7]), 
                    xmax = wrap360(piecoords$lon[7])+15, 
                    ymin = piecoords$lat[7], 
                    ymax = piecoords$lat[7]+15)
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_2012to2016_HR_BoatEncounters_1day_30km_FishingYN.jpeg"), dpi=300)


# Pie Plots FishingYN by gear and flag ------------------------------------

#Fishing Y and N
Tot<- BOATnALBIES_eez %>% group_by(Territory1)%>%
  summarise(nTot=n())
TotGT<- BOATnALBIES_eez %>%
  group_by(Territory1,geartype) %>%
  summarise(n=n())
GT<-left_join(TotGT,Tot,by="Territory1")
GT$Percent<-GT$n/GT$nTot

quartz()
ggplot(data = GT, mapping = aes(x = "", y = Percent, fill = geartype)) + 
                 geom_bar(width = 1, stat = "identity") + 
                 scale_y_continuous(breaks = round(cumsum(rev(GT$Percent)), 1)) +
                 coord_polar("y", start = 0)+theme_bw() +facet_wrap(~Territory1)+
                  theme(axis.title.x = element_blank(), 
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text = element_blank(),
                        panel.grid   = element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_2012to2016_HR_BoatEncounters_1day_30km_FishingYN_geartype.jpeg"), dpi=300)


#Fishing Y                 
Tot<- BOATnALBIES_eez %>% filter(FishingYN=="Y")%>%
  group_by(Territory1)%>%
  summarise(nTot=n())  
TotGT<- BOATnALBIES_eez %>%filter(FishingYN=="Y")%>%
  group_by(Territory1,geartype) %>%
  summarise(n=n())
GT<-left_join(TotGT,Tot,by="Territory1")
GT$Percent<-GT$n/GT$nTot

ggplot(data = GT, mapping = aes(x = "", y = Percent, fill = geartype)) + 
  geom_bar(width = 1, stat = "identity") + 
  scale_y_continuous(breaks = round(cumsum(rev(GT$Percent)), 1)) +
  coord_polar("y", start = 0)+theme_bw() +facet_wrap(~Territory1)+
  theme(axis.title.x = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid   = element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_2012to2016_HR_BoatEncounters_1day_30km_FishingY_geartype.jpeg"), dpi=300)

#Fishing N                 
Tot<- BOATnALBIES_eez %>% filter(FishingYN=="N")%>%
  group_by(Territory1)%>%
  summarise(nTot=n())
TotGT<- BOATnALBIES_eez %>%filter(FishingYN=="N")%>%
  group_by(Territory1,geartype) %>%
  summarise(n=n())
GT<-left_join(TotGT,Tot,by="Territory1")
GT$Percent<-GT$n/GT$nTot

ggplot(data = GT, mapping = aes(x = "", y = Percent, fill = geartype)) + 
  geom_bar(width = 1, stat = "identity") + 
  scale_y_continuous(breaks = round(cumsum(rev(GT$Percent)), 1)) +
  coord_polar("y", start = 0)+theme_bw() +facet_wrap(~Territory1)+
  theme(axis.title.x = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid   = element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_2012to2016_HR_BoatEncounters_1day_30km_FishingN_geartype.jpeg"), dpi=300)


#Fishing Y and N
Tot<- BOATnALBIES_eez %>%
  group_by(Territory1)%>%
  summarise(nTot=n())
TotGT<- BOATnALBIES_eez %>%
  group_by(Territory1,flag) %>%
  summarise(n=n())
GT<-left_join(TotGT,Tot,by="Territory1")
GT$Percent<-GT$n/GT$nTot

ggplot(data = GT, mapping = aes(x = "", y = Percent, fill = flag)) + 
  geom_bar(width = 1, stat = "identity") + 
  scale_y_continuous(breaks = round(cumsum(rev(GT$Percent)), 1)) +
  coord_polar("y", start = 0)+theme_bw() +facet_wrap(~Territory1)+
  theme(axis.title.x = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid   = element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_2012to2016_HR_BoatEncounters_1day_30km_FishingYN_flag.jpeg"), dpi=300)


#Fishing Y                 
Tot<- BOATnALBIES_eez %>% filter(FishingYN=="Y")%>%
  group_by(Territory1)%>%
  summarise(nTot=n())
TotGT<- BOATnALBIES_eez %>%filter(FishingYN=="Y")%>%
  group_by(Territory1,flag) %>%
  summarise(n=n())
GT<-left_join(TotGT,Tot,by="Territory1")
GT$Percent<-GT$n/GT$nTot

ggplot(data = GT, mapping = aes(x = "", y = Percent, fill = flag)) + 
  geom_bar(width = 1, stat = "identity") + 
  scale_y_continuous(breaks = round(cumsum(rev(GT$Percent)), 1)) +
  coord_polar("y", start = 0)+theme_bw() +facet_wrap(~Territory1) +
  theme(axis.title.x = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid   = element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_2012to2016_HR_BoatEncounters_1day_30km_FishingY_flag.jpeg"), dpi=300)

#Fishing N                 
Tot<- BOATnALBIES_eez %>% filter(FishingYN=="N")%>%
  group_by(Territory1)%>%
  summarise(nTot=n())
TotGT<- BOATnALBIES_eez %>%filter(FishingYN=="N")%>%
  group_by(Territory1,flag) %>%
  summarise(n=n())
GT<-left_join(TotGT,Tot,by="Territory1")
GT$Percent<-GT$n/GT$nTot

ggplot(data = GT, mapping = aes(x = "", y = Percent, fill = flag)) + 
  geom_bar(width = 1, stat = "identity") + 
  scale_y_continuous(breaks = round(cumsum(rev(GT$Percent)), 1)) +
  coord_polar("y", start = 0)+theme_bw() +facet_wrap(~Territory1) +
  theme(axis.title.x = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid   = element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_GPSdata_2012to2016_HR_BoatEncounters_1day_30km_FishingN_flag.jpeg"), dpi=300)



