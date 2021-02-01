library(dplyr)
library(ggplot2)
library(lubridate)
library(rworldmap)
library(marmap)
library(maptools)
library("tools") # for file_path_sans_ext()
library(sp)
library(rgdal)
library(raster)
library(adehabitatHR)
library(SDMTools)
library(maptools)
library(marmap)
library(geoR)
library(adehabitatMA)
library(viridis)
library(gridExtra)
library(stringr)
library(gganimate)
library(RColorBrewer)
library(cowplot)
library(magick)

#RACHAEL
if(Sys.info()[7]=="rachaelorben") {
  GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/"
  userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'}
if(Sys.info()[7]=="torresle") {
  GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/"
  userdir<-'C:/Users/leigh.torres/Dropbox (HMSC - OSU)/seabirds/GlobalFishingWatch'}

# ggplot basics -----------------------------------------------------------
bathy2<-readRDS(paste0(userdir,"/Analysis/compileddata/Bathymetryforggplot.rda"))

w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("USA","Russia","China","Japan","Mexico","Canada","North Korea","South Korea","Taiwan","Mongolia"),]

wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

unwrap360<-function(lon360){
  lon<-ifelse(lon360 > 180, -360 + lon360, lon360)
  return(lon)}

# EEZ data ----------------------------------------------------------------
path.eez.world.lr <- ("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Albs_STAL_OConnor_pubs/Post_fledging_tracking/data/World_EEZ_v9_20161021_HR_0_360/")
fnam.eez.world.lr <- "World_EEZ_v9_2016_HR_0_360.shp"
eez.world.lr <- readOGR(dsn = path.eez.world.lr, 
                        layer = file_path_sans_ext(fnam.eez.world.lr))

# eez.world.lr has 249 features and 15 fields
# A Large SpatialLinesDataFrame object with 249 features and 15 fields (8.4 Mb)

# Extract the EEZ:
dat.eez.usa1 <- eez.world.lr[eez.world.lr@data$GeoName == "United States Exclusive Economic Zone", ] #United States Exclusive Economic Zone  
dat.eez.usaHI <- eez.world.lr[eez.world.lr@data$GeoName == "United States Exclusive Economic Zone (Hawaii)", ] #United States Exclusive Economic Zone (Hawaii)   
dat.eez.jap <- eez.world.lr[eez.world.lr@data$GeoName == "Japanese Exclusive Economic Zone", ] #Japanese Exclusive Economic Zone
dat.eez.Kurils <- eez.world.lr[eez.world.lr@data$GeoName == "Disputed area Kuril Islands: Japan / Russia", ] #Disputed area Kuril Islands: Japan / Russia     
dat.eez.ak <- eez.world.lr[eez.world.lr@data$GeoName == "United States Exclusive Economic Zone (Alaska)", ]#United States Exclusive Economic Zone (Alaska)                                        
dat.eez.usru <- eez.world.lr[eez.world.lr@data$GeoName == "Joint regime area United States / Russia", ]# Joint regime area United States / Russia                                              
dat.eez.ru <- eez.world.lr[eez.world.lr@data$GeoName == "Russian Exclusive economic Zone", ]#Russian Exclusive economic Zone                                                       
dat.eez.ca <- eez.world.lr[eez.world.lr@data$GeoName == "Canadian Exclusive Economic Zone", ]#Russian Exclusive economic Zone                                                       
dat.eez.mx <- eez.world.lr[eez.world.lr@data$GeoName == "Mexican Exclusive Economic Zone", ]#Russian Exclusive economic Zone                                                       
rm(eez.world.lr)


# Longhurst data ----------------------------------------------------------
longhust <- readOGR(dsn = "/Users/rachaelorben/Dropbox/Envrion_Vars/LonghurstEcoregions/longhurst_v4_2010/Longhurst_world_v4_2010.shp")

# Extract the longhurst:
dat.long.alsk<- longhust[longhust@data$ProvCode== "ALSK", ] #Alaska Coastal Downwelling Zone  
dat.long.BERS <- longhust[longhust@data$ProvCode== "BERS", ] #Western Subarctic Gyre 
dat.long.CAAL <- longhust[longhust@data$ProvCode== "CCAL", ] #California Current  
dat.long.KURO<- longhust[longhust@data$ProvCode== "KURO", ] #Kuroshio Current  
dat.long.NPPF <- longhust[longhust@data$ProvCode== "NPPF", ] #North Pacific Polar Front  
dat.long.NPSW <- longhust[longhust@data$ProvCode== "NPSW", ] #Western Subarctic Gyre 
dat.long.NPTG <- longhust[longhust@data$ProvCode== "NPTG", ] #Western Subarctic Gyre 
dat.long.PSAE <- longhust[longhust@data$ProvCode== "PSAE", ] #Eastern Subarctic Gyre  
dat.long.PSAW <- longhust[longhust@data$ProvCode== "PSAW", ] #Western Subarctic Gyre 
dat.long.alsk@data$ProvDescr
dat.long.BERS@data$ProvDescr
dat.long.CAAL@data$ProvDescr
dat.long.KURO@data$ProvDescr
dat.long.NPPF@data$ProvDescr
dat.long.NPSW@data$ProvDescr
dat.long.NPTG@data$ProvDescr
dat.long.PSAE@data$ProvDescr
dat.long.PSAW@data$ProvDescr


# FAO data ----------------------------------------------------------------
FAO <- readOGR(dsn = "/Users/rachaelorben/Dropbox/Envrion_Vars/FAO_AREAS/FAO_AREAS.shp")
FAO<-FAO[FAO@data$OCEAN=="Pacific",]
# Extract the FAO:
dat.long.2<- FAO[FAO@data$FID=="2",] 
dat.long.3 <- FAO[FAO@data$FID=="3",] 
dat.long.10 <- FAO[FAO@data$FID=="10",] 
dat.long.17<- FAO[FAO@data$FID=="17",] 
dat.long.130 <- FAO[FAO@data$FID=="130",] 
dat.long.133 <- FAO[FAO@data$FID=="133",] 


# Boat & Bird data --------------------------------------------------------
if(Sys.info()[7]=="rachaelorben") {
  GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/"
  userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'}

boats<-read.csv(paste0(userdir,"/GFW/allyear_vessel_overlap.csv"))
dat1<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_TripNum_2019-06-07.rda"))  

birds<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction_lowresDen_60km2020-08-24_interpinfo.rda"))  
#birds<-readRDS(paste0(userdir,"/Analysis/compileddata/GFW_Albie_EncounterInteraction.rda"))  
birds<-left_join(birds,dat1,by="eventID")
birds$Territory1[is.na(birds$Territory1)==TRUE]<-"HighSeas"
birds$Territory1[birds$Territory1=="HighSeas" & birds$lat>50]<-"Alaska"
birds$Territory1[birds$eventID==330.0]<-"Russia"

birds$year<-year(birds$tmin_30)

PT<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_Pertrip.rds"))

alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_EEZ_lh_FAO.rds"))
names(alllocs)
unique(alllocs$species)
alllocs$month<-month(alllocs$datetime)


# US-RUSSIA boarder -------------------------------------------------------
birdssel<-birds%>%dplyr::filter(env_lat>58 & env_lat<63)%>%
  dplyr::filter(wrap360(env_lon)>175 & wrap360(env_lon)<185)
birdssel_line<-birds%>%dplyr::filter(env_lat>60.5 & env_lat<61.5)%>%
  dplyr::filter(wrap360(env_lon)>181 & wrap360(env_lon)<182.5)

birdssel_line%>%group_by(flag,length,geartype,month)%>%summarise(n=n())
ggplot()+
  geom_bar(data=birdssel_line,aes(x=geartype))


quartz()
ggplot()+
  geom_point(data=birdssel_line,aes(y=env_lat,x=wrap360(env_lon),color=flag))+
  geom_path(data = dat.eez.ak, aes(x = long, y = lat, group = group),colour = "black", size = 0.75) +
  geom_path(data=alllocs%>%dplyr::filter(year==2013)%>%dplyr::filter(month==7),
            aes(x=lon,y=lat,group=UniID,color=UniID))+
  coord_fixed(ratio=1.7,xlim = c(181,182.8),ylim=c(60.5,61.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  facet_wrap(~interactionYN)
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_STAL_USRussiaboarder_flag.png"), dpi=250)

quartz()
ggplot()+
  geom_point(data=birdssel_line,aes(y=env_lat,x=wrap360(env_lon),color=geartype))+
  geom_path(data = dat.eez.ak, aes(x = long, y = lat, group = group),colour = "black", size = 0.75) +
  coord_fixed(ratio=1.7,xlim = c(181,182.8),ylim=c(60.5,61.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  facet_wrap(~interactionYN)
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_STAL_USRussiaboarder_geartype.png"), dpi=250)

quartz()
ggplot()+
  geom_point(data=birdssel_line,aes(y=env_lat,x=wrap360(env_lon),color=geartype))+
  geom_path(data = dat.eez.ak, aes(x = long, y = lat, group = group),colour = "black", size = 0.75) +
  coord_fixed(ratio=1.7,xlim = c(181,182.8),ylim=c(60.5,61.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  facet_wrap(~interactionYN+month,ncol = 5)
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_STAL_USRussiaboarder_geartype&month.png"), dpi=250)

quartz()
ggplot()+
  geom_point(data=birdssel_line,aes(y=env_lat,x=wrap360(env_lon),color=geartype))+
  geom_path(data = dat.eez.ak, aes(x = long, y = lat, group = group),colour = "black", size = 0.75) +
  coord_fixed(ratio=1.7,xlim = c(181,182.8),ylim=c(60.5,61.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  facet_wrap(~interactionYN+year)
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_STAL_USRussiaboarder_geartype&year.png"), dpi=250)


# Figure 1: BFAL -----------------------------------------------------------------
library(wesanderson)
wes_palette("Moonrise3")
colors_gear<-wes_palette("Darjeeling1", 4, type = c("discrete"))
colors_gear<-wes_palette("Moonrise3", 5, type = c("discrete"))
"#3B9AB2"
"#78B7C5"<-tracks (ltblue)
"#EBCC2A"<-ent (yellow)
"#F21A00"<-trawler (red)
"273046"<-longliner (dkblue)
"#00A08A"<-other (tourquise)
"#FD6467"<-unk (pink)
"#9986A5"<-lavendar
birds$simpGear<-birds$geartypeCLASS
birds$simpGear[birds$geartypeCLASS=="Purse seiner"]<-"Other"
birds$simpGear[birds$geartypeCLASS=="pole_and_line"]<-"Other"
birds$simpGear[birds$geartypeCLASS=="Mixed"]<-"Other"
birds$simpGear[birds$geartypeCLASS=="pots_and_traps"]<-"Other"
birds$simpGear[birds$geartypeCLASS=="squid_jigger"]<-"Other"
birds$simpGear[birds$geartypeCLASS=="Fish_factory"]<-"Other"
birds$simpGear[birds$geartypeCLASS=="Reefer"]<-"Other"
birds$simpGear<-as.factor(birds$simpGear)
levels(birds$simpGear)
simpGear_pal<-c("#F98400","#F4B5BD","#F21A00","#9C964A")
ggplot()+geom_bar(data=birds,aes(x=geartypeCLASS))
ggplot()+geom_bar(data=birds,aes(x=simpGear,fill=simpGear))+
  facet_wrap(~species)+
  scale_discrete_manual(values = simpGear_pal,
                        aesthetics = c("colour", "fill"))

#inset dataframe
names(alllocs)
alllocs$month<-month(alllocs$datetime)
alllocs$year<-year(alllocs$datetime)
alllocs$date<-date(alllocs$datetime)
birdsMO<-unique(alllocs%>%dplyr::select("ID.x","species","date","month","year"))
birdsMO.dys<-birdsMO%>%group_by(ID.x,species,month,year)%>%summarise(n=n())
birdsMO.dys1<-birdsMO.dys%>%group_by(species, month)%>%summarise(mean=mean(n),
                                                                 sd=sd(n))

names(birds)
colourCount = nrow(unique(alllocs%>%filter(species=="BFAL")%>%dplyr::select(ID.x)))+1

getPalette = colorRampPalette(brewer.pal(9, "Set1"))
alllocs$ID<-alllocs$ID.x

quartz(height=4,width=6)
b<-ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(species=="BFAL"), 
            aes(x = lon360, y = lat,group=UniID), size = 0.2, color="turquoise")+
  geom_point(data = birds%>%filter(species=="BFAL"), color="yellow", 
            aes(x = wrap360(env_lon), y = env_lat,color=species), size = .5) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(species=="BFAL"), 
             aes(x = wrap360(env_lon), y = env_lat,color=simpGear),  size = .5) +
  scale_discrete_manual(values = simpGear_pal,
                        aesthetics = c("colour"))+
  annotate("text",wrap360(128),y=63,label="black-footed albatross\n(BFAL)",color="black",size=3,hjust = 0)+
  #annotate("text",x=wrap360(128),y=63,label="BFAL",color="black",size=7,hjust = 0)+
  scale_x_continuous(breaks=c(150,180,210,240),labels=c("150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab("")+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  theme(legend.position = "none")

inset.plot.b<-ggplot()+
  geom_bar(data=unique(birdsMO%>%dplyr::select("ID.x","species","month"))%>%
             filter(species=="BFAL"), 
           aes(x=month), fill="grey80")+
  annotate("text",10,y=36,label="BFAL",color="black",size=3,hjust = 0)+
  geom_point(data=birdsMO.dys1%>%
               filter(species=="BFAL"), 
             aes(y=mean, x=month), color="black", size=.5)+
  geom_errorbar(data=birdsMO.dys1%>%
                  filter(species=="BFAL"),
                aes(x=month, ymin=mean-sd, ymax=mean+sd), 
                width=.2,position=position_dodge(.9))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  theme_classic()+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="black", size=6))#,
        #rect = element_rect(fill = "transparent"),
        #plot.background = element_blank())

b.with.inset <-
  ggdraw() +
  draw_plot(b) +
  draw_plot(inset.plot.b, x = 0.712, y = .10, width = .25, height = .3)
b.with.inset 
#quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_BFAL.png"), dpi=250)

# Figure 1: LAAL ----------------------------------------------------------
quartz(height=4,width=6)
l<-ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(species=="LAAL"), 
            aes(x = lon360, y = lat,group=UniID), color="turquoise", size = 0.2) +
  geom_point(data = birds%>%filter(species=="LAAL"),color="yellow", 
             aes(x = wrap360(env_lon), y = env_lat,color=species),  size = .5) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(species=="LAAL"), 
             aes(x = wrap360(env_lon), y = env_lat, color=simpGear),  size = .5) +
  scale_discrete_manual(values = simpGear_pal,
                        aesthetics = c("colour"))+
  annotate("text",wrap360(128),y=63,label="Laysan albatross\n(LAAL)",color="black",size=3,hjust = 0)+
  #annotate("text",wrap360(128),y=63,label="LAAL",color="black",size=7,hjust = 0)+
  scale_x_continuous(breaks=c(150,180,210,240),labels=c("150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(legend.position = "none")

inset.plot.l<-ggplot()+
  geom_bar(data=unique(birdsMO%>%dplyr::select("ID.x","species","month"))%>%
             filter(species=="LAAL"), 
           aes(x=month), fill="grey80")+
  annotate("text",10,y=36,label="LAAL",color="black",size=3,hjust = 0)+
  geom_point(data=birdsMO.dys1%>%
               filter(species=="LAAL"), 
             aes(y=mean, x=month), color="black", size=.5)+
  geom_errorbar(data=birdsMO.dys1%>%
                  filter(species=="LAAL"),
                aes(x=month, ymin=mean-sd, ymax=mean+sd), 
                width=.2,position=position_dodge(.9))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  theme_classic()+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="black", size=6))#,
        #rect = element_rect(fill = "transparent"),
        #plot.background = element_blank())
#ggplot_build(l)$data #finds colors
l.with.inset <-
  ggdraw() +
  draw_plot(l) +
  draw_plot(inset.plot.l, x = 0.712, y = .10, width = .25, height = .3)
l.with.inset 
#quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_LAAL.png"), dpi=250)

# Figure 1: STAL ----------------------------------------------------------
s.bathy<-ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),
                       name="Depth (m)",
                       guide = guide_colorbar(barwidth = 1, barheight = 3))+
  theme(legend.text=element_text(size=6),
        legend.title = element_text(size=8))
s.bathy
s.leg.bathy <- get_legend(s.bathy)

s.boats<-ggplot()+
  geom_point(data = birds%>%filter(species=="STAL")%>%filter(interactionYN=="N"), 
             aes(x = wrap360(env_lon), y = env_lat, pch=interactionYN),color="yellow", size=2.5) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat,color=simpGear),  size = 2)+
  scale_discrete_manual(values = simpGear_pal,
                        aesthetics = c("colour"))+
  labs(colour = "Association:\nGear Type",shape = "Encounters")+
  theme(legend.position = "right",
        legend.background = element_blank(),
        legend.key=element_blank(),
        legend.spacing = unit(.002, 'cm'),
        legend.key.size = unit(.25, "cm"),
        legend.title = element_text(size=7),
        legend.text  = element_text(size=7))
s.boats
s.leg.boats <- get_legend(s.boats)

quartz(height=4.2,width=7)
s<-ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(species=="STAL"), aes(x = lon360, y = lat,group=UniID), color="turquoise", size = 0.2) +
  geom_point(data = birds%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat),color="yellow",  size = .3) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat,color=simpGear),  size = .3) +
  scale_discrete_manual(values = simpGear_pal,
                        aesthetics = c("colour"))+
  annotate("text",wrap360(128),y=63,label="short-tailed albatross\n(STAL)",color="black",size=3,hjust = 0)+
  #annotate("text",wrap360(221),y=26,label="# days",color="black",size=4,hjust = 0)+
  #annotate("text",wrap360(221),y=21,label="# of birds",color="gray80",size=4,hjust = 0)+
  scale_x_continuous(breaks=c(150,180,210,240),labels=c("150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab(expression(paste("Longitude (",degree,"E)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  theme(legend.position = "none")

s_img<-ggdraw() +
  draw_plot(s)+
  draw_image("/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch/Analysis/compileddata/stal2.png",  
             x = 0.4, y = 0.36, scale = .18) 
 s_img

inset.plot.s<-ggplot()+
  geom_bar(data=unique(birdsMO%>%dplyr::select("ID.x","species","month"))%>%
             filter(species=="STAL"), 
           aes(x=month), fill="grey80")+
  annotate("text",10,y=36,label="STAL",color="black",size=3,hjust = 0)+
  geom_point(data=birdsMO.dys1%>%
               filter(species=="STAL"), 
             aes(y=mean, x=month), color="black", size=.5)+
  geom_errorbar(data=birdsMO.dys1%>%
                  filter(species=="STAL"),
                aes(x=month, ymin=mean-sd, ymax=mean+sd), 
                width=.2,position=position_dodge(.9))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  theme_classic()+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="black", size=6),
        legend.position = "none")#,
        #rect = element_rect(fill = "transparent"),
        #plot.background = element_blank())


s.with.inset <-
  ggdraw() +
  draw_plot(s) +
  #draw_plot(inset.plot.s, x = 0.41, y = .085, width = .25, height = .3)
  draw_plot(inset.plot.l, x = 0.712, y = .10, width = .25, height = .3)
s.with.inset 
#quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_STAL.png"), dpi=250)

library(grid)
library(ggplotify)
quartz(height=5.5, width=7.5)
plots1<-plot_grid(s.leg.boats,s.leg.bathy, ncol=2)
plots2<-plot_grid(inset.plot.b,inset.plot.l,inset.plot.s, plots1,
                    labels = c('D', 'E','F'), label_size = 8, nrow=2)
plots3<-plot_grid(b,l,s, plots2,
          labels = c('A', 'B','C'), label_size = 12, nrow=2)
g<-grid.arrange(arrangeGrob(plots3))
cowplot::ggdraw(g) + 
  theme(plot.background = element_rect(fill="white", color = NA))

quartz.save(paste0(userdir,"/Analysis/PLOTS/Fig1_Albie_EncountersInteractions.png"), dpi=250)
quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_Oct2020/Figure1_Albie_EncountersInteractions.png"),dpi=600)

# STAL interaction duration w/ chla & SST ---------------------------------


quartz()
ggplot()+
  geom_point(data = birds%>%filter(species=="STAL")%>%filter(interactionYN=="Y")%>%
               filter(is.na(sst)==FALSE), 
             aes(x = wrap360(env_lon), y = env_lat, pch=interactionYN,color=sst),size=2.5) 
ggplot()+
  geom_point(data = birds%>%filter(species=="STAL")%>%filter(interactionYN=="Y")%>%
               filter(is.na(sst)==FALSE), 
             aes(x = env_lat, y = sst, color=sst),size=2.5)+
  geom_smooth(data = birds%>%filter(species=="STAL")%>%filter(interactionYN=="Y")%>%
                filter(is.na(sst)==FALSE), 
              aes(x = env_lat, y = sst, color=sst))

quartz()
ggplot()+
  geom_point(data = birds%>%filter(species=="STAL")%>%filter(interactionYN=="Y")%>%
               filter(is.na(chla)==FALSE), 
             aes(x = wrap360(env_lon), y = env_lat, pch=interactionYN,color=log(chla)),size=2.5) 
ggplot()+
  geom_point(data = birds%>%filter(species=="STAL")%>%filter(interactionYN=="Y")%>%
               filter(is.na(sst)==FALSE), 
             aes(x = env_lat, y = log(chla), color=log(chla)),size=2.5)+
  geom_smooth(data = birds%>%filter(species=="STAL")%>%filter(interactionYN=="Y")%>%
                filter(is.na(chla)==FALSE), 
              aes(x = env_lat, y = log(chla), color=log(chla)))

# Graphical Abstract ------------------------------------------------------
ru<-fortify(dat.eez.ru)%>%filter(hole==TRUE)%>%
  filter(long>140)%>%filter(lat<60)%>%
  filter(long<150)

quartz(height=6, width=9)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey80", "grey20"),name="Depth") +
  geom_path(data = alllocs, aes(x = lon360, y = lat,group=UniID, color=species), size = 0.2) +
  geom_point(data = birds, 
             aes(x = wrap360(env_lon), y = env_lat),color="yellow",  size = .3) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat,color=simpGear),  size = .3) +
  geom_path(data = fortify(dat.eez.usa1)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "black", size = .5) +
  geom_path(data = fortify(dat.eez.usaHI)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "black", size = .5) +
  geom_path(data = fortify(dat.eez.jap)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "black", size = .5) +
  geom_path(data = fortify(dat.eez.Kurils)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "black", size = .5) +
  geom_path(data = fortify(dat.eez.ak)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "black", size = .5) +
  geom_path(data = fortify(dat.eez.usru)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "black", size = .5) +
  geom_path(data = fortify(dat.eez.ru)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "black", size = .5)+
  geom_path(data = ru, aes(x = long, y = lat, group = group), colour = "black",size = .5)+
  geom_path(data = fortify(dat.eez.ca)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "black", size = .5) +
  geom_path(data = fortify(dat.eez.mx)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "black", size = .5)+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey90",color="grey60",size=0.1)+
  #scale_discrete_manual(values = simpGear_pal,
  #                      aesthetics = c("colour"))+
  #annotate("text",wrap360(128),y=63,label="short-tailed albatross\n(STAL)",color="black",size=3,hjust = 0)+
  #annotate("text",wrap360(221),y=26,label="# days",color="black",size=4,hjust = 0)+
  #annotate("text",wrap360(221),y=21,label="# of birds",color="gray80",size=4,hjust = 0)+
  scale_x_continuous(breaks=c(150,180,210,240),labels=c("150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab(expression(paste("Longitude (",degree,"E)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size=4, color="grey90"),
        axis.text =element_text(size=4, color="grey90"),
        rect = element_rect(fill = "transparent"),
        plot.background = element_blank())

quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_April2020/plots/Allspp_Albie_EncountersInteractions.tiff"),dpi=300)

# Appendix: Longhurst -----------------------------------------------------
dat.BERS<-fortify(dat.long.BERS)
dat.BERS$group<-as.character(dat.BERS$group)
dat.BERS$group[1:52]<-"29.0"
dat.BERS$group[4053:4450]<-"28"

dat.PSAW<-fortify(dat.long.PSAW)
dat.PSAW$group<-as.character(dat.PSAW$group)
dat.PSAW$group[1]<-"31.0"
dat.PSAW$group[395:400]<-"31.05"

dat.NPPF<-fortify(dat.long.NPPF)
dat.NPPF$group<-as.character(dat.NPPF$group)
dat.NPPF$group[72:120]<-"33.0"
dat.NPPF$group[158:182]<-"33.05"

dat.NPSW<-fortify(dat.long.NPSW)
dat.NPSW$group<-as.character(dat.NPSW$group)
dat.NPSW$group[1]<-"34.0"
dat.NPSW$group[227:238]<-"34.25"

quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey85", "grey15"),name="Depth (m)",
                       guide = guide_colorbar(barwidth = 1, barheight = 5)) +
  geom_path(data = fortify(dat.long.alsk)%>%filter(hole==FALSE), aes(x = wrap360(long), y = lat,group = group),
            color="deepskyblue",size = 0.5) +
  geom_path(data = dat.BERS%>%filter(hole==FALSE), 
            aes(x = wrap360(long), y = lat,group = group),color="deepskyblue",size = 0.5) +
  geom_path(data = fortify(dat.long.CAAL)%>%filter(hole==FALSE), aes(x = wrap360(long), y = lat,group = group), 
            color="deepskyblue",size = 0.5) +
  geom_path(data = fortify(dat.long.KURO)%>%filter(hole==FALSE), aes(x = wrap360(long), y = lat, group = group),
            colour = "deepskyblue", size = 0.5) +
  geom_path(data = dat.NPPF%>%filter(hole==FALSE), aes(x = wrap360(long), y = lat, group = group),
            color="deepskyblue",size = 0.5) +
  geom_path(data = dat.NPSW%>%filter(hole==FALSE), aes(x = wrap360(long), y = lat, group = group),
            color="deepskyblue",size = 0.5) +
  geom_path(data = fortify(dat.long.NPTG)%>%filter(hole==FALSE), aes(x = wrap360(long), y = lat, group = group),
            colour = "deepskyblue", size = 0.5) +
  geom_path(data = fortify(dat.long.PSAE)%>%filter(hole==FALSE), aes(x = wrap360(long), y = lat, group = group),
            colour = "deepskyblue", size = 0.5) +
  geom_path(data = dat.PSAW%>%filter(hole==FALSE), aes(x = wrap360(long), y = lat,group = group),
            colour = "deepskyblue", size = 0.5) +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="black",color="grey60",size=0.1)+
  annotate("text",x=wrap360(-146.5),y=59.5,label="ALSK",color="darkorange",size=3,hjust = 0)+
  annotate("text",x=wrap360(-173),y=39,label="NPPF",color="darkorange",size=3,hjust = 0)+
  annotate("text",x=wrap360(144),y=38,label="KURO",color="darkorange",size=3,hjust = 0)+
  annotate("text",x=wrap360(178),y=58,label="BERS",color="darkorange",size=3,hjust = 0)+
  annotate("text",x=wrap360(144),y=54,label="BERS",color="darkorange",size=3,hjust = 0)+
  annotate("text",x=wrap360(-130),y=45,label="CCAL",color="darkorange",size=3,hjust = 0)+
  annotate("text",x=wrap360(170),y=48,label="PSAW",color="darkorange",size=3,hjust = 0)+
  annotate("text",x=wrap360(-160),y=50,label="PSAE",color="darkorange",size=3,hjust = 0)+
  annotate("text",x=wrap360(160),y=25,label="NPSW",color="darkorange",size=3,hjust = 0)+
  annotate("text",x=wrap360(-150),y=25,label="NPTG",color="darkorange",size=3,hjust = 0)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("125", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab(expression(paste("Longitude (",degree,"E)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()
quartz.save(paste0(userdir,"/Analysis/PLOTS/Appendix_LonghurstEcoregions.png"), dpi=250)
quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_April2020/plots/Appendix_LonghurstEcoregions.png"),dpi=1000)


# Appendix: EEZ -----------------------------------------------------------
ru<-fortify(dat.eez.ru)%>%filter(hole==TRUE)%>%
  filter(long>140)%>%filter(lat<60)%>%
  filter(long<150)


#ru%>%group_by(group)%>%summarise(n=n())

quartz(height=4,width=6)
map1<-ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey85", "grey15"),name="Depth (m)",
                       guide = guide_colorbar(barwidth = 1, barheight = 5)) +
  geom_path(data = alllocs, aes(x = lon360, y = lat,color=ID, group=UniID),colour = "grey70", size = .1)+
  geom_path(data = fortify(dat.eez.usa1)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "deepskyblue", size = .5) +
  geom_path(data = fortify(dat.eez.usaHI)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "deepskyblue", size = .5) +
  geom_path(data = fortify(dat.eez.jap)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "deepskyblue", size = .5) +
  geom_path(data = fortify(dat.eez.Kurils)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "deepskyblue", size = .5) +
  geom_path(data = fortify(dat.eez.ak)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "deepskyblue", size = .5) +
  geom_path(data = fortify(dat.eez.usru)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "deepskyblue", size = .5) +
  geom_path(data = fortify(dat.eez.ru)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "deepskyblue", size = .5)+
  geom_path(data = ru, aes(x = long, y = lat, group = group), colour = "deepskyblue",size = .5)+
  geom_path(data = fortify(dat.eez.ca)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "deepskyblue", size = .5) +
  geom_path(data = fortify(dat.eez.mx)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "deepskyblue", size = .5)+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="black",color="black",size=.5)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("125", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab(expression(paste("Longitude (",degree,"E)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()

  map1+
    annotate("text",x=wrap360(-146.5),y=61,label="USA",color="darkorange",size=3,hjust = 0)+
    annotate("text",x=wrap360(-173),y=26,label="USA",color="darkorange",size=3,hjust = 0)+
    annotate("text",x=wrap360(130),y=33,label="Japan",color="darkorange",size=3,hjust = 0)+
    annotate("text",x=wrap360(155),y=55,label="Russia",color="darkorange",size=3,hjust = 0)+
    annotate("text",x=wrap360(-130),y=52,label="Canada",color="darkorange",size=3,hjust = 0)+
    annotate("text",x=wrap360(-121.5),y=28,label="Mexico",color="darkorange",size=3,hjust = 0)+
    annotate("text",x=wrap360(-125),y=40,label="USA",color="darkorange",size=3,hjust = 0)
    
quartz.save(paste0(userdir,"/Analysis/PLOTS/Appendix_ExclusiveEconomicZones.png"), dpi=250)
quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_April2020/plots/Appendix_ExclusiveEconomicZones.png"),dpi=1000)


# Appendix: FAO-----------------------------------------------------------
dat.2<-fortify(dat.long.2)
dat.2$group<-as.character(dat.2$group)
dat.2$group[5004:5059]<-"0"
dat.2$group[5059:5284]<-"17.201"

dat.130<-fortify(dat.long.130)
dat.130$group<-as.character(dat.130$group)
dat.130$group[92:128]<-"0"


quartz(height=4,width=6)
map2<-ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey85", "grey15"),name="Depth (m)",
                       guide = guide_colorbar(barwidth = 1, barheight = 5)) +
  geom_path(data = alllocs, aes(x = lon360, y = lat,color=ID, group=UniID),colour = "grey90", size = .1)+
  geom_path(data = dat.2%>%filter(hole==FALSE)%>%filter(group!="0")%>%filter(group!="17.1"), 
            aes(x = wrap360(long), y = lat, group=group), colour = "deepskyblue", size = .5) +
  geom_path(data = fortify(dat.long.3)%>%filter(hole==FALSE), aes(x = wrap360(long), y = lat),colour = "deepskyblue", size = .5) +
  #geom_path(data = fortify(dat.long.10)%>%filter(hole==FALSE), aes(x = wrap360(long), y = lat),colour = "deepskyblue", size = .5) +
  geom_path(data = fortify(dat.long.17)%>%filter(hole==FALSE), aes(x = wrap360(long), y = lat),colour = "deepskyblue", size = .5) +
  geom_path(data = fortify(dat.long.133)%>%filter(hole==FALSE), aes(x = wrap360(long), y = lat),colour = "deepskyblue", size = .5) +
  geom_path(data = dat.130%>%filter(hole==FALSE)%>%filter(group!="0"), 
            aes(x = wrap360(long), y = lat,  group=group), colour = "deepskyblue", size = .5) +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="black",color="grey55",size=.3)+
  scale_x_continuous(breaks=c(150,180,210,240),labels=c("150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(135,245),ylim=c(19,65))+
  xlab(expression(paste("Longitude (",degree,"E)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()

map2+
  annotate("text",x=wrap360(-160),y=45,label="67",color="darkorange",size=8,hjust = 0)+
  annotate("text",x=wrap360(160),y=45,label="61",color="darkorange",size=8,hjust = 0)+
  annotate("text",x=wrap360(-160),y=30,label="77",color="darkorange",size=8,hjust = 0)

quartz.save(paste0(userdir,"/Analysis/PLOTS/Appendix_FAO.png"), dpi=300)
quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_April2020/plots/Appendix_FAO.png"),dpi=1000)

# Appendix: STAL interactions ---------------------------------------------------------

tracks_stal<-birds%>%filter(time3_min>0)%>%filter(species=="STAL")
tracks_stal$lon360<-wrap360(tracks_stal$env_lon)
tracks_stal$totono<-1
# Make a spatial Points Dataframe
tracks_sp <- SpatialPointsDataFrame(coords=cbind(tracks_stal$env_lon,tracks_stal$env_lat),
                                    data=as.data.frame(tracks_stal),
                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))

# Make a raster with the count of birds in each cell
# Set the grid cell resolution for your out raster (we are in degrees in this example)
res<-1

# Make a raster with the same extent as as your SPDF above with a buffer in the
#  units that the spdf is in
e<-raster::extent(raster::buffer(tracks_sp,width=res))

# Make a raster from the extent with a resolution in the units you have
aa<-raster::raster(e,resolution=res)

stal_In<-raster::rasterize(x =tracks_sp[!is.na(tracks_sp[["totono"]]),],
                           y = aa,
                           field="totono",
                           fun=function(x,...) sum(x))

# change to a dataframe for ggplot (this might explode if you have a ton of cells)
stal_In.df<-raster::as.data.frame(stal_In,xy=T)

col.range=c(1,max(stal_In.df$layer, na.rm=TRUE)+2)
quartz(height=4,width=6)
ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey20",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(species=="STAL"), 
            aes(x = lon360, y = lat,group=UniID), color="grey", size = 0.3) +
  geom_tile(data=stal_In.df %>% filter(),
            aes(x=wrap360(x),y=y,fill=layer))+
  scale_fill_viridis(na.value = NA, option="magma",begin = .1,end=.90,limits=col.range,
                     guide = guide_colorbar(barwidth = 1, barheight = 5))+
  geom_text(data=stal_In.df,aes(x=wrap360(x),y=y,label=layer),size=1,color="white")+
   scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab(expression(paste("Longitude (",degree,"E)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  labs(fill = "Association #")+
  theme_bw()
quartz.save(paste0(userdir,"/Analysis/PLOTS/Appendix_Albie_Interactions_STAL_gridded.png"), dpi=250)
quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_Oct2020/SupplimentaryFiles/Appendix_Albie_Interactions_STAL_gridded.png"),dpi=600)

tracks_stal<-birds%>%filter(species=="STAL")
tracks_stal$lon360<-wrap360(tracks_stal$env_lon)
tracks_stal$totono<-1
# Make a spatial Points Dataframe
tracks_sp <- SpatialPointsDataFrame(coords=cbind(tracks_stal$env_lon,tracks_stal$env_lat),
                                    data=as.data.frame(tracks_stal),
                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))

# Make a raster with the count of birds in each cell
# Set the grid cell resolution for your out raster (we are in degrees in this example)
res<-.2

# Make a raster with the same extent as as your SPDF above with a buffer in the
#  units that the spdf is in
e<-raster::extent(raster::buffer(tracks_sp,width=res))

# Make a raster from the extent with a resolution in the units you have
aa<-raster::raster(e,resolution=res)

stal_In<-raster::rasterize(x =tracks_sp[!is.na(tracks_sp[["totono"]]),],
                           y = aa,
                           field="totono",
                           fun=function(x,...) sum(x))

# change to a dataframe for ggplot (this might explode if you have a ton of cells)
stal_In.df<-raster::as.data.frame(stal_In,xy=T)

col.range=c(1,max(stal_In.df$layer))
quartz(height=4,width=6)
ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(species=="STAL"), aes(x = lon360, y = lat,group=UniID), color="grey", size = 0.3) +# color="turquoise",
  scale_color_manual(values = getPalette(colourCount))+
  #geom_point(data = birds%>%filter(species=="STAL"), 
  #           aes(x = wrap360(env_lon), y = env_lat), color="green", shape=12,size = 1.5) +
  geom_tile(data=stal_In.df %>% filter(),
            aes(x=wrap360(x),y=y,fill=layer))+
  scale_fill_viridis(na.value = NA, limits=col.range)+
  geom_text(data=stal_In.df,aes(x=wrap360(x),y=y,label=layer),size=1,color="white")+
  #scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(175,185),ylim=c(55,65))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_Encounters_STAL_gridded.png"), dpi=250)
max(stal_In.df$layer, na.rm = TRUE)

# Appendix: Gulf of Alaska interactions ---------------------------------------------------------
birds$lon360<-wrap360(birds$env_lon)
tracks_GA<-birds%>%filter(lon360>197)%>%
  filter(lon360<215)%>%
  filter(env_lat>50)%>%
  filter(tmin_3>0)

tracks_GA%>%dplyr::group_by(species)%>%dplyr::summarise(n=n())

tracks_GA$totono<-1
# Make a spatial Points Dataframe
tracks_sp <- SpatialPointsDataFrame(coords=cbind(tracks_GA$env_lon,tracks_GA$env_lat),
                                    data=as.data.frame(tracks_GA),
                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))

# Make a raster with the count of birds in each cell
# Set the grid cell resolution for your out raster (we are in degrees in this example)
res<-.5

# Make a raster with the same extent as as your SPDF above with a buffer in the
#  units that the spdf is in
e<-raster::extent(raster::buffer(tracks_sp,width=res))

# Make a raster from the extent with a resolution in the units you have
aa<-raster::raster(e,resolution=res)

tracks_GA<-raster::rasterize(x =tracks_sp[!is.na(tracks_sp[["totono"]]),],
                           y = aa,
                           field="totono",
                           fun=function(x,...) sum(x))

# change to a dataframe for ggplot (this might explode if you have a ton of cells)
tracks_GA.df<-raster::as.data.frame(tracks_GA,xy=T)

col.range=c(1,max(tracks_GA.df$layer, na.rm=TRUE)+2)
quartz(height=4,width=6)
ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey20",color="grey60",size=0.1)+
  geom_path(data = alllocs,
            aes(x = lon360, y = lat,group=UniID, color=species), size = 0.3) +
  geom_tile(data=tracks_GA.df %>% filter(),
            aes(x=wrap360(x),y=y,fill=layer))+
  scale_fill_viridis(na.value = NA, option="magma",begin = .1,end=.90,limits=col.range)+
  geom_text(data=tracks_GA.df,aes(x=wrap360(x),y=y,label=layer),size=1,color="white")+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(198,215),ylim=c(50,60))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill = "Interaction #")+
  theme_bw()
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_Interactions_GA_gridded_05.png"), dpi=250)

# Appendix: Kuril interactions ---------------------------------------------------------
birds$lon360<-wrap360(birds$env_lon)
tracks_K<-birds%>%
  filter(lon360<160)%>%
  filter(env_lat>45)%>%
  filter(env_lat<55)%>%
  filter(tmin_3>0)

tracks_K%>%dplyr::group_by(species)%>%dplyr::summarise(n=n())

tracks_K$totono<-1
# Make a spatial Points Dataframe
tracks_sp <- SpatialPointsDataFrame(coords=cbind(tracks_K$env_lon,tracks_K$env_lat),
                                    data=as.data.frame(tracks_K),
                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))

# Make a raster with the count of birds in each cell
# Set the grid cell resolution for your out raster (we are in degrees in this example)
res<-.5

# Make a raster with the same extent as as your SPDF above with a buffer in the
#  units that the spdf is in
e<-raster::extent(raster::buffer(tracks_sp,width=res))

# Make a raster from the extent with a resolution in the units you have
aa<-raster::raster(e,resolution=res)

tracks_K<-raster::rasterize(x =tracks_sp[!is.na(tracks_sp[["totono"]]),],
                             y = aa,
                             field="totono",
                             fun=function(x,...) sum(x))

# change to a dataframe for ggplot (this might explode if you have a ton of cells)
tracks_K.df<-raster::as.data.frame(tracks_K,xy=T)

col.range=c(1,max(tracks_K.df$layer, na.rm=TRUE)+2)
quartz(height=4,width=6)
ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey20",color="grey60",size=0.1)+
  geom_path(data = alllocs,
            aes(x = lon360, y = lat,group=UniID, color=species), size = 0.3) +
  geom_tile(data=tracks_K.df %>% filter(),
            aes(x=wrap360(x),y=y,fill=layer))+
  scale_fill_viridis(na.value = NA, option="magma",begin = .1,end=.90,limits=col.range)+
  geom_text(data=tracks_K.df,aes(x=wrap360(x),y=y,label=layer),size=1,color="white")+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(140,160),ylim=c(45,55))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill = "Interaction #")+
  theme_bw()
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_Interactions_Kurils_gridded_05.png"), dpi=250)

# Appendix: Unimak interactions ---------------------------------------------------------
birds$lon360<-wrap360(birds$env_lon)
tracks_U<-birds%>%
  filter(lon360>190)%>%
  filter(lon360<198)%>%
  filter(env_lat>52)%>%
  filter(env_lat<56)%>%
  filter(tmin_3>0)

tracks_U%>%dplyr::group_by(species)%>%dplyr::summarise(n=n())

tracks_U$totono<-1
# Make a spatial Points Dataframe
tracks_sp <- SpatialPointsDataFrame(coords=cbind(tracks_U$env_lon,tracks_U$env_lat),
                                    data=as.data.frame(tracks_U),
                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))

# Make a raster with the count of birds in each cell
# Set the grid cell resolution for your out raster (we are in degrees in this example)
res<-.2

# Make a raster with the same extent as as your SPDF above with a buffer in the
#  units that the spdf is in
e<-raster::extent(raster::buffer(tracks_sp,width=res))

# Make a raster from the extent with a resolution in the units you have
aa<-raster::raster(e,resolution=res)

tracks_U<-raster::rasterize(x =tracks_sp[!is.na(tracks_sp[["totono"]]),],
                            y = aa,
                            field="totono",
                            fun=function(x,...) sum(x))

# change to a dataframe for ggplot (this might explode if you have a ton of cells)
tracks_U.df<-raster::as.data.frame(tracks_U,xy=T)

col.range=c(1,max(tracks_U.df$layer, na.rm=TRUE)+2)
quartz(height=4,width=6)
ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey20",color="grey60",size=0.1)+
  geom_path(data = alllocs,
            aes(x = lon360, y = lat,group=UniID, color=species), size = 0.3) +
  geom_tile(data=tracks_U.df %>% filter(),
            aes(x=wrap360(x),y=y,fill=layer))+
  scale_fill_viridis(na.value = NA, option="magma",begin = .1,end=.90,limits=col.range)+
  geom_text(data=tracks_U.df,aes(x=wrap360(x),y=y,label=layer),size=1,color="white")+
  #scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(190,198),ylim=c(52,56))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill = "Interaction #")+
  theme_bw()
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_Interactions_Unimak_gridded_02.png"), dpi=250)

# Appendix: Northern Bering interactions ---------------------------------------------------------
birds$lon360<-wrap360(birds$env_lon)
tracks_NB<-birds%>%
  filter(env_lat>56)%>%
  filter(lon360<190)%>%
  filter(lon360>170)%>%
  filter(tmin_3>0)

tracks_NB%>%dplyr::group_by(species)%>%dplyr::summarise(n=n())

tracks_NB$totono<-1
# Make a spatial Points Dataframe
tracks_sp <- SpatialPointsDataFrame(coords=cbind(tracks_NB$env_lon,tracks_NB$env_lat),
                                    data=as.data.frame(tracks_NB),
                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))

# Make a raster with the count of birds in each cell
# Set the grid cell resolution for your out raster (we are in degrees in this example)
res<-.2

# Make a raster with the same extent as as your SPDF above with a buffer in the
#  units that the spdf is in
e<-raster::extent(raster::buffer(tracks_sp,width=res))

# Make a raster from the extent with a resolution in the units you have
aa<-raster::raster(e,resolution=res)

tracks_NB<-raster::rasterize(x =tracks_sp[!is.na(tracks_sp[["totono"]]),],
                            y = aa,
                            field="totono",
                            fun=function(x,...) sum(x))

# change to a dataframe for ggplot (this might explode if you have a ton of cells)
tracks_NB.df<-raster::as.data.frame(tracks_NB,xy=T)

col.range=c(1,max(tracks_NB.df$layer, na.rm=TRUE)+2)
quartz(height=4,width=6)
ggplot()+
  geom_path(data = fortify(dat.eez.ak)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "grey50", size = .5) +
  geom_path(data = fortify(dat.eez.usaHI)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "grey50", size = .5) +
  geom_path(data = fortify(dat.eez.ru)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "grey50", size = .5)+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey20",color="grey60",size=0.1)+
  geom_path(data = alllocs,
            aes(x = lon360, y = lat,group=UniID, color=species), size = 0.3) +
  geom_tile(data=tracks_NB.df %>% filter(),
            aes(x=wrap360(x),y=y,fill=layer))+
  scale_fill_viridis(na.value = NA, option="magma",begin = .1,end=.90,limits=col.range)+
  geom_text(data=tracks_NB.df,aes(x=wrap360(x),y=y,label=layer),size=1,color="white")+
  #scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(170,190),ylim=c(56,65))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill = "Interaction #")+
  theme_bw()
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_Interactions_NBering_gridded_02.png"), dpi=250)

# Appendix: fishing density -----------------------------------------------
tracks<-boats
names(tracks)
tracks$totono<-1
# Make a spatial Points Dataframe
tracks_bt <- SpatialPointsDataFrame(coords=cbind(tracks$lon,tracks$lat),
                                    data=as.data.frame(tracks),
                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))

# Make a raster with the count of birds in each cell
# Set the grid cell resolution for your out raster (we are in degrees in this example)
res<-.25

# Make a raster with the same extent as as your SPDF above with a buffer in the
#  units that the spdf is in
e<-raster::extent(raster::buffer(tracks_bt,width=res))

# Make a raster from the extent with a resolution in the units you have
aa<-raster::raster(e,resolution=res)

boats_r<-raster::rasterize(x =tracks_bt[!is.na(tracks_bt[["totono"]]),],
                           y = aa,
                           field="totono",
                           fun=function(x,...) sum(x))

# change to a dataframe for ggplot (this might explode if you have a ton of cells)
boats.df<-raster::as.data.frame(boats_r,xy=T)


col.range=c(1,max(log(boats.df$layer), na.rm=TRUE)+2)

quartz(height=4,width=6)
ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey70",color="grey60",size=0.1)+
  geom_tile(data=boats.df %>% filter(),
            aes(x=wrap360(x),y=y,fill=log(layer)))+
  scale_fill_viridis(na.value = NA, option="magma",limits=col.range)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,240),ylim=c(19,65))+
  xlab(expression(paste("Longitude (",degree,"E)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  labs(fill = "AIS locations")+
  theme_bw()
quartz.save(paste0(userdir,"/Analysis/PLOTS/BOATS_AIS_gridded25.png"), dpi=250)
quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_April2020/plots/Appendix_BOATS_AIS_gridded25.png"),dpi=1000)


# AIS data ----------------------------------------------------------------
boats$dt<-as.character(boats$timestamp)
boats$datetime_GMT<-ymd_hms(boats$dt)
tz(boats$datetime_GMT)<-"GMT"
boats$date<-as.character(date(boats$datetime_GMT))
boats$group=paste0(boats$mmsi,boats$date)
colourCount = nrow(unique(boats%>%dplyr::select(mmsi)))+1

quartz()
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey45", "grey5"),name="Depth") +
  #annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  geom_path(data = dat.eez.usa1, aes(x = long, y = lat, group = group),colour = "deepskyblue", size = 0.1) +
  geom_path(data = dat.eez.usaHI, aes(x = long, y = lat, group = group),colour = "deepskyblue", size = 0.1) +
  geom_path(data = dat.eez.jap, aes(x = long, y = lat, group = group),colour = "deepskyblue", size = 0.1) +
  geom_path(data = dat.eez.Kurils, aes(x = long, y = lat, group = group),colour = "deepskyblue", size = 0.1) +
  geom_path(data = dat.eez.ak, aes(x = long, y = lat, group = group),colour = "deepskyblue", size = 0.1) +
  geom_path(data = dat.eez.usru, aes(x = long, y = lat, group = group),colour = "deepskyblue", size = 0.1) +
  geom_path(data = dat.eez.ru, aes(x = long, y = lat, group = group),colour = "deepskyblue", size = 0.1)+
  geom_path(data = dat.eez.ca, aes(x = long, y = lat, group = group),colour = "deepskyblue", size = 0.1) +
  geom_path(data = dat.eez.mx, aes(x = long, y = lat, group = group),colour = "deepskyblue", size = 0.1)+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey70",color="grey60",size=0.1)+
  geom_path(data = boats, aes(x = wrap360(lon), y = lat,color=as.factor(mmsi), group=group), size = 0.3) +# color="turquoise",
  scale_color_manual(values = getPalette(colourCount))+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,64))+
  xlab(expression(paste("Longitude (",degree,"E)")))+
  ylab(expression(paste("Latitude (",degree,"N)")))+
  theme_bw()+
  #guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.position = "none")
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_AIS.png"), dpi=250)


# STAL density - gridded --------------------------------------------------
tracks<-alllocs%>%filter(species=="STAL")
names(tracks)
tracks$totono<-1
# Make a spatial Points Dataframe
tracks_bt <- SpatialPointsDataFrame(coords=cbind(tracks$lon,tracks$lat),
                                    data=as.data.frame(tracks),
                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))

# Make a raster with the count of birds in each cell
# Set the grid cell resolution for your out raster (we are in degrees in this example)
res<-.25

# Make a raster with the same extent as as your SPDF above with a buffer in the
#  units that the spdf is in
e<-raster::extent(raster::buffer(tracks_bt,width=res))

# Make a raster from the extent with a resolution in the units you have
aa<-raster::raster(e,resolution=res)

stal_r<-raster::rasterize(x =tracks_bt[!is.na(tracks_bt[["totono"]]),],
                           y = aa,
                           field="totono",
                           fun=function(x,...) sum(x))

# change to a dataframe for ggplot (this might explode if you have a ton of cells)
stal.df<-raster::as.data.frame(stal_r,xy=T)


col.range=c(1,max(log(stal.df$layer), na.rm=TRUE)+2)

quartz(height=4,width=6)
ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey70",color="grey60",size=0.1)+
  geom_tile(data=stal.df %>% filter(),
            aes(x=wrap360(x),y=y,fill=log(layer)))+
  scale_fill_viridis(na.value = NA, option="magma",limits=col.range)+
  #geom_path(data = fortify(dat.eez.ak)%>%filter(hole==FALSE), 
  #          aes(x = long, y = lat, group = group),colour = "deepskyblue", size = .05) +
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,240),ylim=c(19,65))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill = "log(# locations)")+
  theme_bw()
quartz.save(paste0(userdir,"/Analysis/PLOTS/STAL_gridded25.png"), dpi=250)


col.range=c(1,61)
tracks<-boats
names(tracks)
tracks$totono<-1
# Make a spatial Points Dataframe
tracks_bt <- SpatialPointsDataFrame(coords=cbind(tracks$lon,tracks$lat),
                                    data=as.data.frame(tracks),
                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))

# Make a raster with the count of birds in each cell
# Set the grid cell resolution for your out raster (we are in degrees in this example)
res<-.1

# Make a raster with the same extent as as your SPDF above with a buffer in the
#  units that the spdf is in
e.1<-raster::extent(raster::buffer(tracks_bt,width=res))

# Make a raster from the extent with a resolution in the units you have
aa<-raster::raster(e.1,resolution=res)

boats_r<-raster::rasterize(x =tracks_bt[!is.na(tracks_bt[["totono"]]),],
                           y = aa,
                           field="totono",
                           fun=function(x,...) sum(x))

# change to a dataframe for ggplot (this might explode if you have a ton of cells)
boats.df<-raster::as.data.frame(boats_r,xy=T)


# Interactions ZOOM ----------------------------------------------------------------
stal_In.df$lon360<-wrap360(stal_In.df$x)
head(stal_In.df)
stal_In.df.NB<-stal_In.df%>%filter(y>60)%>%
  filter(lon360>178)%>%
  filter(lon360<184)
pt<-data.frame(y1=60.19, x1=180.23)
col.range=c(1,max(log(stal_In.df.NB$layer), na.rm=TRUE)+2)
quartz(height=4,width=6)
ggplot()+
  geom_path(data = alllocs, 
            aes(x = lon360, y = lat,color=species, group=UniID), size = 0.05) +# color="turquoise",
  geom_path(data = dat.eez.ak, aes(x = long, y = lat, group = group),colour = "black", size = 0.5) +
  geom_path(data = dat.eez.ru, aes(x = long, y = lat, group = group),colour = "black", size = 0.5) +
  #geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  #scale_fill_gradientn(colours = c("grey45", "grey5"),name="Depth") +
  #geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  #scale_color_manual(values = getPalette(colourCount))+
    #geom_tile(data=boats.df %>% filter(),
    #        aes(x=wrap360(x),y=y,fill=log(layer)))+
  geom_point(data = alllocs%>%filter(year==2013)%>%filter(month==7), 
             aes(x = lon360, y = lat,color=species, group=UniID), size = 0.1) +# color="turquoise",
  #geom_tile(data=stal_In.df.NB %>% filter(),
  #          aes(x=wrap360(x),y=y,fill=layer))+
  scale_fill_viridis(na.value = NA, option="magma",limits=col.range)+
  geom_text(data=stal_In.df,aes(x=wrap360(x),y=y,label=layer),size=1,color="white")+
  geom_point(data=pt, aes(x=x1,y=y1), color="red", size=1)+
  #scale_fill_viridis(na.value = NA, option="magma",limits=col.range)+
  #geom_text(data=boats.df,aes(x=wrap360(x),y=y,label=layer),size=1,color="white")+
  #annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  #scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  #coord_fixed(ratio=1.7,xlim = c(140,160),ylim=c(45,55))+#Kurils
  coord_fixed(ratio=1.7,xlim = c(179,184),ylim=c(60,62))+#N. Bering
  #coord_fixed(ratio=1.7,xlim = c(165,215),ylim=c(51,64))+#AK
  #coord_fixed(ratio=1.7,xlim = c(225,240),ylim=c(45,55))+#CA
  #coord_fixed(ratio=1.7,xlim = c(190,220),ylim=c(20,30))+#HI
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.position = "none")
#quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_Interactions_BOATS_AIS_griddedKurils.png"), dpi=250)
#quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_Interactions_BOATS_AIS_griddedAK.png"), dpi=250)
#quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_Interactions_BOATS_AIS_griddedCA.png"), dpi=250)
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_Interactions_BOATS_AIS_griddedHI.png"), dpi=250)
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_Interactions_BOATS_AIS_griddedNB.png"), dpi=250)

batHR<-readRDS(file = "/Users/rachaelorben/Dropbox/Research/RLKIinc/SGRK_processedDATA/Bathymetry_178_145_50_62.rda")
bat_r<-marmap::as.raster(batHR)
Shore<-rasterToContour(bat_r,level=c(-50,-100,-200,-500,-1000,-1500,-2000,-4000))
Shore_1000<-rasterToContour(bat_r,level=c(-1000))
Shore_1000<-fortify(Shore_1000)
Shore_500<-rasterToContour(bat_r,level=c (-500))
Shore_500<-fortify(Shore_500)
Shore_200<-rasterToContour(bat_r,level=c (-200))
Shore_200<-fortify(Shore_200)
Shore_100<-rasterToContour(bat_r,level=c(-100))
Shore_100<-fortify(Shore_100)

if(Sys.info()[7]=="rachaelorben") HomeDir<-"/Users/rachaelorben/Google Drive/Seabird_Oceanography_Lab/OConnor_pubs/" ##RAO
stal<-read.table("/Users/rachaelorben/Desktop/STALfledglings_all_MTIandSPDfiltered_excluding_deadbirds_and_postfledging_drift_lessthan20kmhr_0.5km_interp_24hr_gaplimit_1hr_timesamp_forR.csv",
                  fill = T, stringsAsFactors = F, header = T, skipNul = TRUE, sep=",")
matlab2POS = function(x,tz = "UTC") {
  days = x - 719529 #719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 #86400 seconds in a day
  return(as.POSIXct(secs,origin = "1970-1-1",tz = tz))#returns POSIXct object
}
stal$datetime<-matlab2POS(stal$mday)
names(stal)

quartz()
ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey70",color="grey60",size=0.1)+
  #geom_point(data = stal, 
  #           aes(x = lon360, y = lat,group=pttid), size = 0.01, color="turquoise")+
  geom_point(data = alllocs%>%filter(species=="STAL")%>%filter(year==2013)%>%filter(month==7), 
             aes(x = lon360, y = lat,group=UniID), size = 0.01,color="turquoise")+
  geom_path(data=Shore_1000,aes(long,lat,group=group), color="grey30", size=0.5)+
  geom_path(data=Shore_500,aes(long,lat,group=group), color="grey40", size=0.5)+
  geom_path(data=Shore_200,aes(long,lat,group=group), color="grey50", size=0.5)+
  geom_path(data=Shore_100,aes(long,lat,group=group), color="grey70", size=0.5)+
  geom_path(data = dat.eez.ak, aes(x = long, y = lat, group = group),colour = "red", size = 0.5) +
  coord_fixed(ratio=1.7,xlim = c(178,184),ylim=c(60,62))+#N. Bering
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.position = "none")
quartz.save(paste0(userdir,"/Analysis/PLOTS/STAL_1hrlocations_NB_allyrs.png"), dpi=250)

quartz()
ggplot()+
  geom_tile(data=bathy2%>%filter(V2>58)%>%
              filter(V1>175)%>%
              filter(V1<184),aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey45", "grey5"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  geom_path(data = dat.eez.ak, aes(x = long, y = lat, group = group),colour = "grey", size = 0.5) +
  coord_fixed(ratio=1.7,xlim = c(175,184),ylim=c(58,62.5))+#N. Bering
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()

col.range=c(1,61)
stal<-alllocs%>%filter(species=="STAL")
stal$lon360<-wrap360(stal$lon)
stal<-stal%>%
  filter(lat>60)%>%
  filter(lon360>178)%>%
  filter(lon360<184)
names(alllocs)
stal$totono<-1
# Make a spatial Points Dataframe
tracks_bt <- SpatialPointsDataFrame(coords=cbind(stal$lon,stal$lat),
                                    data=as.data.frame(stal),
                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))

# Make a raster with the count of birds in each cell
# Set the grid cell resolution for your out raster (we are in degrees in this example)
res<-.1

# Make a raster with the same extent as as your SPDF above with a buffer in the
#  units that the spdf is in
e.1<-raster::extent(raster::buffer(tracks_bt,width=res))

# Make a raster from the extent with a resolution in the units you have
aa<-raster::raster(e.1,resolution=res)

stal_r<-raster::rasterize(x =tracks_bt[!is.na(tracks_bt[["totono"]]),],
                           y = aa,
                           field="totono",
                           fun=function(x,...) sum(x))

# change to a dataframe for ggplot (this might explode if you have a ton of cells)
stal.df<-raster::as.data.frame(stal_r,xy=T)

quartz()
ggplot()+
  #geom_path(data = alllocs, 
  #          aes(x = lon360, y = lat,color=species, group=UniID), size = 0.05) +# color="turquoise",
  geom_polygon(data=w2hr_sub,
               aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  #geom_point(data = alllocs, 
  #           aes(x = lon360, y = lat,color=species, group=UniID), size = 0.1) +# color="turquoise",
  geom_tile(data=stal.df %>% filter(is.na(layer)==FALSE),
            aes(x=wrap360(x),y=y,fill=layer))+
  scale_fill_viridis(na.value = NA, option="magma",limits=col.range)+
  geom_path(data = dat.eez.ak, aes(x = long, y = lat, group = group),colour = "black", size = 0.5) +
  coord_fixed(ratio=1.7,xlim = c(178,184),ylim=c(60,62.5))+#N. Bering
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.position = "none")
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_Interactions_BOATS_AIS_griddedNB.png"), dpi=250)

# BFAL indiv. -------------------------------------------------------------
bfal<-birds%>%filter(species=="BFAL")
quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey45", "grey5"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(species=="BFAL")%>%filter(ID==259817), aes(x = lon360, y = lat,color=ID, group=UniID), size = 0.3) +# color="turquoise",
  scale_color_manual(values = getPalette(colourCount))+
  geom_point(data = birds%>%filter(species=="BFAL")%>%filter(birdID==259817), 
             aes(x = wrap360(env_lon), y = env_lat), color="green", shape=12,size = 1.5) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(species=="BFAL")%>%filter(birdID==259817), 
             aes(x = wrap360(env_lon), y = env_lat),  color="turquoise", size = 1.5) +
  #annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  #guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.position = "none")
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_BFAL259817.png"), dpi=250)


quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(ID==259817), 
            aes(x = lon360, y = lat,group=UniID, color=as.factor(TripNum)), size = 0.2) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(birdID==259817), 
             aes(x = wrap360(env_lon), y = env_lat),  color="yellow", size = 1.5) +
  geom_point(data = birds%>%filter(birdID==259817)%>%filter(time3_min==0),  color="green",
             aes(x = wrap360(env_lon), y = env_lat),  size = 1.5) +
  #annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+facet_wrap(~TripNum)+
  #guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.position = "none")
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_BFAL259817.png"), dpi=250)


# LAAL indiv. -------------------------------------------------------------
quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(ID==1207), 
            aes(x = lon360, y = lat,group=UniID, color=as.factor(TripNum)), size = 0.2) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(birdID==1207), 
             aes(x = wrap360(env_lon), y = env_lat),  color="yellow", size = 1.5) +
  geom_point(data = birds%>%filter(birdID==1207)%>%filter(time3_min==0),  color="green",
             aes(x = wrap360(env_lon), y = env_lat),  size = 1.5) +
  #annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+theme(legend.position = "none")
#guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+

quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_LAAL1207.png"), dpi=250)

quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(ID==1131), 
            aes(x = lon360, y = lat,group=UniID, color=as.factor(TripNum)), size = 0.2) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(birdID==1131), 
             aes(x = wrap360(env_lon), y = env_lat),  color="yellow", size = 1.5) +
  geom_point(data = birds%>%filter(birdID==1131)%>%filter(time3_min==0),  color="green",
             aes(x = wrap360(env_lon), y = env_lat),  size = 1.5) +
  #annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+theme(legend.position = "none")
#guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+

quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_LAAL1131.png"), dpi=250)



# LAAL forage trips w/ interactions (>40% into trip) -------------------------------------------------
laal<-birds%>%filter(species=="LAAL")%>%filter(perTrip>0.40)
laal$ID_TripNum<-paste0(laal$birdID,"_",laal$TripNum)
names(laal)
laal.ids.ptrip<-unique(laal$ID_TripNum)

names(alllocs)
alllocs$ID_TripNum<-paste0(alllocs$ID.x,"_",alllocs$TripNum)
ids<-unique(alllocs$ID_TripNum)
laal_sel<-alllocs[alllocs$ID_TripNum%in%laal.ids.ptrip,]



quartz(height=4,width=6)
ggplot()+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey70",color="grey60",size=0.1)+
  geom_path(data = laal_sel, aes(x = lon360, y = lat,group=UniID), size = 0.5) +
  geom_point(data = laal%>%filter(time3_min==0), 
             aes(x = wrap360(env_lon), y = env_lat), size = 2) +
  geom_point(data = laal%>%filter(time3_min>0), 
             aes(x = wrap360(env_lon), y = env_lat,color=geartypeCLASS),    size = 1.5) +
  geom_path(data = fortify(dat.eez.ak)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "deepskyblue", size = .5) +
  geom_path(data = fortify(dat.eez.usaHI)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "deepskyblue", size = .5) +
  geom_path(data = fortify(dat.eez.ru)%>%filter(hole==FALSE), aes(x = long, y = lat, group = group),colour = "deepskyblue", size = .5)+
  geom_path(data = ru, aes(x = long, y = lat, group = group), colour = "deepskyblue",size = .5)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(170,219),ylim=c(19,58))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_LAAL_40pertrips.png"), dpi=250)


dat<-birds%>%filter(species=="LAAL")%>%
  #filter(env_lat<35)%>%
  filter(Territory1!="Alaska")%>%
  dplyr::select(time3_min, date_min, env_lat, flag, geartype, length, interactionYN,RST_domstate_2hr,perTrip,Territory1)
dat$tripBE<-"B"
dat$tripBE[dat$perTrip>=.50]<-"E"
dat$month<-month(dat$date_min)

dat%>%group_by(tripBE,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))
dat%>%group_by(tripBE,RST_domstate_2hr,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))
dat%>%group_by(tripBE,Territory1,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))

birds%>%filter(species=="LAAL")%>%group_by(Territory1,interactionYN)%>%
  summarise(n=n())%>%spread(interactionYN, n)%>%mutate(total=Y+N,interR=Y/(N+Y))

# STAL track animation ----------------------------------------------------

alllocs<-alllocs%>%dplyr::filter(species=="STAL")
unique(year(alllocs$datetime))
alllocs$month<-month(alllocs$datetime)
alllocs$date_real<-date(alllocs$datetime)
unique(alllocs$month)
alllocs$datetimeC<-as.character(alllocs$datetime)
alllocs$datetimeC<-str_replace(alllocs$datetimeC, "2011", "2019")
alllocs$datetimeC<-str_replace(alllocs$datetimeC, "2012", "2019")
alllocs$datetimeC<-str_replace(alllocs$datetimeC, "2013", "2019")
alllocs$datetimeC<-str_replace(alllocs$datetimeC, "2014", "2019")
alllocs$datetimeC<-str_replace(alllocs$datetimeC, "2015", "2019")

#for (i in 68107:nrow(alllocs)){
#  if(alllocs$month==1 | alllocs$month==2 |alllocs$month==3|alllocs$month==4 |alllocs$month==5){
#    alllocs$datetimeC[i]<-str_replace(alllocs$datetimeC[i], "2019", "2020")}
#}

alllocs$datetime_dt<-ymd_hms(alllocs$datetimeC)
alllocs$date<-date(alllocs$datetime_dt)
min(alllocs$datetime_dt, na.rm=TRUE)
max(alllocs$datetime_dt, na.rm=TRUE)
alllocs<-alllocs%>%dplyr::filter(is.na(datetime_dt)==FALSE)

birds<-birds%>%dplyr::filter(species=="STAL")
names(birds)
birds$datetimeC<-as.character(birds$tmin_30)
birds$datetimeC<-str_replace(birds$datetimeC, "2011", "2019")
birds$datetimeC<-str_replace(birds$datetimeC, "2012", "2019")
birds$datetimeC<-str_replace(birds$datetimeC, "2013", "2019")
birds$datetimeC<-str_replace(birds$datetimeC, "2014", "2019")
birds$datetimeC<-str_replace(birds$datetimeC, "2015", "2019")
birds$datetime_dt<-ymd_hms(birds$datetimeC)
birds$date_real<-date(birds$tmin_30)
birds$date<-date(birds$datetime_dt)
birds<-birds%>%dplyr::filter(is.na(datetime_dt)==FALSE)
ghost_points_fin <- tibble(
  date = seq(as.Date('2019-01-01'),
                   as.Date('2019-12-31'),
                   by = 'days'),
  followers = 0, lon = 0, lat = 0)
names(birds)
AL<-birds%>%dplyr::select(birdID, date_real,eventID,interactionYN)
head(AL)
alllocs1<-left_join(alllocs,AL,by=c("ID"="birdID", "date_real"="date_real"))
head(alllocs$UniID)
head(alllocs1$date_real)
unique(alllocs1$interactionYN)

pal<-colorRampPalette(brewer.pal(9,"GnBu"))(34)

leg<-NULL
leg$type<-c("E", "I")
leg$lat<-c(61,64)
leg$lon<-c(225,225)
leg<-data.frame(leg)

animated_plot<-
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  geom_point(data = alllocs1, aes(x = lon360, y = lat,color=UniID, group=UniID), size = 0.2) +
  scale_colour_manual(values = pal)+
  geom_point(data = alllocs1%>%filter(interactionYN=="Y"), 
             aes(x = wrap360(lon360), y = lat, group=eventID), color="coral1", size = 1) +
  geom_point(data = alllocs1%>%filter(interactionYN=="N"), 
             aes(x = wrap360(lon360), y = lat, group=eventID),  color="yellow", size = .5) +
  geom_point(data=leg%>%dplyr::filter(type=="E"), aes(x=lon, y=lat), color="yellow", size=3)+
  geom_point(data=leg%>%dplyr::filter(type=="I"), aes(x=lon, y=lat), color="coral1", size=3)+
  geom_point(data=ghost_points_fin,aes(x = lon, y = lat, size = followers),alpha = 0) +
  annotate("text",x=wrap360(-133),y=61,label="Encounter (≤30km)",color="black",size=5,hjust = 0)+
  annotate("text",x=wrap360(-133),y=64,label="Interaction (≤3km)",color="black",size=5,hjust = 0)+
  annotate("text",x=wrap360(125),y=66,label="Short-tailed albatross",color="black",
           size=6,hjust = 0,angle = 0)+
  annotate("text",x=wrap360(-170),y=20,label="(Deguchi et al. 2014, Orben et al. 2018)",color="white",size=5,hjust = 0)+
  scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  theme(legend.position = "none")+
  #shadow_wake(wake_length = 0.1, alpha = FALSE)+
  labs(title='Day: {format(frame_time, "%b %e")}')+
  transition_time(date) +
  ease_aes('linear')
anim_save("/Users/rachaelorben/Desktop/WSTC5_photosplots/STAL_Tracks.gif", animated_plot, 
          nframes=360, fps = 8, end_pause = 0, width = 1024, height = 512)

quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(species=="STAL"), aes(x = lon360, y = lat,group=UniID), color="turquoise", size = 0.2) +
  geom_point(data = birds%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat,color=species),  size = .75) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat),  color="yellow", size = .75) +
  #annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  #scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(145,160),ylim=c(42,51))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.position = "none")
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_STAL_Kurils.png"), dpi=600)

quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(species=="STAL"), aes(x = lon360, y = lat,group=UniID), color="turquoise", size = 0.2) +
  geom_path(data = dat.eez.ak, aes(x = long, y = lat, group = group),colour = "black", size = 0.75) +
  geom_path(data = dat.eez.ru, aes(x = long, y = lat, group = group),colour = "blue", size = 0.75)+
  geom_point(data = birds%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat,color=flag),  size = .75) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat),  color="yellow", size = .2) +
  #annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  #scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(175,185),ylim=c(58,63))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.position = "none")
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_STAL_NBering_flag.png"), dpi=600)

quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(species=="STAL"), aes(x = lon360, y = lat,group=UniID), color="turquoise", size = 0.2) +
  geom_path(data = dat.eez.ak, aes(x = long, y = lat, group = group),colour = "black", size = 0.75) +
  geom_path(data = dat.eez.ru, aes(x = long, y = lat, group = group),colour = "blue", size = 0.75)+
  geom_point(data = birds%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat,color=geartypeCLASS),  size = .75) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat),  color="yellow", size = .2) +
  #annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  #scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(175,185),ylim=c(58,63))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme()
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_STAL_NBering_gear.png"), dpi=600)

quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),name="Depth") +
  geom_path(data = alllocs%>%filter(species=="STAL"), 
            aes(x = lon360, y = lat,group=UniID), alpha=.3,color="turquoise", size = 0.2) +
  geom_path(data = dat.eez.ak, aes(x = long, y = lat, group = group),colour = "blue", size = 0.3) +
  geom_path(data = dat.eez.ru, aes(x = long, y = lat, group = group),colour = "blue", size = 0.3)+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  geom_point(data = birds%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat,color=species),  size = .1) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat),  color="yellow", size = .1) +
  #annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  #scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(161,205),ylim=c(50,65))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.position = "none")
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_STAL_Bering_EEZ.png"), dpi=600)

quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(species=="STAL"), 
            aes(x = lon360, y = lat,group=UniID), alpha=.3,color="turquoise", size = 0.2) +
  geom_point(data = birds%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat,color=species),  size = .1) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(species=="STAL"), 
             aes(x = wrap360(env_lon), y = env_lat),  color="yellow", size = .1) +
  #annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  #scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(190,200),ylim=c(53,56))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.position = "none")
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_STAL_eAleutians.png"), dpi=600)

quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey60", "grey20"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="deepskyblue70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(species=="STAL"), 
            aes(x = lon360, y = lat,group=UniID), alpha=.3,color="turquoise", size = 0.2) +
  geom_point(data = birds%>%filter(species=="STAL")%>%filter(month==12), 
             aes(x = wrap360(env_lon), y = env_lat,color=species),  size = .1) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(species=="STAL")%>%filter(month==12), 
             aes(x = wrap360(env_lon), y = env_lat),  color="yellow", size = .1) +
  #annotate("text",x=wrap360(-140),y=23,label="Depth (m)",color="black",size=3,hjust = 0)+
  #scale_x_continuous(breaks=c(125,150,180,210,240),labels=c("120", "150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(225,238),ylim=c(47,55))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 6, barheight = .7,direction = "horizontal"))+
  theme(legend.position = "none")
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_EncountersInteractions_STAL_DecCanada.png"), dpi=600)



# histrograms -------------------------------------------------------------
names(birds)
quartz(width = 8,height=3)
ggplot()+
  geom_bar(data=birds,aes(x=geartypeCLASS,fill=geartypeCLASS))+
  theme_bw()+facet_wrap(~species, scales="free")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_Encounters_geartype.png"), dpi=600)

quartz(width = 8,height=3)
ggplot()+
  geom_bar(data=birds,aes(x=flag,fill=flag))+
  theme_bw()+facet_wrap(~species, scales="free")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
quartz.save(paste0(userdir,"/Analysis/PLOTS/Albie_Encounters_flag.png"), dpi=600)

names(alllocs)
alllocs$month<-month(alllocs$datetime)
alllocs$year<-year(alllocs$datetime)
alllocs$date<-date(alllocs$datetime)
birdsMO<-unique(alllocs%>%dplyr::select("ID.x","species","date","month","year"))
birdsMO.dys<-birdsMO%>%group_by(ID.x,species,month,year)%>%summarise(n=n())
birdsMO.dys1<-birdsMO.dys%>%group_by(species, month)%>%summarise(mean=mean(n),
                                                   sd=sd(n))

ggplot()+
  geom_bar(data=unique(birdsMO%>%dplyr::select("ID.x","species","month")), 
           aes(x=month), fill="grey80")+
  geom_point(data=birdsMO.dys1, 
           aes(y=mean, x=month), color="black", size=.5)+
  geom_errorbar(data=birdsMO.dys1,
                aes(x=month, ymin=mean-sd, ymax=mean+sd), 
                width=.2,position=position_dodge(.9))+
  scale_x_continuous(breaks=c(2,4,6,8, 10, 12),labels=c("2", "4","6","8","10","12"))+
  theme_bw()+facet_wrap(~species)


birds%>%
  filter(env_lat>60)%>%
  filter(wrap360(env_lon)>178)%>%
  filter(wrap360(env_lon)<186)%>%
  filter(geartypeCLASS=="Trawlers")%>%
  filter(flag=="USA" | flag=="RUS")%>%
  group_by(flag, interactionYN)%>%summarise(n=n())
68/(68+176)
8/(8+22)

names(birds)
birds%>%
  filter(wrap360(env_lon)>160)%>%
  filter(LonghurstCode=="BERS")%>%
  filter(geartypeCLASS=="Trawlers" | geartypeCLASS=="Longliners")%>%
  filter(flag=="USA" | flag=="RUS")%>%
  group_by(flag, geartypeCLASS,interactionYN)%>%summarise(n=n())
80/(80+239) #rus trawl
72/(72+255) #us trawl
20/(20+49) #rus longline
28/(28+32) #us longline

birds%>%
  filter(wrap360(env_lon)<160)%>%
  filter(LonghurstCode=="BERS")%>%
  #filter(geartypeCLASS=="Trawlers")%>%
  filter(flag=="USA" | flag=="RUS")%>%
  group_by(flag, geartypeCLASS, interactionYN)%>%summarise(n=n())
37/(37+89)
5/(5+7)



alllocs<-alllocs%>%dplyr::filter(species=="STAL")
alllocs%>%group_by(ID.x)%>%summarise(year1=min(year))


#MIDWAY
#inset dataframe
names(alllocs)
alllocs$month<-month(alllocs$datetime)
alllocs$year<-year(alllocs$datetime)
alllocs$date<-date(alllocs$datetime)
birdsMO<-unique(alllocs%>%dplyr::select("ID.x","species","date","month","year"))
birdsMO.dys<-birdsMO%>%group_by(ID.x,species,month,year)%>%summarise(n=n())
birdsMO.dys1<-birdsMO.dys%>%group_by(species, month)%>%summarise(mean=mean(n),
                                                                 sd=sd(n))

names(birds)
colourCount = nrow(unique(alllocs%>%filter(species=="BFAL")%>%dplyr::select(ID.x)))+1

getPalette = colorRampPalette(brewer.pal(9, "Set1"))
alllocs$ID<-alllocs$ID.x

quartz(height=4,width=6)
ggplot()+
  geom_tile(data=bathy2,aes(x=V1,y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey45", "grey5"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="grey70",color="grey60",size=0.1)+
  geom_path(data = alllocs%>%filter(colony=="MIAT"), 
            aes(x = lon360, y = lat,group=UniID, color =species), size = 0.2)+
  geom_point(data = birds%>%filter(colony=="MIAT"), 
             aes(x = wrap360(env_lon), y = env_lat,color=species), size = .5) +
  geom_point(data = birds%>%filter(time3_min>0)%>%filter(colony=="MIAT"), 
             aes(x = wrap360(env_lon), y = env_lat),  color="yellow", size = .5) +
  annotate("text",x=wrap360(128),y=63,label="BFAL",color="black",size=8,hjust = 0)+
  scale_x_continuous(breaks=c(150,180,210,240),labels=c("150", "180","-150","-120"))+
  coord_fixed(ratio=1.7,xlim = c(130,245),ylim=c(19,65))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  theme(legend.position = "none")

birds$time3_min_01<-birds$time3_min>0
birds$time3_min_01[birds$time3_min_01>0]<-1

birds%>%filter(colony=="MIAT")%>%
  group_by(species,birdID,time3_min_01)%>%
  summarise(n=n())



