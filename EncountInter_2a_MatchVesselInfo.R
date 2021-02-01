library(dplyr)
library(ggplot2)
library(lubridate)
library(rworldmap)
library(marmap)
wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}

if(Sys.info()[7]=="rachaelorben") {
  userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'
  GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/"
  userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'}

prox.sum<-readRDS(paste0(userdir,"/Analysis/compileddata/proxsummary.rda"))
prox.sumsel<-prox.sum%>%filter(time30_min>0)
prox.sumsel$date_min<-date(prox.sumsel$tmin_30)
prox.sumsel$date_max<-date(prox.sumsel$tmax_30)

vessels.id<-unique(prox.sumsel$mmsi)

vessels<-read.csv("/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch/GFW/vessels/vessels.csv",na.strings=c(""," ","NA"))
colnames(vessels)
vessels<-vessels%>%select(-shipname,-callsign,-imo,-inferred_geartype_score,-inferred_subgeartype,
                          -inferred_subgeartype_score,-source)%>%
  filter(mmsi %in% vessels.id)

#finds duplicated mmsi entries
count_na <- function(x) sum(is.na(x))#https://stackoverflow.com/questions/35443115/add-a-column-with-count-of-nas-and-mean-in-r-with-dplyr

vessel.dups<-vessels %>% filter(duplicated(.[["mmsi"]]))%>%
  arrange(mmsi)%>%
  mutate(count_na = apply(., 1, count_na)) #chooses the line with the most info

v.dups<-unique(vessel.dups$mmsi)

v.dups.sel<-NULL
for (i in 1:length(v.dups)){
  vd<-vessel.dups%>%filter(mmsi==v.dups[i])
  v<-vd%>%filter(count_na==min(vd$count_na)) #if the lines have the same amount of info, chooses the first entry
  if (nrow(v)==2){v<-v[1,]}
  v.dups.sel<-rbind(v.dups.sel,v)
}

vesselsa<-vessels%>%filter(!(mmsi %in% v.dups.sel$mmsi))
vessels<-rbind(vesselsa,v.dups.sel[,1:10])
colnames(vessels)

V<-NULL
for (i in 1:nrow(vessels)){
  rowdta<-vessels[i,]
  
  #geartype combo
  rowdta$geartype<-rowdta$registry_geartype
  if (is.na(rowdta$registry_geartype)==TRUE){rowdta$geartype<-rowdta$inferred_geartype}
  
  #length combo
  rowdta$length<-rowdta$registry_length
  if (is.na(rowdta$registry_length)==TRUE){rowdta$length<-rowdta$inferred_length}
  
  #engine_power combo
  rowdta$engine_power<-rowdta$registry_engine_power
  if (is.na(rowdta$registry_engine_power)==TRUE){rowdta$engine_power<-rowdta$inferred_engine_power}
  
  #engine_power combo
  rowdta$engine_power<-rowdta$registry_engine_power
  if (is.na(rowdta$registry_engine_power)==TRUE){rowdta$engine_power<-rowdta$inferred_engine_power}
  
  V<-rbind(V,rowdta)
}
colnames(V)
vessels<-V%>%select(mmsi,flag,geartype,length,engine_power)

strict_left_join <- function(x, y, by = NULL, ...){
  #https://github.com/tidyverse/dplyr/issues/2278
  by <- common_by(by, x, y)
  if(any(duplicated(y[by$y]))) {
    stop("Duplicate values in foreign key")
  } else left_join(x, y, by = by, ...)
}


# Gear Type Classification ------------------------------------------------
vessels$geartypeCLASS<-NA
unique(vessels$geartype)
#longliners
vessels$geartypeCLASS[which(vessels$geartype=="drifting_longlines|research" |
                              vessels$geartype=="drifting_longlines|set_longlines" |
                              vessels$geartype=="set_longlines" |
                              vessels$geartype=="drifting_longlines")]<-"Longliners"
#Other/Mixed
vessels$geartypeCLASS[which(vessels$geartype=="driftnets|trawlers" |
                              vessels$geartype=="pots_and_traps|set_gillnets|set_longlines" |
                              vessels$geartype=="pots_and_traps|trawlers" |
                              vessels$geartype=="set_longlines|trawlers" |
                              vessels$geartype=="drifting_longlines|set_longlines|trawlers")]<-"Mixed"
#Unknown
vessels$geartypeCLASS[which(vessels$geartype=="fishing"|
                              vessels$geartype=="NA"|
                              vessels$geartype=="other_fishing"|
                              is.na(vessels$geartype)==TRUE)]<-"Unknown"
#Trawlers
vessels$geartypeCLASS[which(vessels$geartype=="trawlers" |
                              vessels$geartype=="cargo|trawlers")]<-"Trawlers"
#Fish factory
vessels$geartypeCLASS[which(vessels$geartype=="fish_factory|fishing" |
                              vessels$geartype=="fish_factory|trawlers" |
                              vessels$geartype=="fish_factory")]<-"Fish_factory"

#pole_and_line
vessels$geartypeCLASS[which(vessels$geartype=="pole_and_line"|
                              vessels$geartype=="trollers"
                              )]<-"pole_and_line"

#squid_jigger
vessels$geartypeCLASS[which(vessels$geartype=="squid_jigger" )]<-"squid_jigger"

#Purse seiner
vessels$geartypeCLASS[which(vessels$geartype=="purse_seines" |
                              vessels$geartype=="tuna_purse_seines")]<-"Purse seiner"

#pots_and_traps
vessels$geartypeCLASS[which(vessels$geartype=="pots_and_traps" |
                              vessels$geartype=="fixed_gear")]<-"pots_and_traps"
#Reefer
vessels$geartypeCLASS[which(vessels$geartype=="reefer" |
                              vessels$geartype=="reefer|trawlers" |
                              vessels$geartype=="fishing|reefer")]<-"Reefer"


unique(vessels$geartypeCLASS)

vessels%>%filter(is.na(geartypeCLASS)==TRUE)
gt<-vessels%>%group_by(geartype)%>%summarise(n=n())

prox.sumsel.vessels<-strict_left_join(prox.sumsel,vessels,by="mmsi") #phew same number of events!
prox.sumsel.vessels$interactionYN<-"N"
prox.sumsel.vessels$interactionYN[prox.sumsel.vessels$time3_min>0]<-"Y"
ggplot()+
  geom_bar(data=prox.sumsel.vessels,aes(geartypeCLASS, fill=interactionYN))+
  facet_wrap(~species, scales="free_y")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






write.csv(gt,paste0(userdir,"/GearType.csv"))

noinfo<-vessels%>%filter(geartype=="fishing")
write.csv(noinfo,paste0(userdir,"/GearType_noinfo.csv"))

prox.sumsel.vessels%>%filter(geartype=="drifting_longlines|research")
saveRDS(prox.sumsel.vessels,paste0(userdir,"/Analysis/compileddata/EI_vesseldata.rda"))
