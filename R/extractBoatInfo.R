extractBoatInfo<-function(alllocsG,yr,distlim=80,searchbox=1.5,GFWDataDir,resolution="LR"){
  require(lubridate)
  require(data.table)
  
  locs<-subset(alllocsG,year(datetime)==yr)
  locs$day<-date(locs$datetime)
  days<-unique(date(locs$datetime))
  
  BOATnALBIES<-data.frame()
  for (i in 1:length(days)){
    day<-days[i]
    print(day)
    #pulls in GFW data
    Data<-fread(file=paste0(GFWDataDir,"/",yr,"/",day,".csv"),stringsAsFactors=FALSE) #edit
    
    if (resolution=="LR"){
      Data$lat=(Data$lat_bin)*.10
      Data$lon=wrap360((Data$lon_bin)*.10)}
    
    if (resolution=="HR"){
      Data$lat=(Data$lat_bin)*.010
      Data$lon=wrap360((Data$lon_bin)*.010)}
    
    head(Data)
    
    #selects tracking data from that day
    d<-locs[locs$day==day,]
    d_lat_min<-min(d$lat); d_lat_max<-max(d$lat)
    d_lon_min<-min(d$lon360); d_lon_max<-max(d$lon360)
    
    #subsets GFW data to area around albatross locations
    Data_s<-subset(Data, lat>(d_lat_min-searchbox) & lat<(d_lat_max+searchbox))
    Data_s<-subset(Data_s, lon>(d_lon_min-searchbox) & lon<(d_lon_max+searchbox))
    
    #distance between all albtross locations and all grid cells with boats. 
    BoatsAlbies<-data.frame()
    #start.time <- Sys.time()
    for(k in 1:nrow(d)){ 
      
      #tracking data
      lat1<-d$lat[k];lon1<-unwrap360(d$lon360[k]); lon1360<-d$lon360[k]
      
      #subsets GFW data to area around albatross locations
      Data_ss<-subset(Data_s, lat>(min(lat1)-searchbox) & lat<(max(lat1)+searchbox))
      Data_ss<-subset(Data_ss, lon>(min(lon1360)-searchbox) & lon<(max(lon1360)+searchbox))
      
      Data_ss$Dist2Boat<-NA
      for (j in 1:nrow(Data_ss)){
        Data_ss$Dist2Boat[j]<-argosfilter::distance(lat1 = lat1,lat2 = Data_ss$lat[j],lon1 = lon1, lon2 = Data_ss$lon[j])
      }
      Boats<-subset(Data_ss,Dist2Boat<distlim)
      Boats$albatross_ID<-d$ID[k]
      Boats$albatross_species<-d$species[k]
      Boats$albatross_datetime<-d$datetime[k]
      Boats$albatross_oid<-d$global_oid[k]
      Boats$albatross_lat<-d$lat[k]
      Boats$albatross_lon360<-d$lon360[k]
      BoatsAlbies<-rbind(BoatsAlbies,Boats) 
      
    }
    #end.time <- Sys.time()
    #time.taken <- end.time - start.time
    #print(time.taken)
    BOATnALBIES<-rbind(BOATnALBIES,BoatsAlbies)
  }
  return(BOATnALBIES)
}
