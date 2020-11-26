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
    rday1<-rasterstackG[[id]]
    rday<-crop(x = rday1, y = new.extent)
    
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
