#install.packages("devtools")
#devtools::install_github("ropensci/rerddap", force = TRUE)
#devtools::install_github("rmendels/rerddapXtracto")
library(lubridate)
library(rerddap)
library(dplyr)

library(rerddapXtracto)
library(plotdap)
library(sf)
library(velox)

library(lubridate)
library(sp)
library(raster)
library(rgeos)

#plotting
library(plotdap)
library(ggplot2)
#devtools::install_github('thomasp85/gganimate')
library(gganimate)

wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}
unwrap360<-function(lon360){
  lon<-ifelse(lon360 > 180, -360 + lon360, lon360)
  return(lon)}

mapFrame<- function(longitude,latitude,chla){
  dims<-dim(chla)
  chla<-array(chla,dims[1]*dims[2])
  longitude<-longitude-360
  chlaFrame<-expand.grid(x=longitude,y=latitude)
  chlaFrame$chla<-chla
  return(chlaFrame)
}



if(Sys.info()[7]=="rachaelorben") userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'
alllocs<-readRDS(paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr.rds"))
min(alllocs$lat)
max(alllocs$lat)
min(wrap360(alllocs$lon), na.rm=TRUE)
max(wrap360(alllocs$lon), na.rm=TRUE)
max(alllocs$date,na.rm=TRUE)
alllocs$lon<-unwrap360(alllocs$lon360)

year<-seq(2012,2017,1)

# WIND xtractogons ------------------------------------------------
(info <- rerddap::info('ncdcOw6hrP')) #https://www.ncdc.noaa.gov/data-access/marineocean-data/blended-global/blended-sea-winds


for (i in 1:length(year)){
  yr<-year[i]
  tpos <- c(paste0(yr,"-01-01"), paste0(yr,"-12-01"))
  
  start.time <- Sys.time()
  x<-rxtracto_3D(info, parameter="v",
                 xcoord = c(135,245), ycoord = c(17,65),
                 zcoord = 10, tcoord = tpos,
                 xName = 'longitude', yName = 'latitude', tName = 'time')#,
  #urlbase = 'z', verbose = FALSE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  saveRDS(x,paste0(userdir,"/environmentaldata/wind_v_ncdcOw6hrP/",yr,"_v_NP_ncdcOw6hrP.rds"),compress = "bzip2")
}

for (i in 1:length(year)){
  yr<-year[i]
  tpos <- c(paste0(yr,"-01-01"), paste0(yr,"-12-01"))
  
  start.time <- Sys.time()
  x<-rxtracto_3D(info, parameter="u",
                 xcoord = c(135,245), ycoord = c(17,65),
                 zcoord =10, tcoord = tpos,
                 xName = 'longitude', yName = 'latitude', tName = 'time')#,
  #urlbase = 'http://upwell.pfeg.noaa.gov/erddap', verbose = FALSE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  saveRDS(x,paste0(userdir,"/environmentaldata/wind_u_ncdcOw6hrP/",yr,"_u_NP_ncdcOw6hrP.rds"),compress = "bzip2")
}


# chl xtractogons ------------------------------------------------
(info <- rerddap::info('erdMH1chla8day')) ##urlbase <- 'http://upwell.pfeg.noaa.gov/erddap'

#
for (i in 1:length(year)){
  yr<-year[i]
  tpos <- c(paste0(yr,"-01-01"), paste0(yr,"-12-01"))
  
  start.time <- Sys.time()
  x<-rxtracto_3D(info, parameter="chlorophyll",
                 xcoord = c(135,179.97917), ycoord = c(17,65),
                 tcoord = tpos,
                 xName = 'longitude', yName = 'latitude', tName = 'time')#,
  #urlbase = 'http://upwell.pfeg.noaa.gov/erddap', verbose = FALSE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  #saveRDS(x,paste0(userdir,"/environmentaldata/chl_erdMH1chla8day/",yr,"_chl_westNP_erdMH1chla8day.rds"),compress = "bzip2")
}

for (i in 1:length(year)){
  yr<-year[i]
  tpos <- c(paste0(yr,"-01-01"), paste0(yr,"-12-01"))
  
  start.time <- Sys.time()
  x<-rxtracto_3D(info, parameter="chlorophyll",
                 xcoord = c(-179.97917,-115), ycoord = c(17,65),
                 tcoord = tpos,
                 xName = 'longitude', yName = 'latitude', tName = 'time')#,
  #urlbase = 'http://upwell.pfeg.noaa.gov/erddap', verbose = FALSE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  saveRDS(x,paste0(userdir,"/environmentaldata/chl_erdMH1chla8day/",yr,"_chl_eastNP_erdMH1chla8day.rds"),compress = "bzip2")
}

tpos <- c(paste0(yr,"-01-01"), paste0(yr,"-01-01"))
x<-rxtracto_3D(info, parameter="chlorophyll",
               #xcoord = c(-135,-179.97917), ycoord = c(17,65),
               xcoord = c(125,179.97917), ycoord = c(17,65),
               tcoord = tpos,
               xName = 'longitude', yName = 'latitude', tName = 'time')#,

plotBBox(x, maxpixels = 10000)#,xcoord <- c(-135,-179.97917))
quartz()
sanctchlPlot

# SST Aqua MODIS ------------------------------------------------
#SST, Aqua MODIS, NPP, 4km, Daytime (11 microns), 2003-present (8 Day Composite)
(info <- rerddap::info('erdMH1sstd8day')) ##urlbase <- 'http://upwell.pfeg.noaa.gov/erddap'

#
for (i in 1:length(year)){
  yr<-year[i]
  tpos <- c(paste0(yr,"-01-01"), paste0(yr,"-12-01"))
  
  start.time <- Sys.time()
  x<-rxtracto_3D(info, parameter="sst",
                 xcoord = c(135,179.97917), ycoord = c(17,65),
                 tcoord = tpos,
                 xName = 'longitude', yName = 'latitude', tName = 'time')#,
  #urlbase = 'http://upwell.pfeg.noaa.gov/erddap', verbose = FALSE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  saveRDS(x,paste0(userdir,"/environmentaldata/SST_erdMH1sstd8day/",yr,"_SST_westNP_erdMH1sstd8day.rds"),compress = "bzip2")
}

for (i in 1:length(year)){
  yr<-year[i]
  tpos <- c(paste0(yr,"-01-01"), paste0(yr,"-12-01"))
  
  start.time <- Sys.time()
  x<-rxtracto_3D(info, parameter="sst",
                 xcoord = c(-179.97917,-115), ycoord = c(17,65),
                 tcoord = tpos,
                 xName = 'longitude', yName = 'latitude', tName = 'time')#,
  #urlbase = 'http://upwell.pfeg.noaa.gov/erddap', verbose = FALSE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  saveRDS(x,paste0(userdir,"/environmentaldata/SST_erdMH1sstd8day/",yr,"_SST_eastNP_erdMH1sstd8day.rds"),compress = "bzip2")
}
#SSTGHRSST Global 1-km Sea Surface Temperature (G1SST), Global, 0.01 Degree, 2010-2017, Daily


# SST ---------------------------------------------------------------------
(info <- rerddap::info('jplMURSST41'))
library(stringr)

#
for (i in 1:length(year)){
  for (k in 1:12){
  yr<-year[i]
  mo1<-str_pad(k, 2, pad = "0")
  mo2<-str_pad(k+1, 2, pad = "0")
  
  tpos <- c(paste0(yr,"-",mo1,"-01T00:00:00Z"), paste0(yr,"-",mo2,"-01T00:00:00Z"))
  
  start.time <- Sys.time()
  x<-rxtracto_3D(info, parameter="analysed_sst",
                 xcoord = c(135,180), ycoord = c(17,65),
                 tcoord = tpos,
                 xName = 'longitude', yName = 'latitude', tName = 'time',
                 xlen = .2, ylen = .2)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  saveRDS(x,paste0(userdir,"/environmentaldata/SST_jplMURSST41/",yr,"_",mo1,"_SST_westNP_jplMURSST41.rds"),compress = "bzip2")
  }
}

for (i in 1:length(year)){
  yr<-year[i]
  tpos <- c(paste0(yr,"-01-01T09:00:00Z"), paste0(yr,"-12-01T09:00:00Z"))
  
  start.time <- Sys.time()
  x<-rxtracto_3D(info, parameter="analysed_sst",
                 xcoord = c(-179.99,-115), ycoord = c(17,65),
                 tcoord = tpos,
                 xName = 'longitude', yName = 'latitude', tName = 'time')#,
  #urlbase = 'http://upwell.pfeg.noaa.gov/erddap', verbose = FALSE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  saveRDS(x,paste0(userdir,"/environmentaldata/SST_erdMH1sstd8day/",yr,"_SST_eastNP_erdMH1sstd8day.rds"),compress = "bzip2")
}

# Plot environmental layers -----------------------------------------------
chl<-readRDS(paste0(userdir,"/environmentaldata/chl_erdMH1chla8day/2013_chl_westNP_erdMH1chla8day.rds"))
myFunc <- function(x) log(x)
str(chl)

plot(chl[[1]])
a<-plotBBox(chl, plotColor = 'chlorophyll', myFunc = myFunc, time = identity, animate = FALSE)

anim_save(a,paste0(userdir,"/environmentaldata/chl_erdMH1chla8day/2013_chl_westNP_erdMH1chla8day.gif"))

# Xtract along Track ------------------------------------------------------
library(stringr)

colnames(alllocs)
summary(alllocs$year)

alllocs$date<-date(alllocs$datetime)
alllocs$hour<-hour(alllocs$datetime)
unique(alllocs$hour)
alllocs$hour1<-NA
idx0<-which(alllocs$hour==0 |alllocs$hour==1 |alllocs$hour==2 |alllocs$hour==3 |alllocs$hour==4 |alllocs$hour==5)
alllocs$hour1[idx0] <- 0
idx6<-which(alllocs$hour==6 |alllocs$hour==7 |alllocs$hour==8 |alllocs$hour==9 |alllocs$hour==10 |alllocs$hour==11)
alllocs$hour1[idx6] <- 6
idx12<-which(alllocs$hour==12 |alllocs$hour==13 |alllocs$hour==14 |alllocs$hour==15 |alllocs$hour==16 |alllocs$hour==17)
alllocs$hour1[idx12] <- 12
idx18<-which(alllocs$hour==18 |alllocs$hour==19 |alllocs$hour==20 |alllocs$hour==21 |alllocs$hour==22 |alllocs$hour==23)
alllocs$hour1[idx18] <- 18
alllocs%>%group_by(hour1)%>%summarise(n=n())
alllocs$datetime1<-paste0(alllocs$date," ",str_pad(alllocs$hour1,2,pad="0"),":00:00 GMT")
str(alllocs$datetime1)
alllocs$datetime1<-as.POSIXlt(alllocs$datetime1,tz="GMT")
head(alllocs$datetime1,25)

alllocs$year<-year(alllocs$date)
head(alllocs$date);head(alllocs$datetime)
 
YRS<-unique(alllocs$year)
YRS<-data.frame(YRS)
YRS<-YRS%>%filter(is.na(YRS)==FALSE)


# Wind: full map ----------------------------------------------------------
alllocs$wind_v<-NA
alllocs$wind_u<-NA

for (k in 2:nrow(YRS)){
  yr=YRS$YRS[k]
  print ("Wind v New Year")
  
  x<-readRDS(paste0(userdir,"/environmentaldata/wind_v_ncdcOw6hrP/",yr,"_v_NP_ncdcOw6hrP.rds"))
  trackssel<- alllocs[alllocs$year==yr,]
  
  datesY<-unique(trackssel$datetime1)
  datesY<-as.POSIXlt(datesY,tz="GMT")

  for(i in 1:length(datesY)){
    print(datesY[i])
    
    # select the date you want
    datex<-(x$time[x$time==datesY[i]])
    
    # skip the date if it is not in the raster data
    if(length(datex)<1) next
    
    # make a spDataFrame for that date only
    pt<-data.frame(lon360=alllocs$lon360[which(alllocs$datetime1==datesY[i])],
                   lat=alllocs$lat[which(alllocs$datetime1==datesY[i])])
    coordinates(pt)<-cbind(alllocs$lon360[which(alllocs$datetime1==datesY[i])],
                           alllocs$lat[which(alllocs$datetime1==datesY[i])])
    
    crs(pt)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap=180")
    
    #take the xtractogon and make it into a raster - ug
    wiz<-which(x$time==datex)
    chlaFrame<-mapFrame(x$longitude,x$latitude,x$v[,,,wiz])
    #length(x$longitude); length(x$latitude); str(x$v[,,,wiz])
    chlaFrame$x<-wrap360(chlaFrame$x)
    s<- rasterFromXYZ(xyz = chlaFrame) 
    crs(s)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap=180")
    plot(s)
    
    #image(s)
    dat_sf <- st_as_sf(pt)
    
    #Extract the wind underlying each point
    dat_sf <- dat_sf %>%
      mutate(xtract = raster::extract(s, .))
    
    alllocs$wind_v[which(alllocs$datetime1==datesY[i])] <- dat_sf$xtract
    
  }
}

for (k in 2:nrow(YRS)){
  yr=YRS$YRS[k]
  print ("Wind u New Year")
  
  x<-readRDS(paste0(userdir,"/environmentaldata/wind_u_ncdcOw6hrP/",yr,"_u_NP_ncdcOw6hrP.rds"))
  trackssel<- alllocs[alllocs$year==yr,]
  
  datesY<-unique(trackssel$datetime1)
  datesY<-as.POSIXlt(datesY,tz="GMT")
  
  for(i in 1:length(datesY)){
    print(datesY[i])
    
    # select the date you want
    datex<-(x$time[x$time==datesY[i]])
    
    # skip the date if it is not in the raster data
    if(length(datex)<1) next
    
    # make a spDataFrame for that date only
    pt<-data.frame(lon360=alllocs$lon360[which(alllocs$datetime1==datesY[i])],
                   lat=alllocs$lat[which(alllocs$datetime1==datesY[i])])
    coordinates(pt)<-cbind(alllocs$lon360[which(alllocs$datetime1==datesY[i])],
                           alllocs$lat[which(alllocs$datetime1==datesY[i])])
    
    crs(pt)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap=180")
    
    #take the xtractogon and make it into a raster - ug
    wiz<-which(x$time==datex)
    chlaFrame<-mapFrame(x$longitude,x$latitude,x$u[,,,wiz])
    chlaFrame$x<-wrap360(chlaFrame$x)
    s<- rasterFromXYZ(xyz = chlaFrame) 
    crs(s)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap=180")
    plot(s)
    
    #image(s)
    dat_sf <- st_as_sf(pt)
    
    #Extract the wind underlying each point
    dat_sf <- dat_sf %>%
      mutate(xtract = raster::extract(s, .))
    
    alllocs$wind_u[which(alllocs$datetime1==datesY[i])] <- dat_sf$xtract
    
  }
}


# SST west & east ---------------------------------------------------------
alllocs$SST<-NA
alllocs$datetime1<-as.POSIXct(alllocs$datetime1)
bufferM<-185000
10*1000

for (k in 2:nrow(YRS)){
  yr=YRS$YRS[k]
  print ("SST west New Year")
  
  x<-readRDS(paste0(userdir,"/environmentaldata/SST_erdMH1sstd8day/",yr,"_SST_westNP_erdMH1sstd8day.rds"))
  trackssel<- alllocs%>%dplyr::filter(year==yr)%>%
    dplyr::filter(lon<=179.97917)
  
  datesY<-unique(trackssel$date)
  x$time1<-date(x$time)
  for(i in 1:length(datesY)){
    print(datesY[i])

    # select the date you want
    #finds the closest one
    idx<-which(abs(x$time1-datesY[i]) == min(abs(x$time1 - datesY[i])))
    
    datex<-(x$time[idx])[1]
    
    # skip the date if it is not in the raster data
    if(length(datex)<1|is.na(datex)) next
    
    # make a spDataFrame for that date only
    pt<-data.frame(lon=alllocs$lon[which(alllocs$date==datesY[i])],
                   lat=alllocs$lat[which(alllocs$date==datesY[i])])
    coordinates(pt)<-cbind(alllocs$lon[which(alllocs$date==datesY[i])],
                           alllocs$lat[which(alllocs$date==datesY[i])])
    
    crs(pt)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap=180")
    
    #take the xtractogon and make it into a raster - ug
    wiz<-which(x$time==datex)
    
    chlaFrame<-mapFrame(x$longitude,x$latitude,x$sst[,,wiz])
    coordinates(chlaFrame) = ~x+y   
    
    # create an empty raster object to the extent of the points
    rast <- raster(ext=extent(chlaFrame), resolution=c(0.05,0.05))
    
    # rasterize your irregular points 
    s<-rasterize(chlaFrame, rast, chlaFrame$chla, fun=max) # we use a mean function here to regularly grid the irregular input points
    plot(s)
    
    crs(s)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap=180")
    plot(s)
    
    #image(s)
    dat_sf <- st_as_sf(pt)
    
    #Extract the wind underlying each point
    dat_sf <- dat_sf %>%
      mutate(xtract = raster::extract(s, .))
    
    alllocs$SST[which(alllocs$date==datesY[i])] <- dat_sf$xtract
    
  }
}


#create a SpatialPointsDataFrame
pts<-read.table("D:/AMB Docs/Current projects/SAERI paper/Oil models_sean/Combined Risk Blowout.txt",sep=",")
colnames(pts) <- c('x', 'y', 'z')
coordinates(chlaFrame) = ~x+y   

# create an empty raster object to the extent of the points
rast <- raster(ext=extent(chlaFrame), resolution=c(0.05,0.05))

# rasterize your irregular points 
rasOut<-rasterize(chlaFrame, rast, chlaFrame$chla, fun=max) # we use a mean function here to regularly grid the irregular input points
plot(rasOut)
plot(r1, add=T)
#
r1rsmp <- resample(r1,rasOut, resample='bilinear') 
plot(r1rsmp)

###


for (k in 1:nrow(YRS)){
  yr=YRS$YRS[k]
  print ("New Year")
  
  x<-readRDS(paste0(userdir,"/environmentaldata/SST_erdMH1sstd8day/",yr,"_SST_eastNP_erdMH1sstd8day.rds"))
  trackssel<- alllocs%>%dplyr::filter(year==yr)%>%
    dplyr::filter(lon>179.97917)
  
  datesY<-unique(trackssel$date)
  for(i in 1:length(datesY)){
    print(datesY[i])
    
    # select the date you want
    datex<-(x$time[x$time==datesY[i]])[1]
    
    # skip the date if it is not in the raster data
    if(length(datex)<1|is.na(datex)) next
    
    # make a spDataFrame for that date only
    pt<-data.frame(lon=alllocs$lon[which(alllocs$date==datesY[i])],
                   lat=alllocs$lat[which(alllocs$date==datesY[i])])
    coordinates(pt)<-cbind(alllocs$lon[which(alllocs$date==datesY[i])],
                           alllocs$lat[which(alllocs$date==datesY[i])])
    
    crs(pt)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap=180")
    
    #take the xtractogon and make it into a raster - ug
    wiz<-which(x$time==datex)
    chlaFrame<-mapFrame(x$longitude,x$latitude,x$v[,,,wiz])
    chlaFrame$x<-wrap360(chlaFrame$x)
    s<- rasterFromXYZ(xyz = chlaFrame) 
    crs(s)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap=180")
    plot(s)
    
    #image(s)
    dat_sf <- st_as_sf(pt)
    
    #Extract the wind underlying each point
    dat_sf <- dat_sf %>%
      mutate(xtract = raster::extract(s, .))
    
    alllocs$SST[which(alllocs$date==datesY[i])] <- dat_sf$xtract
    
  }
}
# chla west & east---------------------------------------------------------
alllocs$chla<-NA

for (k in 2:nrow(YRS)){
  yr=YRS$YRS[k]
  print ("New Year")
  
  x<-readRDS(paste0(userdir,"/environmentaldata/chl_erdMH1chla8day/",yr,"_chl_westNP_erdMH1chla8day.rds"))
  trackssel<- alllocs%>%dplyr::filter(year==yr)%>%
    dplyr::filter(lon<=179.97917)
  x$time1<-date(x$time)
  str(datesY[i])
  str(x$time1)
  datesY<-unique(trackssel$date)
  for(i in 1:length(datesY)){
    print(datesY[i])
    
    # select the date you want
    datex<-(x$time[x$time1==datesY[i]])[1]
    idx<-which(abs(x$time1-datesY[i]) == min(abs(x$time1 - datesY[i])))
    datex<-(x$time[idx])[1]
    
    # skip the date if it is not in the raster data
    if(length(datex)<1) next
    
    # make a spDataFrame for that date only
    pt<-data.frame(lon=alllocs$lon[which(alllocs$date==datesY[i])],
                   lat=alllocs$lat[which(alllocs$date==datesY[i])])
    coordinates(pt)<-cbind(alllocs$lon[which(alllocs$date==datesY[i])],
                           alllocs$lat[which(alllocs$date==datesY[i])])
    
    crs(pt)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap=180")
    
    #take the xtractogon and make it into a raster - ug
    wiz<-which(x$time==datex)
    chlaFrame<-mapFrame(x$longitude,x$latitude,x$chlorophyll[,,wiz])
    chlaFrame$x<-wrap360(chlaFrame$x)
    s<- rasterFromXYZ(xyz = chlaFrame) 
    crs(s)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap=180")
    plot(s)
    
    #image(s)
    dat_sf <- st_as_sf(pt)
    
    #Extract the wind underlying each point
    dat_sf <- dat_sf %>%
      mutate(xtract = raster::extract(s, .))
    
    alllocs$SST[which(alllocs$date==datesY[i])] <- dat_sf$xtract
    
  }
}

for (k in 1:nrow(YRS)){
  yr=YRS$YRS[k]
  print ("New Year")
  
  x<-readRDS(paste0(userdir,"/environmentaldata/chl_erdMH1chla8day/",yr,"_chl_eastNP_erdMH1chla8day.rds"))
  trackssel<- alllocs%>%dplyr::filter(year==yr)%>%
    dplyr::filter(lon>179.97917)
  
  datesY<-unique(trackssel$date)
  for(i in 1:length(datesY)){
    print(datesY[i])
    
    # select the date you want
    datex<-(x$time[x$time==datesY[i]])[1]
    
    # skip the date if it is not in the raster data
    if(length(datex)<1|is.na(datex)) next
    
    # make a spDataFrame for that date only
    pt<-data.frame(lon=alllocs$lon[which(alllocs$date==datesY[i])],
                   lat=alllocs$lat[which(alllocs$date==datesY[i])])
    coordinates(pt)<-cbind(alllocs$lon[which(alllocs$date==datesY[i])],
                           alllocs$lat[which(alllocs$date==datesY[i])])
    
    crs(pt)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap=180")
    
    #take the xtractogon and make it into a raster - ug
    wiz<-which(x$time==datex)
    chlaFrame<-mapFrame(x$longitude,x$latitude,x$v[,,,wiz])
    chlaFrame$x<-wrap360(chlaFrame$x)
    s<- rasterFromXYZ(xyz = chlaFrame) 
    crs(s)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_wrap=180")
    plot(s)
    
    #image(s)
    dat_sf <- st_as_sf(pt)
    
    #Extract the wind underlying each point
    dat_sf <- dat_sf %>%
      mutate(xtract = raster::extract(s, .))
    
    alllocs$SST[which(alllocs$date==datesY[i])] <- dat_sf$xtract
    
  }
}

# Bathymetry --------------------------------------------------------------
library(marmap)
stal$oid<-1:nrow(stal)
# Download bathymetric data and save on disk
bat <- getNOAA.bathy(130, -115, 18.5, 65, res = 1, keep = TRUE,antimeridian = TRUE); plot(bat)
alllocs$oid<-1:nrow(alllocs)
track1<-data.frame(lon=alllocs$lon360,lat=alllocs$lat,time=alllocs$oid)

# Get depth values for each gps tracking point
# get.depth() retrieve depth values only for gps tracking points
depth1 <- get.depth(bat, track1$lon, track1$lat, locator = FALSE, distance = FALSE) ; 

# Plot
quartz(height=14, width=12)
ggplot(depth1, aes(x=lon, y=lat))+
  geom_point(aes(colour = depth))

depth<-depth1$depth
alllocs<-cbind(alllocs,depth)

alllocs$bathyCat<-"UNK"
alllocs$bathyCat[alllocs$depth>0]<-"land"
alllocs$bathyCat[alllocs$depth==0]<-"land"
alllocs$bathy<-abs(alllocs$depth)
alllocs$bathyCat[alllocs$bathy<201]<-"shelf"
alllocs$bathyCat[alllocs$bathy>200 & alllocs$bathy<1001 ]<-"shelf break"
alllocs$bathyCat[alllocs$bathy>1000 & alllocs$bathy<3001 ]<-"slope"
alllocs$bathyCat[alllocs$bathy>3000 ]<-"oceanic"

quartz()
ggplot(alllocs, aes(x=lon360, y=lat))+
  geom_point(aes(colour = as.factor(bathyCat)))

saveRDS(alllocs,paste0(userdir,"/Analysis/compileddata/Interpolate10_BFALLAAL_10minbytrip_STAL1hr_winduv_bathy.rds"))
