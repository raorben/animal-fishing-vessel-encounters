library(raster)
library(data.table)
library(sp)
library(rgdal)

wrap360 = function(lon) {
  lon360<-ifelse(lon<0,lon+360,lon)
  return(lon360)
}

# Directory with daily GFW .csv files
if(Sys.info()[6]=="rachaelorben") {GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/" ##RAO
Files<-list.files(paste0(GFWDataDir,"daily_csvs/2012/"),pattern = ".csv",full.names = F,recursive = F)}

#Global fishing watch lat/lon bins
#lat_bin: the southern edge of the grid cell, 
#in 100ths of a degree -- 101 is the grid cell with a southern edge at 1.01 degrees north
#lon_bin: the western edge of the grid cell, 
#in 100ths of a degree -- 101 is the grid cell with a western edge at 1.01 degrees east

for (i in 1:length(Files)){ #each iteraction takes ~50sec
start.time <- Sys.time()
filename=Files[i]
print(filename)
Data<-fread(file=paste0(GFWDataDir,"daily_csvs/2012/",filename),stringsAsFactors=FALSE) #35seconds!!!
Data$lat=(Data$lat_bin)*.010
Data$lon=wrap360((Data$lon_bin)*.010)
head(Data)

fyes<-rep(1,nrow(Data))
pts<-data.frame(lon=Data$lon,lat=Data$lat,z=fyes)
coordinates(pts)=~lon+lat
gridded(pts)=TRUE

r = rasterFromXYZ(pts,crs="+proj=longlat +datum=WGS84")
new.extent <- extent(115,250, 15,65) #north pacific albatross extent
rr<-crop(x = r, y = new.extent)
rr[is.na(rr)==TRUE]<-0
shortdte<-gsub("[^0-9]","",filename)
writeRaster(rr,paste0(GFWDataDir,"daily_geotiffs/",shortdte,".tiff"),"GTiff")
removeTmpFiles(.03)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
}

rastYN_321<-rr
# Geotiff_boatnum ---------------------------------------------------------

#adt_list<-NULL
for (i in 1:length(Files)){ #each iteraction takes ~50sec
  start.time <- Sys.time()
  filename=Files[i]
  print(filename)
  Data<-fread(file=paste0(GFWDataDir,"daily_csvs/",filename),stringsAsFactors=FALSE) #35seconds!!!
  Data$lat=(Data$lat_bin)*.010
  Data$lon=wrap360((Data$lon_bin)*.010)
  head(Data)
  
  pts<-data.frame(lon=Data$lon,lat=Data$lat,z=Data$mmsi_present)
  coordinates(pts)=~lon+lat
  gridded(pts)=TRUE
  
  r = rasterFromXYZ(pts,crs="+proj=longlat +datum=WGS84")
  new.extent <- extent(115,250, 15,65) #north pacific albatross extent
  dayR<-crop(x = r, y = new.extent)
  dayR[is.na(dayR)==TRUE]<-0
  shortdte<-gsub("[^0-9]","",filename)
  writeRaster(dayR,paste0(GFWDataDir,"daily_geotiffs_hr01_boatnum/",shortdte,"_BoatNum.tiff"),"GTiff",overwrite=TRUE)
  removeTmpFiles(.03)
  #adt_list <- c(adt_list, list(dayR))
  #GFW12 <- stack(adt_list)
  #saveRDS(GFW12, file = paste0(userdir,"/Analysis/compileddata/GFW12_rasterstack_01.rda"))

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}
