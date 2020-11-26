library(raster)
library(data.table)
library(sp)
library(rgdal)

#Rachael Orben Nov 26, 2020
#Makes geoTiffs from Global Fishing Watch daily csv files
#geoTiffs are trimmed to the study area at line #40

# Directory with daily GFW .csv files
if(Sys.info()[6]=="rachaelorben") {GFWDataDir<-"/Volumes/GoogleDrive/My Drive/GFW fishing effort/fishing_effort_byvessel/";
gitdir<-"/Users/rachaelorben/git_repos/animal-fishing-vessel-encounters/"} ##RAO

Files<-list.files(paste0(GFWDataDir,"daily_csvs/"),pattern = ".csv",full.names = F,recursive = F)

files.sources = list.files(paste0(gitdir,"R"), full.names = TRUE)
sapply(X = files.sources, FUN=source)

#Global fishing watch lat/lon bins
#lat_bin: the southern edge of the grid cell, 
#in 10ths of a degree -- 101 is the grid cell with a southern edge at 1.01 degrees north
#lon_bin: the western edge of the grid cell, 
#in 10ths of a degree -- 101 is the grid cell with a western edge at 1.01 degrees east

for (i in 1:length(Files)){ #each iteration takes ~50sec
start.time <- Sys.time()
filename=Files[i]
print(filename)
Data<-fread(file=paste0(GFWDataDir,"daily_csvs/",filename),stringsAsFactors=FALSE) #35seconds!!!
Data$lat=(Data$lat_bin)*.10
Data$lon=wrap360((Data$lon_bin)*.10)
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
