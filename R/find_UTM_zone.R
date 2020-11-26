find_UTM_zone <- function(longitude, latitude) {
  
  #https://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm/48838123#48838123
  library(dplyr)
  library(sp)
  library(rgdal)
  library(tibble) 
  # Special zones for Svalbard and Norway
  if (latitude >= 72.0 && latitude < 84.0 ) 
    if (longitude >= 0.0  && longitude <  9.0) 
      return(31);
  if (longitude >= 9.0  && longitude < 21.0)
    return(33)
  if (longitude >= 21.0 && longitude < 33.0)
    return(35)
  if (longitude >= 33.0 && longitude < 42.0) 
    return(37)
  
  (floor((longitude + 180) / 6) %% 60) + 1
}
