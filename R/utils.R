'
Script      : utility
Created     : May, 2016
Author(s)   : Elvis Bando
Version     : v1.0
License     : Apache License, Version 2.0
Description : Utility functions and database setup
'

#===============================================#
#  These are functions that load up the data    #
# and prepare the data for api access           #
#===============================================#

#disable scientific notation
options(scipen=999)

EnsurePackage <- function(x){
# ==============================================================================
#     Ensures that the required packages are installed, and then loads the library
#
# Args:
#   x: names of the library
#
# Returns:
#   nothing:
# ===============================================================================

x = as.character(x)
if (!require(x,character.only=TRUE))
{
  install.packages(pkgs=x,repos="http://cran.r-project.org")
  require(x,character.only=TRUE)
}
}

PrepareRR = function(libs){
  # ==============================================================================
  #     Loads the library specified
  #
  # Args:
  #   lib: list of libraries to be loaded
  #
  # Returns:
  #   nothing:
  # ===============================================================================

  for(i in 1:length(libs)){
    EnsurePackage(libs[i])

  }



}

con <- function(database_name,database_password){
  # ==============================================================================
  #     sets up connection to the MYSQL database
  #
  # Args: none
  #
  # Returns:
  #   nothing:
  # ===============================================================================

  database_name = "mfarm" #databasename
  database_password = "" #database password
  #database_password = ""
  dbConnect(MySQL(), user="root", password=database_password, dbname=database_name)
}

construct.geocode.url = function(address) {
  # ==============================================================================
  #     creates a geocoding url
  #
  # Args:
  #    address: string character/name of the location
  #
  # Returns:
  #   url: and encoded URL
  # ===============================================================================
  return.call = "json"
  sensor = "false"

  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

GeoCode = function(address,verbose=FALSE) {
  # ==============================================================================
  #   gets the geocordinates of an address provided in a url
  #
  # Args:
  #   address: url encoded
  #
  # Returns:
  #   location: lat long coordinates
  # ===============================================================================


  if(verbose) cat(address,"\n")
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    return(c(lat, lng))
  } else {
    return(c(NA,NA))
  }
}

rad2deg = function(rad) {(rad * 180) / (pi)}
deg2rad = function(deg) {(deg * pi) / (180)}

nextgps = function(lat, long, bearing, dist){
  # ==============================================================================
  #   calculates the next GPS coordinate from a given point
  #
  # Args:
  #   lat,long: latitude and longitude of origin
  #   bearing : Direction in degrees measured clockwise
  #   distance: Distance in KM fromt the point of origin
  #
  # Returns:
  #   location: lat long coordinates of the destination
  # ===============================================================================


  d = dist
  R = 6378
  long = deg2rad(long)
  lat = deg2rad(lat)
  brng = deg2rad(bearing)

  final_lat = asin(sin(lat)*cos(d/R) + cos(lat) * sin(d/R) * cos(brng) );
  final_long = long + atan2(sin(brng)*sin(d/R)*cos(lat),cos(d/R)-sin(lat)*sin(final_lat));

  lat1 = rad2deg(final_lat)
  long1 = rad2deg(final_long)
  ss = list(lat = lat1, long = long1)
  return(ss)

}

bound_box = function(coods){
  # ==============================================================================
  #   calculates the GPS coordinates of a bounding box of size dist by dist
  #
  # Args:
  #   coods: input from bound_polygon. List of bounding edges of a square polygon
  #
  # Returns:
  #   location: min and max lat, min and max long coordinates of the bounding box
  # ===============================================================================


  dft = as.data.frame(coods)

  longs = c(dft$long$firstcorner, dft$long$thirdcorner)
  lats = c(dft$lat$firstcorner, dft$lat$thirdcorner)
  data = c(min(lats), max(lats),min(longs), max(longs) )
  ssd = extent(data)
  return(ssd)
  }


bound_polygon = function(lat, long, dist){
  # ==============================================================================
  #   calculates the GPS coordinates of a bounding box of size dist by dist
  #
  # Args:
  #   lat,long: latitude and longitude of origin
  #   dist: length in KM of one edge of the bounding box
  #
  # Returns:
  #   location: min and max lat, min and max long coordinates of the bounding box
  # ===============================================================================

  startpoint = nextgps(lat, long, 90, dist/2)
  firstcorner = nextgps(startpoint$lat, startpoint$long, 0, dist/2)
  secondcorner = nextgps(firstcorner$lat, firstcorner$long, 270, dist)
  thirdcorner = nextgps(secondcorner$lat, secondcorner$long, 180, dist)
  fourthcorner = nextgps(thirdcorner$lat, thirdcorner$long, 90, dist)

  polygonia = rbind(firstcorner, secondcorner, thirdcorner, fourthcorner)

  return(polygonia)
}


unixtodate = function(unixtime){
  # ==============================================================================
  #   Converts unixtime to standard date format
  #
  # Args:
  #   unixtime: numeric, unixtime value e.g 148757747
  #
  # Returns:
  #   Date: date time in standard readable format
  # ===============================================================================
  dd = (as.POSIXct(unixtime ,origin = "1970-01-01"))
  return(dd)
}

timex = function(times, format="%d-%m-%y %H:%M"){
  # ==============================================================================
  #   Conputes date time into unix time
  #
  # Args:
  #   datetime: character date time e.g 2015.05.06
  #
  # Returns:
  #   unixtime: date time in standard readable format
  # ===============================================================================

  timest = as.numeric(as.POSIXct(times, format=format))
  return(timest)
}

splitter = function(string,ind){
  data = strsplit(string,"_")[[1]][ind]
  return(as.character(data))
}


to_numerics = function(list_cols, data){
 # ==============================================================================
  #   converts numeric data imported as factors using data.tables fread back into
  #   numeric data
  #
  # Args:
  #   lis_cols: names of columns that need to be convertedcharacter
  #   data    : Fread data
  #
  # Returns:
  #   data.table: date table with converted data
  # ===============================================================================

  data2 = cbind(data[, excl, with=FALSE], data[, lapply(.SD[, -excl, with=FALSE], as.numeric)])
  return(data2)
}


combi = function(cases, by){
  s = prod(rep(cases,by))
  return(s)
}

similar = function(df){
  test = FALSE
  nms = as.character(df)
  nm = list(nms[grep("_", nms)])
  
    if(length(nm) > 0){
      pars = c(nm, "_")
      all_nams = do.call(strsplit, pars)
      all_nams = unlist(lapply(all_nams, `[[`, 1))
      ss = sapply(all_nams, grep, nms) 
      test = any(sapply(ss, length) > 1)
      }
 
return(test) 
}

