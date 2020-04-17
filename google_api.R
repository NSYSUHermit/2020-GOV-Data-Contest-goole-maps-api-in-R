##########################import modules & packages##########################
library(reticulate)
library(xml2)
library(rvest)
library(jsonlite)

np <- import("numpy")
googlemaps <- import("googlemaps")
pd <- import("pandas")
time <- import("time")
requests <- import("requests")
json <- import("json")
gmaps = googlemaps$Client(key='AIzaSyAb9IrdXcIaqOBT35bCWhgBO6J36yd7mXk')


##########################funcion def######################################

# Function for count place 
ltd_count_num <- function(latitude,longitude,keyword,radius){
  # Geocoding an address        
  query_result = gmaps$places_nearby(keyword=keyword,location = c(latitude,longitude), radius=radius)
  return(length(query_result$results))
}


# Function for find town place function
ltd_town_find <- function(latitude,longitude){
  urls <- paste0("https://maps.googleapis.com/maps/api/geocode/json?latlng=",latitude,",",longitude,"&language=zh-TW&key=AIzaSyAb9IrdXcIaqOBT35bCWhgBO6J36yd7mXk&fbclid=IwAR1ruO5pKEYtG4C1x335oY3cByVSQwMTc1fUgUDtEZ1ri-onAuaFn3mn71g")
  id_link <- read_html(urls)
  my.df <- fromJSON(html_text(id_link))
  return(substring(my.df$results$formatted_address[4], 9, 11))
}

# Function for find village place
ltd_vill_find <- function(latitude,longitude){
  urls <- paste0("https://maps.googleapis.com/maps/api/geocode/json?latlng=",latitude,",",longitude,"&language=zh-TW&key=AIzaSyAb9IrdXcIaqOBT35bCWhgBO6J36yd7mXk&fbclid=IwAR1ruO5pKEYtG4C1x335oY3cByVSQwMTc1fUgUDtEZ1ri-onAuaFn3mn71g")
  id_link <- read_html(urls)
  my.df <- fromJSON(html_text(id_link))
  return(substring(my.df$results$formatted_address[4], 12, 15))
}


# Function for calculate the town ltds
get_town_ltd <- function(city,town,distance){
  library(sf)
  ltd = read_sf("./TOWN_MOI_1090324.shp")
  ## lat & lon difference setting 
  lat_d = distance*0.000009090909
  lon_d = distance*0.00001
  ## get town ltd point
  d = ltd[which(ltd$COUNTYNAME== city & ltd$TOWNNAME == town),][[8]][[1]][[1]][[1]]
  ## count all range town ltd point
  lon_line <- seq(min(d[,1]),max(d[,1]),by = lon_d/2)[seq(2,length(seq(min(d[,1]),max(d[,1]),by = lon_d/2)),2)]
  lat_line <- seq(min(d[,2]),max(d[,2]),by = lat_d/2)[seq(2,length(seq(min(d[,2]),max(d[,2]),by = lat_d/2)),2)]
  locations <- expand.grid(lat_line,lon_line)
  loc_num = vector()
  t1<-Sys.time()
  ## check ltd points in town or not
  for(i in c(1:nrow(locations))){
    loc_num = c(loc_num,ltd_town_find(locations$Var1[i],locations$Var2[i]) == town)
    t2 <- Sys.time()
    time <- t2-t1
    print(paste0("Town Ltd Completion:",i/nrow(locations),". Time cost:",time))
  }
  loc_num[is.na(loc_num)]= FALSE
  ## return true town ltd point
  return(locations[which(loc_num == TRUE),])
}


# Function for calculate the town informations
town_ltd_info <- function(city,town,distance,num1,num2,num3,num4,num5,num6,num7){
  a <- get_town_ltd(city,town,distance)
  clinc <- vector()
  hospital <- vector()
  hospital_center <- vector()
  park <- vector()
  library <- vector()
  mrt_station <- vector()
  bus_stop <- vector()
  t1<-Sys.time()
  for(i in c(1:nrow(a))){
    clinc[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"clinic",num1)
    hospital[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"hospital",num2)
    hospital_center[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"hospital center",num3)
    park[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"park",num4)
    library[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"library",num5)
    mrt_station[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"mrt station",num6)
    bus_stop[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"bus stop",num7)
    t2 <- Sys.time()
    time <- t2-t1
    print(paste0("Completion:",i/nrow(a),". Time cost:",time))
  }
  town_info <- a
  town_info['clinc'] = clinc
  town_info['hospital'] = hospital
  town_info['hospital_center'] = hospital_center
  town_info['park'] = park
  town_info['library'] = library
  town_info['mrt_station'] = mrt_station
  town_info['bus_stop'] = bus_stop
  return(town_info)
}