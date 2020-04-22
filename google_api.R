##########################import modules & packages##########################
library(reticulate)
library(xml2)
library(rvest)
library(jsonlite)
library(sf)

np <- import("numpy")
googlemaps <- import("googlemaps")
pd <- import("pandas")
time <- import("time")
requests <- import("requests")
json <- import("json")
gmaps = googlemaps$Client(key='AIzaSyAb9IrdXcIaqOBT35bCWhgBO6J36yd7mXk')



##########################funcion def######################################
# Function for min.max normalize
nor.min.max <- function(x) {
  if (is.numeric(x) == FALSE) {
    stop("Please input numeric for x")
  }
  x.min <- min(x)
  x.max <- max(x)
  x <- (x - x.min) / (x.max - x.min)
  return (x)
}

# Function for count place 
ltd_count_num <- function(latitude,longitude,keyword,radius){
  # Geocoding an address        
  query_result = gmaps$places_nearby(keyword=keyword,location = c(latitude,longitude), radius=radius)
  return(length(query_result$results))
}

# Function for find locate lon&lan function
find_ltd <- function(locate){
  urls <- paste0("https://maps.googleapis.com/maps/api/geocode/xml?address=",locate,"&language=zh-TW&key=AIzaSyAb9IrdXcIaqOBT35bCWhgBO6J36yd7mXk&fbclid=IwAR1ruO5pKEYtG4C1x335oY3cByVSQwMTc1fUgUDtEZ1ri-onAuaFn3mn71g")
  id_link <- read_html(urls)
  lat_lon <- c(as.numeric(html_text(html_nodes(id_link,"geometry location lat"))),as.numeric(html_text(html_nodes(id_link,"geometry location lng"))))
  return(lat_lon)
}

# Function for find town place function
ltd_town_find <- function(latitude,longitude){
  urls <- paste0("https://maps.googleapis.com/maps/api/geocode/xml?latlng=",latitude,",",longitude,"&language=zh-TW&key=AIzaSyAb9IrdXcIaqOBT35bCWhgBO6J36yd7mXk&fbclid=IwAR1ruO5pKEYtG4C1x335oY3cByVSQwMTc1fUgUDtEZ1ri-onAuaFn3mn71g")
  id_link <- read_html(urls)
  nodes <- html_nodes(id_link,"result address_component")
  return(html_text(html_nodes(nodes[grep("administrative_area_level_3",html_text(nodes))],"short_name"))[1])
}

# Function for find village place
ltd_vill_find <- function(latitude,longitude){
  urls <- paste0("https://maps.googleapis.com/maps/api/geocode/xml?latlng=",latitude,",",longitude,"&language=zh-TW&key=AIzaSyAb9IrdXcIaqOBT35bCWhgBO6J36yd7mXk&fbclid=IwAR1ruO5pKEYtG4C1x335oY3cByVSQwMTc1fUgUDtEZ1ri-onAuaFn3mn71g")
  id_link <- read_html(urls)
  nodes <- html_nodes(id_link,"result address_component")
  return(html_text(html_nodes(nodes[grep("administrative_area_level_4",html_text(nodes))],"short_name"))[1])
}

# Function for get the boundary ltds of town
get_bound_ltd <- function(city,town){
  ltd = read_sf("./TOWN_MOI_1090324.shp")
  d = ltd[which(ltd$COUNTYNAME== city & ltd$TOWNNAME == town),][[8]][[1]][[1]][[1]]
  return(as.data.frame(d))
}

# Function for calculate the town ltds
get_town_ltd <- function(city,town,distance){
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
  village <- vector()
  clinc <- vector()
  hospital <- vector()
  hospital_center <- vector()
  park <- vector()
  library <- vector()
  mrt_station <- vector()
  bus_stop <- vector()
  t1<-Sys.time()
  for(i in c(1:nrow(a))){
    village[i] <- ltd_vill_find(a$Var1[i],a$Var2[i])
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
  town_info['city'] <- city
  town_info['village'] <- village
  town_info['clinc'] = clinc
  town_info['hospital'] = hospital
  town_info['hospital_center'] = hospital_center
  town_info['park'] = park
  town_info['library'] = library
  town_info['mrt_station'] = mrt_station
  town_info['bus_stop'] = bus_stop
  dep <- read.csv("2020Data_final.csv")
  dep <- dep[,-c(1,2,4,5)]
  dep['village'] <- dep[,2]
  dep['city'] <- dep[,1]
  dep <- dep[,-c(1,2)]
  df <- merge(town_info, dep)
  
  return(df)
}

# Function for draw the results
town_location_draw <- function(city,town,distance,num1,num2,num3,num4,num5,num6,num7){
  library(dplyr)
  library(leaflet)
  library(purrr)
  library(BBmisc)
  ltd = read_sf("./TOWN_MOI_1090324.shp")
  df3 <- town_ltd_info(city,town,distance,num1,num2,num3,num4,num5,num6,num7)
  bdd <- get_bound_ltd(city,town)
  df3$info <- paste(sep = "<br/>",
                    "診所個數:",df3$clinc,
                    "地區醫院個數:",df3$hospital,
                    "醫學中心個數:",df3$hospital_center,
                    "公園個數:",df3$park,
                    "圖書館個數:",df3$library,
                    "捷運站出口個數:",df3$mrt_station,
                    "公車站牌個數:",df3$bus_stop,
                    "110老化指數:",df3[,46])
  lat_d = distance*0.000009090909
  lon_d = distance*0.00001
  leaflet(df3) %>% addTiles() %>%
    addPolygons(lng = bdd$V1,
                lat = bdd$V2,
                fillOpacity = 0,
                weight = 1,
                color = "red",
                popup = ~as.factor(df3$city))%>%
    #setView(lng=find_ltd(locate)[2],lat=find_ltd(locate)[1],zoom=14)%>%
    addMarkers(lng = ~Var2, lat = ~Var1,popup = ~as.factor(df3$village),clusterOptions = markerClusterOptions())%>%
    addRectangles(
      lng1=~Var2-lon_d/2, lat1=~Var1-lat_d/2,
      lng2=~Var2+lon_d/2, lat2=~Var1+lat_d/2,
      fillOpacity = nor.min.max(rowSums(scale(df3[,c(3:11,46)])))*0.8+0.1,
      color = "green",#關於線條的顏色
      stroke = FALSE,
      group = NULL,
      popup = ~as.factor(df3$info)#%>%
    # addCircles(lng = ~Var2, lat = ~Var1, 
    #            radius = distance/2, 
    #            fillOpacity = nor.min.max(rowSums(scale(df3[,c(3:11,46)])))*0.8+0.1,
    #            color = "green",#關於線條的顏色
    #            stroke = FALSE,
    #            group = NULL,
    #            popup = ~as.factor(df3$info)
    )
  #return(leaflet)
}