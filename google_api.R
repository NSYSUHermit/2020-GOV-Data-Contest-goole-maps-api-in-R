##########################import modules & packages##########################
library(reticulate)
library(xml2)
library(rvest)
library(jsonlite)
library(sf)
library(shinyWidgets)
library(data.table)
apiKey ="AIzaSyBwMa0N_fzP9WtCPpVufYzBfcMi6etyTMQ"
googlemaps <- import("googlemaps")
gmaps = googlemaps$Client(key=apiKey)

#ltd = read_sf("D:/$/data/rscript/TOWN_MOI_1090324.shp")
ltd = readRDS("ltd.rds")
#price = fread("D:/$/data/rscript/house_price1.csv")[,c(6:9,11)]
price = readRDS("price.rds")
city = data.frame("city" = c("宜蘭縣","花蓮縣","金門縣","南投縣","屏東縣","苗栗縣","桃園市","高雄市","基隆市","連江縣",
                             "雲林縣","新北市","新竹市","新竹縣","嘉義市","嘉義縣","彰化縣","臺中市","臺北市","臺東縣",
                             "臺南市","澎湖縣"),
                  "num" = c(1,4,7,10,13,16,19,21,2,5,8,11,14,17,20,22,3,6,9,12,15,18),StringsAsFactors=FALSE)
town = function(x){
  as.character(data.frame(table(ltd$TOWNNAME[which(ltd$COUNTYNAME==paste0(x))]))[,1])
}

name = c("一般診所","地區醫院","醫學中心","公園","圖書館","捷運站","公車站")


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
  urls <- paste0("https://maps.googleapis.com/maps/api/geocode/xml?address=",locate,"&language=zh-TW&key=",apiKey)
  id_link <- read_html(urls)
  lat_lon <- c(as.numeric(html_text(html_nodes(id_link,"geometry location lat"))),as.numeric(html_text(html_nodes(id_link,"geometry location lng"))))
  return(lat_lon)
}

# Function for find town place function
ltd_town_find <- function(latitude,longitude){
  urls <- paste0("https://maps.googleapis.com/maps/api/geocode/xml?latlng=",latitude,",",longitude,"&language=zh-TW&key=",apiKey)
  id_link <- read_html(urls)
  nodes <- html_nodes(id_link,"result address_component")
  return(html_text(html_nodes(nodes[grep("administrative_area_level_3",html_text(nodes))],"short_name"))[1])
}

# Function for find village place
ltd_vill_find <- function(latitude,longitude){
  urls <- paste0("https://maps.googleapis.com/maps/api/geocode/xml?latlng=",latitude,",",longitude,"&language=zh-TW&key=",apiKey)
  id_link <- read_html(urls)
  nodes <- html_nodes(id_link,"result address_component")
  return(html_text(html_nodes(nodes[grep("administrative_area_level_4",html_text(nodes))],"short_name"))[1])
}

# Function for get the boundary ltds of town
get_bound_ltd <- function(city,town){
  d = ltd[which(ltd$COUNTYNAME== city & ltd$TOWNNAME == town),][[8]][[1]][[1]][[1]]
  return(as.data.frame(d))
}

# Function for calculate the town ltds
get_town_ltd <- function(city,town,distance){
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

# Function for find ltd house price
latlng_price <- function(lat1,lon1,city){
  #price_use <- price[which(price[,6] == city & str_detect(price$township,township)==TRUE),]
  price_use <- price[which(price$town ==city),]
  price_use$d = apply(data.frame(1:nrow(price_use)),2,function(x){return((lon1-price_use$lon[x])^2+(lat1-price_use$lat[x])^2)})
                #(lon1-price_use$lon)^2+(lat1-price_use$lat)^2
  price_use <- price_use[order(price_use$d),]
  return(mean(price_use$每坪價格[1:5]))
}

# Function for calculate the town informations
town_ltd_info <- function(city,town,distance,num1,num2,num3,num4,num5,num6,num7,num8){
  town_info <- get_town_ltd(city,town,distance)
  village = clinc =hospital =hospital_center = park=library=mrt_station=bus_stop=nursing =vector()
  for(i in c(1:nrow(town_info))){
    village[i] <- ltd_vill_find(town_info$Var1[i],town_info$Var2[i])
    if(num1 == 0){clinc[i] = 0}
    else{clinc[i] <- ltd_count_num(town_info$Var1[i],town_info$Var2[i],"clinic",num1)}
    if(num2 == 0){hospital[i] = 0}
    else{hospital[i] <- ltd_count_num(town_info$Var1[i],town_info$Var2[i],"hospital",num2)}
    if(num3 == 0){hospital_center[i] = 0}
    else{hospital_center[i] <- ltd_count_num(town_info$Var1[i],town_info$Var2[i],"hospital center",num3)}
    if(num4 == 0){park[i] = 0}
    else{park[i] <- ltd_count_num(town_info$Var1[i],town_info$Var2[i],"park",num4)}
    if(num5 == 0){library[i] = 0}
    else{library[i] <- ltd_count_num(town_info$Var1[i],town_info$Var2[i],"library",num5)}
    if(num6 == 0){mrt_station[i] = 0}
    else{mrt_station[i] <- ltd_count_num(town_info$Var1[i],town_info$Var2[i],"mrt station",num6)}
    if(num7 == 0){bus_stop[i] = 0}
    else{bus_stop[i] <- ltd_count_num(town_info$Var1[i],town_info$Var2[i],"bus stop",num7)}
    if(num8 == 0){nursing[i] = 0}
    else{nursing[i] <- ltd_count_num(town_info$Var1[i],town_info$Var2[i],"nursing",num8)}
  }
  town_info['city'] <- city
  town_info['village'] <- village
  town_info['clinc'] = clinc
  town_info['hospital'] = hospital
  town_info['hospital_center'] = hospital_center
  town_info['park'] = park
  town_info['library'] = library
  town_info['mrt_station'] = mrt_station
  town_info['bus_stop'] = bus_stop
  town_info$nursing =nursing
  
  #dep <- read.csv("D:/$/data/rscript/2020Data_final.csv")
  dep = readRDS("dep.rds")
  dep <- dep[,-c(1,2,4,5)]
  dep['village'] <- dep[,2]
  dep['city'] <- dep[,1]
  #colnames(dep)[1:2] = c('city','village')
  dep <- dep[,-c(1,2)]
  df <- merge(town_info, dep)
  return(df)
}

# Function for draw the results
# town_location_draw <- function(city,town,distance,num1,num2,num3,num4,num5,num6,num7,num8){
#   library(dplyr)
#   library(leaflet)
#   library(purrr)
#   library(BBmisc)
#   df3 <- town_ltd_info(city,town,distance,num1,num2,num3,num4,num5,num6,num7,num8)
#   bdd <- get_bound_ltd(city,town)
#   num= apply(df3,1,function(x){latlng_price(df3$Var1[x],df3$Var2[x],city)})
#   df3$info <- paste(sep = "<br/>",
#                     "診所個數:",df3$clinc,
#                     "地區醫院個數:",df3$hospital,
#                     "醫學中心個數:",df3$hospital_center,
#                     "公園個數:",df3$park,
#                     "圖書館個數:",df3$library,
#                     "捷運站出口個數:",df3$mrt_station,
#                     "公車站牌個數:",df3$bus_stop,
#                     "範圍內平均地價:",num,
#                     "養老院數量:",df3$nursing,
#                     "110老化指數:",df3[,46])
#   lat_d = distance*0.000009090909
#   lon_d = distance*0.00001
#   leaflet(df3) %>% addTiles() %>%
#     addPolygons(lng = bdd$V1,
#                 lat = bdd$V2,
#                 fillOpacity = 0,
#                 weight = 1,
#                 color = "red",
#                 popup = ~as.factor(df3$city))%>%
#     #setView(lng=find_ltd(locate)[2],lat=find_ltd(locate)[1],zoom=14)%>%
#     addMarkers(lng = ~Var2, lat = ~Var1,popup = ~as.factor(df3$village),clusterOptions = markerClusterOptions())%>%
#     addRectangles(
#       lng1=~Var2-lon_d/2, lat1=~Var1-lat_d/2,
#       lng2=~Var2+lon_d/2, lat2=~Var1+lat_d/2,
#       fillOpacity = nor.min.max(rowSums(scale(df3[,c(5:12)])))*0.8+0.1,
#       color = "green",#關於線條的顏色
#       stroke = FALSE,
#       group = NULL,
#       popup = ~as.factor(df3$info))
# }
