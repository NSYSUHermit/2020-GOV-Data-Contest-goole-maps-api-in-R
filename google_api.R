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
gmaps = googlemaps$Client(key='AIzaSyBwMa0N_fzP9WtCPpVufYzBfcMi6etyTMQ')

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

# Function to find nearest land price
near_price <- function(latitude,longitude){
  price <- read.csv("DATA_house.csv")
  ltd <- find_ltd()
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
  urls <- paste0("https://maps.googleapis.com/maps/api/geocode/xml?address=",locate,"&language=zh-TW&key=AIzaSyBwMa0N_fzP9WtCPpVufYzBfcMi6etyTMQ")
  id_link <- read_html(urls)
  lat_lon <- c(as.numeric(html_text(html_nodes(id_link,"geometry location lat"))),as.numeric(html_text(html_nodes(id_link,"geometry location lng"))))
  return(lat_lon)
}

# Function for find ltd house price
latlng_price <- function(lat1,lon1,city){
  price <- read.csv("house_price.csv")
  price_use <- price[which(price[,9] == city),]
  lat2 = price_use$lat
  lon2 = price_use$lon
  price_use$d = (lat1-lat2)^2+(lon1-lon2)^2
  price_use <- price_use[order(price_use$d),]
  return(mean(sort(price_use$"單價元平方公尺"[1:10])[3:8]))
}

# Function for find town place function
ltd_town_find <- function(latitude,longitude){
  urls <- paste0("https://maps.googleapis.com/maps/api/geocode/xml?latlng=",latitude,",",longitude,"&language=zh-TW&key=AIzaSyBwMa0N_fzP9WtCPpVufYzBfcMi6etyTMQ")
  id_link <- read_html(urls)
  nodes <- html_nodes(id_link,"result address_component")
  return(html_text(html_nodes(nodes[grep("administrative_area_level_3",html_text(nodes))],"short_name"))[1])
}

# Function for find village place
ltd_vill_find <- function(latitude,longitude){
  urls <- paste0("https://maps.googleapis.com/maps/api/geocode/xml?latlng=",latitude,",",longitude,"&language=zh-TW&key=AIzaSyBwMa0N_fzP9WtCPpVufYzBfcMi6etyTMQ")
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
    if(num1 == 0){clinc[i] = 0}
    else{clinc[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"clinic",num1)}
    if(num2 == 0){hospital[i] = 0}
    else{hospital[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"hospital",num2)}
    if(num3 == 0){hospital_center[i] = 0}
    else{hospital_center[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"hospital center",num3)}
    if(num4 == 0){park[i] = 0}
    else{park[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"park",num4)}
    if(num5 == 0){library[i] = 0}
    else{library[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"library",num5)}
    if(num6 == 0){mrt_station[i] = 0}
    else{mrt_station[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"mrt station",num6)}
    if(num7 == 0){bus_stop[i] = 0}
    else{bus_stop[i] <- ltd_count_num(a$Var1[i],a$Var2[i],"bus stop",num7)}
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
  # combine the population data
  dep <- read.csv("2020Data_final.csv")
  dep <- dep[,-c(1,2,4,5)]
  dep['village'] <- dep[,2]
  dep['city'] <- dep[,1]
  dep <- dep[,-c(1,2)]
  df <- merge(town_info, dep)
  # combine the house prices data
  df$price <- 0
  for(j in c(1:nrow(df))){df$price[j] <- latlng_price(df$Var1[j],df$Var2[j],df$city[j])}
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
  
  df3$index <- 0.3*(nor.min.max(df3$clinc+df3$hospital+df3$hospital_center+df3$park+df3$library+df3$mrt_station+df3$bus_stop)+1.5)/3+(3*df3[,14]+7*df3[,16])*0.003-0.05*(nor.min.max(df3$price)+1.5)
  df3$info <- c("")
  df3$info <- paste0(df3$info,"<font size='4'><font color='red'>區域指標:",df3$index,"</font></font><br/>")
  if(num1 != 0){df3$info <- paste0(df3$info,"診所個數:",df3$clinc,"<br/>")}
  if(num2 != 0){df3$info <- paste0(df3$info,"地區醫院個數:",df3$hospital,"<br/>")}
  if(num3 != 0){df3$info <- paste0(df3$info,"醫學中心個數:",df3$hospital_center,"<br/>")}
  if(num4 != 0){df3$info <- paste0(df3$info,"公園個數:",df3$park,"<br/>")}
  if(num5 != 0){df3$info <- paste0(df3$info,"圖書館個數:",df3$library,"<br/>")}
  if(num6 != 0){df3$info <- paste0(df3$info,"捷運站出口個數:",df3$mrt_station,"<br/>")}
  if(num7 != 0){df3$info <- paste0(df3$info,"公車站牌個數:",df3$bus_stop,"<br/>")}
  df3$info <- paste0(df3$info,"110老化指數:",df3[,46])
  
  
  lat_d = distance*0.000009090909
  lon_d = distance*0.00001
  
  rc2 <- colorRampPalette(colors = c("white", "green"), space = "Lab")(50)
  mypal <- colorNumeric(palette =rc2, domain = df3$index)
  
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
      fillOpacity = df3$index,
      color = "blue",#關於線條的顏色
      fillColor =  ~mypal(df3$index),
      popup = ~as.factor(df3$info)
    )%>%
    addLegend(position = "bottomright", pal = mypal, values = df3$index,title = "選址條件指標",
              opacity = 1)
  #return(leaflet)
}