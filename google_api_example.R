source("google_api.R", encoding = "utf-8")

# city, town, radius, clinc, hospital, hospital_center, park, library, mrt_station,bus_stop
town_location_draw("高雄市","鹽埕區",1000,100,1000,5000,300,300,500,100)

df <- town_ltd_info("高雄市","鹽埕區",1000,0,1000,5000,300,300,500,100)


leaflet() %>% addTiles() %>%
  addRectangles(
    lng1=-118.456554, lat1=34.078039,
    lng2=-118.436383, lat2=34.062717,
    fillOpacity = 0.1,
    color = "green"
    #fillColor = "transparent"
  )




