source("google_api.R", encoding = "utf-8")

# city, town, radius, clinc, hospital, hospital_center, park, library, mrt_station,bus_stop
town_location_draw("高雄市","鹽埕區",500,100,1000,5000,300,300,500,100)

df <- town_ltd_info("高雄市","鹽埕區",500,100,1000,5000,300,300,500,100)




