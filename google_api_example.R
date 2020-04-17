source("google_api.R", encoding = "utf-8")

# city, town, lan&lot distnace, clinc, hospital, hospital_center, park, library, mrt_station,bus_stop
df <- town_ltd_info("高雄市","前金區",300,100,1000,5000,300,300,500,100)
