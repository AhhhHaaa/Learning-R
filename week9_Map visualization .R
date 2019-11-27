# ggmap with ggplot2 (static map data) ------------------------------------

install.packages("ggmap")
library(tidyverse)
library(ggmap)
#plot us and with the boundary box 
us <- c(left = -120, bottom = 25.75, right = -67, top = 49)
get_stamenmap(us, zoom = 5) %>% ggmap()
#stamen map will reurn a terrain map

#plot uk map
uk <- c(left = -10, bottom = 49,  right = 2, top = 59)
get_stamenmap(uk, zoom = 6, maptype = "toner-lite") %>% ggmap()

#plot sheffeild map
sheffield <- c(left = -1.49,  bottom =  53.37, right = -1.45, top = 53.39)
get_stamenmap(sheffield,  zoom = 15, maptype = "toner-lite") %>% ggmap()

#load in sheffield CCTV dataset
sheffieldCameras <- read.csv("sheffield_CCTV_Locations.csv")
sheffield <- c(left = -1.49,  bottom =  53.37, right = -1.45, top = 53.39)
get_stamenmap(sheffield,  zoom = 15, maptype = "toner-lite") %>% 
  ggmap() +
  geom_point(data = sheffieldCameras, aes(x = lon, y = lat), colour = "red") +
  labs(title = "position of CCTV camera in sheffield city centre in 2017",
       caption = "Data : sheffield cit council 2017")

world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgray", colour = "white") +
  theme(panel.background = element_blank()) +
  labs(title = "World map", caption = "maps package R")

eu.countries <- c( "Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus",
" Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "UK"
)
eu.map <- map_data("world", region = eu.countries)
ggplot(eu.map, aes(x = long, y = lat, group = group )) +
  geom_polygon(fill = "lightgrey", colour = "black") +
  labs(title = "EU map", caption = "maps package, R") +
  theme(panel.background = element_blank()) + #to remove the background
  coord_map()

#try out the asia map
asia.four <- c("Taiwan", "Japan", "South Korea", "china")
asia.map <- map_data("world", region = asia.four)
ggplot(asia.map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgrey", colour = "black") +
  labs(title = "Asia map", caption = "maps package, R") +
  coord_map()

# fill attributes to polygon ----------------------------------------------
life.exp.map <- read_csv("WHO map.csv")
ggplot(life.exp.map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = lifeExp), colour = "White") +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Life\nExpectancy", 
       title = "World map coloured by life expentancy in 2015", 
       caption = "Data source: WHO")

# Interactive map using leaflet -------------------------------------------
#leaflet package required
install.packages("leaflet")
library("leaflet")

#create map centred on New York
leaflet() %>% 
  addTiles() %>% #add the map info to plot
  setView(lat = 40.7128, lng = -73.0059, zoom = 7) #latitude and longtitude

#create  map centred on Taiwan
leaflet() %>% 
  addTiles() %>%  
  setView(lat = 24.105497, lng = 121.197366, zoom = 7)

#creat map centre on malysia
leaflet() %>% 
  addTiles() %>% 
  setView(lat = 3.105497, lng = 101.197366, zoom = 7)

#add infomation by addMakers()
leaflet() %>% 
  addTiles() %>%
  addMarkers(lat = 40.748816, lng = -73.985428, 
          popup = "Empire state Building")
