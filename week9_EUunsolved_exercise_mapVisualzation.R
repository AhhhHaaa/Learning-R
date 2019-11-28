
# Exercise 1 get_stamenmap ------------------------------------------------
#required packages
install.packages("ggmap")
library(tidyverse)
library(ggmap)
#information school coordinates(53.3811602, -1.4821673)
#top: Broad Lane (53.3829996, -1.4797801)
#left: Upper hanover street(53.3787742, -1.4862576)
#bottom: weststreet(53.3809558, -1.4778744)
#right: mapping street(53.3813965, -1.4815208)

sheff_information <- c(top = 53.3829996, left = -1.4861673,
                 right = -1.4715208, bottom = 53.3809558)
#coordinates need to be corrected
get_stamenmap(sheff_information, zoom = 17) %>% 
  ggmap() + ggtitle("Centred sheffield infomation school") +
  labs(x = "Longtitude", y = "latitude")
ggsave("sheffield infomation school.png")
##if the aim's to plot a smaller scope of map,
#it needs to have the bigger zoom(eg. zoom = 13)

#load the camera data
sheffieldCameras <- read.csv("sheffield_CCTV_Locations.csv")

  
get_stamenmap(sheff_information, zoom = 18, maptype = "toner-lite") %>% 
  ggmap() + 
      #using geom_point to add the camera on the map 
  geom_point(data = sheffieldCameras, aes(x = lon, y = lat), colour = "red") +
  ggtitle("Centred sheffield infomation school") +
  labs(x = "Longtitude", y = "latitude")

ggsave("cameras surrounded info_school.png")


# Exercise2 Choropleth --------------------------------------------------------------
#load the map data
world_map <- map_data("world")
#load WHO data 
life.exp.map <- read_csv("WHO map.csv")
#plot the baisc world map
p2 <- ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey", colour = "white") +
  theme(panel.background = element_blank()) +
  labs(title = "world map", caption = "maps package, R")

#plot life.exp 

p1 <- ggplot(life.exp.map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = lifeExp), colour = "white") +
  scale_fill_viridis_c() +
  theme_void() 

#
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgrey") +
  geom_polygon(aes(fill = as.factor(life.exp.map$lifeExp)))

ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey", colour = "white") 
###test
ggplot() +
  geom_polygon(data = world_map,
                        aes(x = long, y = lat, group = group),
                            fill = "lightgrey") +
  geom_polygon(data = life.exp.map, aes(x = long, y = lat, group = group,
                                        fill = life.exp.map$lifeExp)) +
  scale_fill_viridis_c() +
  labs(fill = "Life Expectancy") +
  theme_void() 



#2 layers(world map X WHo) of ggplots 
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgrey",colour = "white") +
  geom_polygon(data = life.exp.map, 
               aes(x = long, y = lat, group = group,
                   fill = life.exp.map$lifeExp)) +
  labs(fill = "Life Expectancy") +
  scale_fill_viridis_c() + 
  theme_void()
#2.1 Extra: draw Life expectancy on EU.map

eu.countries <- c( "Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus",
" Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "UK"
)
EU.map = map_data("world", eu.countries)
life.exp.map %>% 
  select(reigion == eu.countries)

ggplot(EU.map, aes(x = long, y = lat, group = group)) +
  geom_polygon(data = life.exp.map, aes(fill = life.exp.map$lifeExp, 
               x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgrey", colour = "white") 
  
  
  geom_polygon(data = Eu.,
               aes(fill = life.exp.map$lifeExp, x = long, y = lat)) +
  scale_fill_viridis_c() +
  labs(fill = "Life Expectancy")
##failed in plotting Eu with WHo dataset 

# exercise3: Interactive maps by leaflet()-------------------------------------------------------------
library(leaflet)
leaflet() %>% 
  addTiles() %>% 
  setView(lng = , lat = )
  
#exercise 3.1
library(htmltools)
#using sheff.cam as abbreviation
sheff.cam <- sheffieldCameras
#it's inspired by "https://stackoverflow.com/questions/31562383/using-leaflet-library-to-output-multiple-popup-values"
leaflet(df) %>% 
  addTiles() %>% 
  addMarkers(lng =  sheff.cam$lon, lat = sheff.cam$lat, 
             popup =  sheff.cam$Location,
             )
#exercise 3.2 (Extra):clusterOptions = makerClusterOptions()

leaflet(df) %>% 
  addTiles() %>% 
  addMarkers(lng =  sheff.cam$lon, lat = sheff.cam$lat, 
             popup =  sheff.cam$Location,
             clusterOptions = markerClusterOptions()
             )


  
  
  
