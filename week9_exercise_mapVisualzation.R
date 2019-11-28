
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
#ggplot
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_ploygon(fill)



