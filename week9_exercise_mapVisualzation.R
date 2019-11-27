
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

sheff_information <- c(top = 53.3829996, left = -1.4821673,
                 right = -1.4815208, bottom = 53.3809558)
#coordinates need to be corrected
get_stamenmap(sheff_information, zoom = 1) %>% ggmap()
