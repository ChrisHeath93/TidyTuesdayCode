
library(tidyverse)
library(rnaturalearth)


# Get the Data
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')


# Zoom to relevant area by limiting lat values, select lat and long
sf_trees <- filter(sf_trees, latitude<40)
sf_trees <- filter(sf_trees, latitude>37.6)
sf_trees <- dplyr::select(sf_trees, contains ("itude"))


#Plot lat and long values
ggplot(data = sf_trees, aes(y = latitude, x = longitude)) +
  geom_point(color = 'darkgreen', alpha = 1/50, size = 0.1) +
  coord_fixed() +
  theme_void()

#Get map data, zoom on relevant area
USA <- ne_countries(scale = "medium", returnclass = "sf")
class(USA)
sites <- data.frame(longitude = c(-122.36, -122.5), latitude = c(37.6, 40))


#Combine map data with existing lat and long values
ggplot(data = USA) +
  geom_sf() +
  geom_point(data = sf_trees, aes(x = longitude, y = latitude), color = 'darkgreen', alpha = 1/50, size = 0.1) +
  coord_sf(xlim = c(-122.36, -123), ylim = c(37.6, 38), expand = FALSE) +
  theme_void()




                                     