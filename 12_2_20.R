library(tidyverse)
library(rnaturalearth)


#Read in and view data
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')
View(hotels)



#Calculate geographical info for total number of visitors by country, prepare for join to map data
Country_totals <- count(hotels, country)
Country_totals <- Country_totals %>%
 rename(iso_a3 = country) 

#Add in map data and join to geographical info
Map <- ne_countries(scale = "medium", returnclass = "sf")
class(Map)
Map <- full_join(Map, Country_totals, by = "iso_a3")

#Visualisation
ggplot(data = Map) +
  geom_sf() +
  geom_sf(aes(fill = n))


#Split into resort and city hotel data, get counts of visitors by country for each
Resort <- filter(hotels, hotel == "Resort Hotel")
City <- filter(hotels, hotel == "City Hotel")

Resort_totals <- count(Resort, country) %>%
  rename(nresort = n)

city_totals <- count(City, country) %>%
  rename(ncity = n)



#Join resort and city data, calculate disparity, and remove those with minimal disparity
Cityvsresort <- full_join(Resort_totals, city_totals, by = "country") %>%
  replace_na(list(nresort = 0, ncity = 0)) %>%
  mutate(Disparity = ncity - nresort) %>%
  filter((Disparity > 100) | (Disparity < -100)) %>%
  arrange(desc(Disparity)) %>%
  mutate(Preference = ifelse(Disparity < 0, "Resorts", "Cities")) %>%
  filter(country != "NULL") %>%
  mutate(country = replace(country, country == "PRT", "Portugal")) %>%
  mutate(country = replace(country, country == "FRA", "France")) %>%
  mutate(country = replace(country, country == "DEU", "Germany")) %>%
  mutate(country = replace(country, country == "ITA", "Italy")) %>%
  mutate(country = replace(country, country == "BEL", "Belgium")) %>%
  mutate(country = replace(country, country == "BRA", "Brazil")) %>%
  mutate(country = replace(country, country == "NLD", "Netherlands")) %>%
  mutate(country = replace(country, country == "CHE", "Switzerland")) %>%
  mutate(country = replace(country, country == "GBR", "Great Britain")) %>%
  mutate(country = replace(country, country == "IRL", "Ireland")) %>%
  mutate(country = replace(country, country == "CN", "China")) %>%
  mutate(country = replace(country, country == "GRC", "Greece")) %>%
  mutate(country = replace(country, country == "MAR", "Morocco")) %>%
  mutate(country = replace(country, country == "KOR", "Korea")) %>%
  mutate(country = replace(country, country == "CZE", "Czechia")) %>%
  mutate(country = replace(country, country == "LUX", "Luxembourg")) %>%
  mutate(country = replace(country, country == "HUN", "Hungary")) %>%
  mutate(country = replace(country, country == "FIN", "Finland")) %>%
  mutate(country = replace(country, country == "ROU", "Romania")) %>%
  mutate(country = replace(country, country == "JPN", "Japan")) %>%
  mutate(country = replace(country, country == "TUR", "Turkey")) %>%
  mutate(country = replace(country, country == "AUS", "Australia")) %>%
  mutate(country = replace(country, country == "POL", "Poland")) %>%
  mutate(country = replace(country, country == "RUS", "Russia")) %>%
  mutate(country = replace(country, country == "DNK", "Denmark")) %>%
  mutate(country = replace(country, country == "NOR", "Norway")) %>%
  mutate(country = replace(country, country == "AGO", "Angola")) %>%
  mutate(country = replace(country, country == "SWE", "Sweden")) %>%
  mutate(country = replace(country, country == "ESP", "Spain")) %>%
  mutate(country = replace(country, country == "CHN", "China")) %>%
  mutate(country = replace(country, country == "AUT", "Austria")) %>%
  filter(country != "China")


#Visualise
ggplot(data = Cityvsresort, aes(x = reorder (country, -Disparity), y = Disparity, label = Disparity)) +
  geom_bar(stat = 'identity', aes(fill = Preference)) +
  coord_flip() +
  labs(title = "Disparity between number of visitors for a city hotel versus a resort hotel by visitor nationality", x = "Visitor nationality") 
