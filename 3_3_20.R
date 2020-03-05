library(tidyverse)
library(ggridges)
library(ggthemes)
library(gganimate)




game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')


#Extract stats for top 10 goalscorers (post 1979)
top_10 <- game_goals %>%
  filter(player == "Wayne Gretzky" | 
         player == "Jaromir Jagr" | 
         player == "Brett Hull" | 
         player == "Mike Gartner" |
         player == "Alex Ovechkin" |
         player == "Mark Messier" |
         player == "Steve Yzerman" |
         player == "Mario Lemieux" |
         player == "Teemu Selanne" |
         player == "Luc Robitaille"
         ) %>%
  group_by(player) %>%
  mutate(player_total_goals = sum(goals)) %>%
  mutate(cum_goals = cumsum(goals)) %>%
  ungroup %>%
  mutate(player = fct_reorder(player, player_total_goals)) %>%
  separate(age, sep = "-", into = c("Years", "Days"))

top_10$Years <- as.numeric(as.character(top_10$Years))
top_10$Days <- as.numeric(as.character(top_10$Days))

top_10 <- top_10 %>%
  mutate(years_days = Years * 365) %>%
  mutate(total_days = years_days + Days) %>%
  mutate(total_years = total_days / 365)

total_days <- 6700:17000

union(top_10, total_days)

ggplot(data = top_10, aes(x = total_years, y = player, fill = player)) +
  geom_density_ridges (scale = 1.5, size = 0.5, rel_min_height = 0.01) +
  theme_ridges()

animated <- ggplot(data = top_10, aes(x = total_years, y = cum_goals, color = player)) +
  geom_line() +
  transition_reveal(total_years) +
  view_follow(fixed_x = 50, NA,
              fixed_y = 1000, NA) +
  exit_shrink()
  

animate(animated)

anim_save("Hockey_players.gif")

