### Tidy Tuesday: Bechdel Test ###
### By: Jasmin ###

### Load Libraries ###
library(tidyverse)
library(tidytuesdayR)
library(here)
library(ggeasy)
library(ggplot2)
library(tvthemes)

### Load Data ###
raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')


view(raw_bechdel)
view(movies)

### Analyze Data ###
movies_data <- movies %>%
  select(title, binary, year) %>% 
  inner_join(raw_bechdel)

view(movies_data)  

### Select Data ###
bechdel <- raw_bechdel %>%
  pivot_wider(names_from = year,
              values_from = rating) %>% #pivot data to show year and rating
  pivot_longer(cols = "1888":"2021", #all the years used in data sheet
               names_to = "year",
               values_to = "rating") %>%
  filter(between(year, 1990, 2000)) %>% #filter for the 90s
  drop_na() %>% #dropping NAs from data sets 
  select(year, rating) %>% #using year and rating data
  group_by(year) %>% #group by year
  summarise(mean_rating = mean(rating)) #using the mean for the rating 

### Graph Data ###
ggplot(bechdel, aes(x=year, y=mean_rating, group = 1)) + #x and y axis 
  geom_smooth() + #smooth will show the trend, cha cha real smooth
  theme_rickAndMorty() + #ri ri rick, what are you doing?? is this a good idea? the graph is too loud! "yeah morty dont worry about it its the last week!"
  labs(color = "rating", 
       x = "Year",
       y = "Rating",
       title = "Bechdel Ratings",
       subtitle = "feat. The 90s",
       caption = "Source: Five Thirty Eight")
  

