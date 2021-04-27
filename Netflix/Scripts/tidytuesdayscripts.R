### Tidy Tuesday: Netflix ###
### Date: 2021-04-24 ###
### By: Jasmin Abdulla ###


### Load Libraries ###
library(tidyverse)
library(here)
library(tidytuesdayR)
library(ghibli)
library(ggplot2)
library(ggeasy)

### Load Data ###
netflix <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')
view(netflix)

### Create Graphs ###
movies <- netflix %>% #selecting specific data
  select("country", "release_year") %>% #selecting country and release year
  filter(country == "United States") %>% #filtering just for United States, was complete chaos when this filter wasnt on
  drop_na() %>% #filter out NAs
  group_by(country) #group by for United States as only selected country
view(movies)

ggplot(movies, aes(x = country, y = release_year)) + #x and y axis selection
  geom_jitter() + #jitterbug plot
  theme_bw() + #tried playing with other themes but this is the cleanest one 
  labs(x = "Country", 
       y = "Release Year", 
       title = "Tidy Tuesday: Netflix", 
       subtitle = "Release Year in United States", 
       caption = "Source: Kaggle") 

  ggplot


