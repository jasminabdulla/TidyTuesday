### Tidy Tuesday ###
### Week 6: Superbowl Ads ###
### By: Jasmin Abdulla ###


### Load Libraries ###
library(tidyverse)
library(tidytuesdayR)
library(here)

### Load Data ###

youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv') #load data

youtube #view data
view(youtube) #view data

### Data ### 

YT <- youtube %>% 
  select(animals, brand, danger, funny, use_sex) %>% #selecting categories
  group_by(brand) %>% # group by brand
  rename(Animals = animals, #proper capitalization
         Brand = brand, 
         Danger = danger, 
         Humor = funny, 
         Gender = use_sex) %>% 
  pivot_longer(cols = Humor, #type of graph to pivot longer 
               names_to = "category",
               values_to = "status",
               values_transform = list(status = as.numeric)) %>% # using true/false only
  group_by(Brand) %>%  # group by brand
  filter(status ==1) %>%  # filter for only ads associated for a category
  ggplot(aes(x = Brand, # plot by brand
             fill = Brand))+ # fill by brand
  geom_bar()+ # bar plot
  coord_flip()+ # flipping for horizontal instead of vertical geom bar plot
  facet_wrap(~category,  # graphing by category
             scales = "free")+ #scales set to free
  labs(title = "SuperBowl Ads by Views", #titles of graph, aesthetic and proper details
       subtitle = "A TidyTuesday Project", 
         x = "Brand",
         y = "Number of Ads",
       caption = "Source: YouTube") + 
  theme_classic() +
  ggsave(here("Output", "sb.png"))
  

YT #view graph

