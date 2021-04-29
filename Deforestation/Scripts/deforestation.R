### Tidy Tuesday: Global Deforestation ###
### By: Jasmin ###


### Load Libraries ###
library(tidyverse)
library(tidytuesdayR)
library(here)
library(tvthemes)


### Load Data ###
brazil_loss <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')


### Select Data ###
brazil <- brazil_loss %>% 
  pivot_longer(cols = flooding_due_to_dams:natural_disturbances, #pivot data to show loss for reasons selected
               names_to = "reason",
               values_to = "loss") %>% 
  select(reason, loss, year) %>% #columns selected
  group_by(year) %>% #group by year
  filter(year <= 2005 & year >= 2000) %>% #filtering for 2000-2005
  mutate(reason = recode(reason, #renaming for aesthetics
                        'natural_disturbances' = "Natural Disturbances",
                        'flooding_due_to_dams' = "Flooding by Dams"))


### Graph Data ###
BZL <- brazil %>% 
  ggplot(aes(x=year, y=loss,fill=reason))+ #create plot
  geom_stream()+ #stream geom
  labs(color = "Reason", #labels for title, subtitle, axis
       x = "Year",
       y = "Loss",
       title = "Global Deforestation",
       subtitle = "Microscope on Brazil (2001-2005)",
       caption = "Source: Our World in Data")+
  guides(fill = guide_legend(title="Reason for Loss"))+ #legend title
  theme_avatar() #avatar of course

BZL