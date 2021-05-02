### Tidy Tuesday: US Post Offices ###
### By: Jasmin ###

### Load Libraries ###
library(tidyverse)
library(tidytuesdayR)
library(here)
library(ggeasy)
library(ggplot2)
library(tvthemes)
library(magick)

### Load Data ###
unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

view(unvotes)
view(roll_calls)
view(issues)

### Select Data ###

unvotes%>% #using the UN Votes data sheet
  filter(country_code %in% c("US")) %>% #filtering for USA votes only
  select(vote, rcid) #selecting data to graph
  

### Graph Data ###

USA <- unvotes %>% 
ggplot() +
  aes(x=rcid, fill=vote, color = vote) + #choosing what to place on plot
  geom_density(alpha=0.5) + #geom density plot
  theme_bw()+ #bw theme
  theme(plot.title = element_text(color = "#99fadc", size = 20, face = "bold"), #aesthetics for text
        axis.text.x = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#3c89d0", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#3c89d0", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
  labs(y = "Frequency of Votes", #labels
       x = "Roll Call ID",
       title = "US Votes in the UN",
       subtitle = "A Look at How the USA Votes",
       caption = "Source: Harvard's Dataverse"
       ) +
  coord_flip()+ #flip x and y axes
  ylim(0,0.0003)  #setting the limits for the y axis

USA

UN <- image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/UN_emblem_blue.svg/1205px-UN_emblem_blue.svg.png") #uploading UN logo
USA <- image_read(here("UNVotes","Output","usaplot.png")) #saving plot 

theUSA <- image_composite(USA, image_scale(UN,"x200"), #compositing the image onto the plot
                                    offset = "+75,+100", #setting the location of image
                                    gravity = "northeast") %>%  
  image_write(here("UNVotes","Output","unusaplot.png")) #saving image of plot



