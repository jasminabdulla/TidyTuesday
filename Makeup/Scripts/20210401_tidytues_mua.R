### Tidy Tuesday: Makeup ###
### Date: 2021-04-01 ###
### By: Jasmin Abdulla ###


### Load Libraries ###

library(tidytuesdayR)
library(tidyverse)
library(ggthemes)
library(ggbeeswarm)
library(here)
library(ghibli)


### Load Data ###
sephora <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/sephora.csv')
ulta <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/ulta.csv')
allCategories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv')
allShades <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv')
allNumbers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allNumbers.csv')

view(sephora)
view(ulta)
view(allCategories)
view(allShades)
view(allNumbers)

### Clean + Graph Data ###
mua <- tidytuesdayR::tt_load('2021-03-30') #renaming selected data

ghibli_palettes$MarnieMedium1 #ghibli color palette selection

sephora <- c("Lancôme","Clinique","Estée Lauder","Laura Mercier",
               "Dior","Yves Saint Laurent","Shiseido",
               "La Mer", "TOM FORD") #selecting the high end brands


data <- mua$allCategories %>% #renaming data set 
  filter(brand %in% sephora) #filtering for brand
shades <- data$hex #color would be foundation colors sorted by hex
plot <- ggplot(data=data,aes(x = lightness,y=brand)) + #lightness in each brand
  geom_beeswarm(color=shades,size=2.5)+ #ggplot is beeswarm
  geom_boxplot(alpha = 0.1, width = 0.25,
               outlier.shape = NA) +
  theme_minimal() +
  coord_flip() + #flipping the coordinates
  theme(plot.title = element_text(size = 15, color = "#E9D097FF"), #aesthetics
        axis.text = element_text(size = 8, color = "white"), 
        axis.text.x = element_text(size = 7.5, color = "#E9D097FF", face = "bold"),
        panel.background = element_rect(fill = "#C5A387FF"), 
        plot.background = element_rect(fill = "#5E2D30FF")) +
  labs(title = "Diversity Amongst Luxury High End Brand Names", #naming 
       subtitle = "(the makeup industry is racist)",
       x = "Lightness",
       y = "Brands",
       caption = "Source: ThePudding")
plot

ggsave(here("Makeup", "Output", "dobettermakeupindustry.png"), 
       width = 7, height = 5)
