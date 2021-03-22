### Tidy Tuesday: Gaming ###
### Date: 2021-03-22 ###
### By: Jasmin Abdulla ###


### Load Libraries ###
library(tidyverse)
library(here)
library(tidytuesdayR)
library(ghibli)


### Load Data ###
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')


### Create Graphs ###
games$month <- as.factor(games$month) #month to factor for labeling
games$month <- recode_factor(games$month, #using numbers of month instead of name
                             January = "1",
                             February = "2",
                             March = "3",
                             April = "4",
                             May = "5",
                             June = "6",
                             July = "7",
                             August = "8",
                             September = "9",
                             October = "10",
                             November = "11",
                             December = "12")

games$month <- as.numeric(games$month) #confirming months as numbers

games %>%
  select("month", "year", "gamename", "peak") %>% #selecting categories for graphing
  filter(complete.cases(.), #filtering for complete cases
         year > "2017" & year < "2021", #data range from 2017 to 2021
         gamename == "Counter-Strike: Global Offensive" | gamename == "Dota 2" | gamename == "PLAYERUNKNOWN'S BATTLEGROUNDS") %>%
  ggplot(aes(x = month, #x, y, and color of lines that represent data to be plotted
             y = peak,
             color = gamename)) +
  geom_line(size = 2) + #thickness of linne
  theme_bw()+ #theme for ggplot
  theme(panel.background = element_rect(fill = "white")) + #white background
  scale_color_manual(values = ghibli_palette("KikiMedium")) + #kikis delivery service color scheme
  facet_wrap(~year) + #graphs separated by year
  scale_x_continuous(breaks = seq(1, 12, 1),  #separating x-axis via months
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) + #rename months
  scale_y_continuous(expand = c(0, 0)) + #aesthetics from this point on 
  theme(axis.title=element_text(size = 20,
                                color = "navy"),
        panel.background = element_rect(fill="white")) +
  labs(x = "Month",
       y = "Peak",
       title = "Peak of Players in Top Three Games",
       caption = "Source: SteamCharts") + 
  ggsave(here("Week_8", "Output", "goodgame.png"),
         width = 7, height = 5)