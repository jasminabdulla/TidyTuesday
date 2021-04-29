### Tidy Tuesday: CEO Departures ###
### Date: 2021-04-27 ###
### By: Jasmin ###


### Load Libraries ###
library(tidyverse)
library(here)
library(tidytuesdayR)
library(paletteer)
library(ggplot2)
library(ggeasy)
library(ggstream)
library(mdthemes)
library(extrafont)

### Load Data ###
tt <- tt_load("2021-04-27")

### Select Data ###
raw_data <- tt$departures
raw_data %>% count(departure_code, sort = TRUE)
dep_solver <- tibble(departure_code = c(1:9,NA), #selecting all the reasons 
                     reason = c("CEO Death", #specifically selecting reasons, wanted voluntary and involuntary only
                                "CEO Illness",
                                "Job performance",
                                "Legal violations",
                                "V: Retired",
                                "V: New opportunity",))

### Clean Data ###
clean_data <- raw_data %>%
  filter(!(departure_code %in% c(7,8,9,NA))) %>% #only want codes 1:6
  select(fyear_gone, departure_code) %>% #selecting year left and departure code
  left_join(dep_solver,
            by = c("departure_code")) %>%  #left joining data
  count(fyear_gone,reason) %>% #count the reason for departure against the year
  arrange(fyear_gone) %>% #organize by year gone
  filter(fyear_gone <= 2020 & fyear_gone >= 2010) %>% #selecting last 10 years only
  mutate(reason = fct_reorder(reason, n, .desc = TRUE))  #mutating the reason for only true values
ranges <- clean_data %>% 
  group_by(fyear_gone) %>% 
  summarise(min = min(n), 
            max = max(n))

### Graph Data ###
clean_data %>% #setting up graph with clean data
  ggplot(aes(fyear_gone, n, fill = reason)) + #plotting the year gone and filling with the reason
  geom_area(type = "proportional", #tryng out new gg plot 
              extra_span = 0.3,
              true_range = "none") +
  paletteer::scale_fill_paletteer_d("nord::aurora") + #color palette
  theme_bw() + 
  labs(color = "Reason", #labeling axises
       x = "Year of Departure",
       y = "Number of CEOs Departing",
       title = "CEO Departures in 1500 S&P firms",
       subtitle = "Voluntary and Involuntary Reasons",
       caption = "Source: Gentry")