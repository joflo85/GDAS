


# Installeer en laad te R packages. Creeer een working directory.

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("ggthemes")
tinytex::install_tinytex()

library(tidyverse)  
library(lubridate)
library(ggplot2) 
library(ggthemes)
library(tinytex)

# LET OP! Onderstaande directory is mijn working directory! Voor het reproduceren dient deze te worden aangepast naar een eigen working directory op jouw eigen PC of laptop!
setwd("/Users/joramhofm/Desktop/GDAPC/Archive/Cyclistic Bike Share Data Analysis")
getwd()

# Verzamelen van de data
# upload de CSV bestanden en converteer ieder bestand naar een R data.frame
bike_share_analysis <- read_csv("/Users/joramhofm/Desktop/GDAPC/Archive/Cyclistic Bike Share Data Analysis/Cyclistic Data.csv")

# Voer eventueel basis statistiek uit betreft de ritduur van "members" en "casual" fietsers
aggregate(bike_share_analysis$ride_length ~ bike_share_analysis$type_fietser, FUN = mean)
aggregate(bike_share_analysis$ride_length ~ bike_share_analysis$type_fietser, FUN = median)
aggregate(bike_share_analysis$ride_length ~ bike_share_analysis$type_fietser, FUN = max)
aggregate(bike_share_analysis$ride_length ~ bike_share_analysis$type_fietser, FUN = min)

# Analyse van de historische fiets-data

## Gemiddelde ritduur van Casual fietsers en Members 
bike_share_analysis %>%
  group_by(type_fietser) %>%
  summarise(average_duration = mean(ride_length)) %>%
  ggplot(aes(x = type_fietser, y = average_duration, fill = type_fietser)) +
  geom_col(position = "dodge") + 
  scale_fill_manual(values = c("casual" = "#32cd32", member = "#1b98e0")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,4000), labels = function(x) format(x, scientific = FALSE)) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(title = "Gemiddelde ritduur per type fietser (in seconden)", 
       x = "Type fietser",
       y = "Gemiddelde ritduur") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 13.5, hjust = -0.3), axis.title.y = element_text(margin = margin(0, 15, 0, 0)), 
        axis.title.x = element_text(margin = margin(10, 15, 0)), axis.ticks = element_line(), axis.line = element_line(), 
        panel.grid.major = element_blank(),legend.position = "right") +
  guides(fill = guide_legend(title.theme = element_text( size = 10.5), title.position = "top"))


## Aantal ritten gereden door Casual fietsers en Members
bike_share_analysis %>%
  group_by(type_fietser) %>%
  summarise(number_of_trips = n()) %>% 
  ggplot(aes(x = type_fietser, y = number_of_trips, fill = type_fietser)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("casual" = "#32cd32", member = "#1b98e0")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,4000000), labels = function(x) format(x, scientific = FALSE)) + 
  scale_x_discrete(expand = c(0, 0)) +
  labs(title = "Aantal ritten per type fietser",
       x = "Type fietser",
       y = "Aantal ritten") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 13.5, hjust = 0.6), axis.title.y = element_text(margin = margin(0, 15, 0, 0)), 
        axis.title.x = element_text(margin = margin(10, 15, 0)), axis.ticks = element_line(), axis.line = element_line(),    
        panel.grid.major= element_blank(),
        legend.position = "right") +
  guides(fill = guide_legend(title.theme = element_text( size = 10.5), title.position = "top"))


## Aantal ritten per dag van de week
bike_share_analysis$day_of_week <- factor(bike_share_analysis$day_of_week, levels = c("maandag", "dinsdag", "woensdag", "donderdag", "vrijdag", "zaterdag",     "zondag"))
bike_share_analysis %>%
  group_by(type_fietser, day_of_week) %>%
  summarise(number_of_trips = n(), average_duration = mean(ride_length)) %>%
  arrange(type_fietser,day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_trips, fill = type_fietser)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("casual" = "#32cd32", member = "#1b98e0")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600000), labels = function(x) format(x, scientific = FALSE)) + 
  labs(title = "Aantal ritten per dag van de week", 
       x = "Dag van de week",
       y = "Aantal ritten") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 13.5, hjust = 0.5), axis.title.y = element_text(margin = margin(0, 15, 0, 0)), 
        axis.title.x = element_text(margin = margin(10, 15, 0, 0)), axis.ticks = element_line(), axis.line = element_line(), 
        panel.grid.major = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(title.theme = element_text( size = 10.5), title.position = "top"))


## De gemiddelde ritduur per dag van de week 
bike_share_analysis %>%
  group_by(type_fietser, day_of_week) %>%
  summarise(number_of_trips = n(), average_duration = mean(ride_length)) %>%
  arrange(type_fietser, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = type_fietser)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("casual" = "#32cd32", member = "#1b98e0")) +
  labs(title = "Gemiddelde ritduur per dag van de week", 
       x = "Dag van de week",
       y = "Gemiddelde ritduur") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,4000), labels = function(x) format(x, scientific = FALSE)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 13.5, hjust = 0.5), axis.title.y = element_text(margin = margin(0, 15, 0, 0)), 
        axis.title.x = element_text(margin = margin(10, 15, 0)), axis.ticks = element_line(), axis.line = element_line(), 
        panel.grid.major = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(title.theme = element_text( size = 10.5), title.position = "top"))


## Het aantal ritten per uur van de dag  
bike_share_analysis %>%
  group_by(type_fietser, hour_of_day) %>%
  summarise(number_of_trips = n(), average_duration = mean(ride_length)) %>%
  arrange(type_fietser, hour_of_day) %>%
  ggplot(aes(x = hour_of_day, y = number_of_trips, fill = type_fietser)) +
  geom_col(width = 0.4, position = "dodge") +
  scale_fill_manual(values = c("casual" = "#32cd32", member = "#1b98e0")) +
  labs(title = "Aantal ritten per uur van de dag", 
       x = "Uur van de dag",
       y = "Aantal ritten") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 430000), labels = function(x) format(x, scientific = FALSE)) +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 13.5, hjust = 0.5), axis.title.y = element_text(margin = margin(0, 15, 0, 0)), 
        axis.title.x = element_text(margin = margin(10, 15, 0)), axis.ticks = element_line(), axis.line = element_line(), 
        panel.grid.major = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(title.theme = element_text( size = 10.5), title.position = "top"))


## Aantal ritten per type fietser per maand  
bike_share_analysis %>%
  group_by(type_fietser, month) %>%
  summarise(number_of_trips = n(), average_duration = mean(ride_length)) %>%
  arrange(type_fietser, month) %>%
  ggplot(aes(x = month, y = number_of_trips, fill = type_fietser)) +
  geom_col(width = 0.5, position = "dodge") +
  scale_fill_manual(values = c("casual" = "#32cd32", member = "#1b98e0")) +
  labs(title = "Aantal ritten per maand", 
       caption = "Q2 t/m Q4 2019 en Q1 2020",       
       x = "Maand",
       y = "Aantal ritten") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,450000), labels = function(x) format(x, scientific = FALSE)) +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 13.5, hjust = 0.5), axis.title.y = element_text(margin = margin(0, 15, 0, 0)), 
        axis.title.x = element_text(margin = margin(10, 15, 0)), axis.ticks = element_line(), axis.line = element_line(), 
        panel.grid.major = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(title.theme = element_text( size = 10.5), title.position = "top"))