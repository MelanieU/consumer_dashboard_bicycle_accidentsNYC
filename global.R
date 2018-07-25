
library(lubridate)
library(stringr)
library(dplyr)
library('leaflet')
library(ggplot2)
library('shiny')
library(tidyverse)
library(googleVis)
library(maps)
library(ggmap)
library(shinythemes)

bike <- read.csv("./bike.csv", stringsAsFactors = F)
bike$date = as.Date(bike$date, "%Y-%m-%d")

bike %>%
  mutate(POSIX =  as.POSIXct(paste(date, time))) %>%
  mutate(months = month(date)) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  mutate(mon = as.numeric(format(date, "%m"))) %>%
  mutate(season = ifelse(mon %in% c(6,7,8), "summer", 
                         ifelse(mon %in% c(9,10,11), "fall",
                                ifelse(mon %in% c(12,1,2), "winter", "spring")))) %>%
  
  mutate(hour = floor_date(POSIX, period(hour = 1))) %>%
  mutate(HOUR = as.numeric(format(hour, "%H"))) %>%
  mutate(contributing.factor.vehicle.1 = toupper(contributing.factor.vehicle.1),
         vehicle.type.code.1 = toupper(vehicle.type.code.1)) %>%
  mutate(., vehicle.type.code.1 = gsub('LIVERY VEHICLE','TAXI', x = vehicle.type.code.1)) -> bike_bike

themes <- theme(
    plot.background = element_rect(
      fill = '#282B30', colour = '#282B30'),
    panel.background = element_rect(
      fill = "#282B30", colour = '#686868'),
    panel.grid.major = element_line(colour = '#686868', size = 0.07, linetype = "solid"),
    panel.grid.minor = element_line(colour = '#686868', size = 0.05, linetype = "solid"),
    axis.title = element_text(color = '#ffffff', size = 18), 
    axis.text = element_text(color = '#ffffff', size = 15),
    plot.title = element_text(
      color = '#ffffff', size = 18,hjust = 0.5), 
    legend.background = element_rect(
      fill = '#282B30', colour = '#282B30'),
    legend.text = element_text(color = '#ffffff', size = 15),
    legend.title = element_text(color = "#ffffff",size = 15),
    text = element_text(size=20),
    legend.key = element_rect(fill = "#282B30", colour = "#282B30"))

bike_bike %>%
  filter(year != 2018 & year != 2012) %>%
  group_by(date) %>%
  summarise(n = n()) -> bike_line    

bike_bike %>%
  filter(year != 2018 & year != 2012) %>%
  group_by(year, season) %>%
  summarise(n = n())-> bike_total

bike_bike %>%
  group_by(year, season) %>%
  summarize(killed = sum(number.of.cyclist.killed), injured = sum(number.of.cyclist.injured)) %>%
  gather(., outcome, number, killed:injured) -> bike_outcome

bike_bike %>%
  group_by(season, HOUR) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) -> bike_hour

bike_bike %>%
  filter(., !is.na(longitude) & !is.na(latitude)) -> bike_map

bike_map %>%
  group_by(longitude, latitude) %>%
  summarize(n = n()) %>%
  filter(n >= 10) -> hot_spots

bike_bike %>%
  filter(contributing.factor.vehicle.1 != "UNSPECIFIED" ) %>%
  group_by(vehicle.type.code.1, contributing.factor.vehicle.1) %>%
  summarise(count = n(), freq = (count/14782)*100) %>%
  mutate(freq = as.numeric(format(round(freq, 2)))) %>%
  arrange(desc(count)) %>%
  filter(freq >= 1) -> bike_combo

bike_bike %>%
  filter(contributing.factor.vehicle.1 != "UNSPECIFIED") %>%
  group_by(contributing.factor.vehicle.1, HOUR) %>%
  summarize (count = n()) %>%
  arrange(desc(count)) %>%
  filter(contributing.factor.vehicle.1 != "") -> bike_heatmap

bike_map %>%
  filter(zip.code %in% c(10013, 10012, 10014, 10002, 10003, 10009, 10011, 10010, 10001, 10016, 10018, 10017)) %>%
  group_by(longitude, latitude) %>%
  summarize(count_accidents = n()) %>%
  arrange(desc(count_accidents)) %>%
  filter(count_accidents > 10) %>%
  unite(., "coord", c(latitude, longitude), sep = ", ") -> high_freq

bike_map %>%
  filter(zip.code %in% c(10013, 10012, 10014, 10002, 10003, 10009, 10011, 10010, 10001, 10016, 10018, 10017)) %>%
  group_by(longitude, latitude) %>%
  summarize(count_accidents = n()) %>%
  arrange(desc(count_accidents)) %>%
  filter(count_accidents <5) %>%
  unite(., "coord", c(latitude, longitude), sep = ", ") -> low_freq

bike_lanes1 = data.frame(bike_lanes = c(1,1,0,1,0,1,1,0,1,0,1,1,1,0,1,1,1,1), freq = c(rep("high (n > 10)", 18)))
bike_lanes2 = data.frame(bike_lanes = c(1,1,0,0,0,1,0,0,1,1,1,0,0,0,1,0,0,1), freq = c(rep("low (n < 10)", 18)))                       
bike_lanes= rbind(bike_lanes1, bike_lanes2)

bike_lanes %>%
  group_by(freq) %>%
  summarise(perc = ((sum(bike_lanes)/18)*100)) -> bike_lanes

    
