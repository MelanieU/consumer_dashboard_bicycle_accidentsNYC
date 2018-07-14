# set wd
setwd("/Users/mela/Desktop/Shiny/shiny_project")
getwd()

# load libraries
library(datasets)
library(lubridate)
library(stringr)
library(dplyr)
library('leaflet')
library(ggplot2)
library('shiny')
library(tidyverse)
# read in file
tweets_original <- read.csv("tweets_fixed.csv")

# removing columns of no interest and filtering for MX and USA
tweets_original %>%
  filter(country  == "US") %>%
  select(-c(num_favs, num_retweets, listed, url)) -> tweets_USA

# create dataset with US state indicators (see datafrme package)
## learn: use of data frame package
state_df <- data.frame(state.name, state.abb, state.region, stringsAsFactors = F)
str(state_df)

# parcing place column into state names and state regions
## learn: shifting columns with string detection
tweets_USA %>%
  mutate(locationType = ifelse(str_detect(place, ", [A-Z][A-Z]$"), "city",
                               ifelse(str_detect(place, ", USA$"), "state", "other")),
         state = ifelse(locationType == 'city', str_sub(place, -2, -1),
                        ifelse(locationType == 'state', str_sub(place, -3, -1), NA))) %>%
  left_join(state_df, by  =c('state' = 'state.abb')) %>%
  mutate(state = ifelse(locationType == 'city', state.name,
                        ifelse(locationType == 'state', state, NA))) -> tweets_USA

tweets_USA$state = NULL



## parsing with function instead of ifelse statement
getLocationType <- function(place) {
  if(str_detect(place, ", [A-Z][A-Z]$")) return("city")
  else if(str_detect(place, ", USA$")) return("state")
  else return ("other") }

tweets %>%
  filter(country == "US") %>%     
  mutate(locationtype = getLocationType(place),
         ifelse(locationType == 'state', str_sub(place, -3, -1), NA)) %>%
  
  library(datasets)
state_df <- data.frame(state.name, state.abb, state.region, stringsAsFactors = F)

## exploring exceptions
tweets %>%
  filter(country == "US") %>%       # filter for US country
  mutate(locationType = ifelse(str_detect(place, ", [A-Z][A-Z]$"), "city",
                               ifelse(str_detect(place, ", USA$"), "state", "other")),
         state = ifelse(locationType == 'city', str_sub(place, -2, -1),
                        ifelse(locationType == 'state', str_sub(place, -3, -1), NA))) %>%
  left_join(state_df, by  =c('state' = 'state.abb')) %>%
  mutate(state = ifelse(locationType == 'city', state.name,
                        ifelse(locationType == 'state', state, NA))) %>%
  filter(is.na(state)) -> exeptions

exceptions %>%
  filter(!str_detect(place, ", DC$"))

# Hashtag counts over time
## learn: posix time
tweets_USA %>%
  mutate(POSIX = as.POSIXct(paste(date, time))) %>%
  mutate(intervals = floor_date(POSIX, period(minute = 15))) %>%
  group_by(intervals)%>%
  summarize(hashtags_count=sum(str_count(tweet,"#."))) %>%
  ggplot(aes(x = intervals, y=hashtags_count)) +
  geom_line() +
  scale_x_datetime()

# Number of tweets per hour and per language
tweets_USA %>%
  mutate(POSIX = as.POSIXct(paste(date, time))) %>%
  mutate(hour = floor_date(POSIX, period(hour = 1))) %>%
  group_by(hour, country, language)%>%
  summarize(total=n()) %>%
  ungroup() %>%
  ggplot(aes(x=hour, y=total)) +
  geom_bar(stat = 'identity') +
  facet_grid(country ~ language) +
  scale_x_datetime(breaks = NULL)


## Count tweets per hour
tweets_USA %>%
  mutate(POSIX = as.POSIXct(paste(date, time))) %>%
  mutate(hour = floor_date(POSIX, period(hour = 1))) %>%
  group_by(date, hour(hour)) %>%
  summarise(posts = n()) %>%
  arrange(desc(posts)) %>%
  ggplot(aes(x=hour, y=posts) + geom_col())

# Extract hashtags
tw = tweets_USA$`tweet`
tweets_list = str_split(tw, " ")

getHashtags <- function(x) {
  return(x[str_detect(x, '#')])
}

hashtags_list = lapply(tweets_list, getHashtags)
hashtags_list

hashtags_list %>%
  str_c(collapse = " ") %>%
  str_replace_all('character\\(0\\)', '') %>%
  str_replace_all("\"", "") %>%
  str_replace_all("c[\\(+]", "") %>%
  str_replace_all("[\\(\\)]", "") %>%
  str_replace_all("[,\\$\\?]", "") %>%
  str_replace_all("\\\\x..", "") %>%
  str_split(' ') -> hashtags_list

hashtags_list[[1]]  
summary(as.factor(hashtags_list[[1]]))

matrix(hashtags_list[[1]], dimnames=list(NULL, "Hashtag")) -> df
as.data.frame(df) -> df

df %>%
  group_by(Hashtag) %>%
  summarise( n = n()) %>%
  arrange(desc(n)) %>%
  filter(n > 100) -> hashtag_df


ggplot(tweets_USA, aes(x=followers, y=following)) + geom_point()

# sum of followers per region
tweets_USA %>%
  drop_na(state.region) %>%
  group_by(state.region, name) %>%
  summarise(sum = sum(followers, na.rm = T)) %>%
  group_by(state.region) %>%
  summarize(SUM = sum(sum)) %>%
  ggplot(aes(x = state.region, y=SUM)) + geom_col()

# most tweets per region
tweets_USA %>%
  drop_na(state.region) %>%
  group_by(state.region) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = state.region, y=n)) + geom_col()
