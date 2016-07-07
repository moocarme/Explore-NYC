library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)

# To do add in holidays- use mutate to make a new factor variable 
# with levels - weekend, holiday, weekday

felony.data <- read_csv('data/NYPD_7_Major_Felony_Incidents.csv')
str(felony.data)
colnames(felony.data)
length(unique(felony.data$Identifier))
tidy.fdata <- felony.data %>% filter(Offense == 'ROBBERY') %>% 
  select(Hour = `Occurrence Hour`,Day.Week = `Day of Week`) %>% 
  filter(!(Day.Week %in% c('Saturday', 'Sunday'))) %>%
  group_by(Hour) %>% summarise(Hourly.Crime.Rate= n())

ggplot(data = tidy.fdata) + geom_line(aes(x = Hour, y = Hourly.Crime.Rate)
                                      , color= 'darkgreen')

new.fdata <- felony.data %>% filter(Offense == 'ROBBERY') %>% 
  select(Hour = `Occurrence Hour`,Day.Week = `Day of Week`, `Location 1`) %>% 
  filter(!(Day.Week %in% c('Saturday', 'Sunday')), Hour =='15')

new.fdata2 <- new.fdata %>%
  separate(`Location 1`, c('Lat_tmp', 'Long_tmp'),sep = ',') %>%
  mutate(Lat = round(as.numeric(gsub("\\(|\\)","",Lat_tmp)), 3)) %>%
  mutate(Long = round(as.numeric(gsub("\\(|\\)","",Long_tmp)), 3)) %>%
    group_by(Long, Lat) %>% summarise(Count = n())

ggmap()
