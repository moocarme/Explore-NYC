# Load in libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(caret)

# Read in and filter data =======================================
felony.data <- read_csv('data/NYPD_7_Major_Felony_Incidents.csv')
str(felony.data) # check out structure
colnames(felony.data)
length(unique(felony.data$Identifier)) # almost all unique
# filter data for robberies on the weekdays 
tidy.fdata <- felony.data %>% filter(Offense == 'ROBBERY') %>% 
  select(Hour = `Occurrence Hour`,Day.Week = `Day of Week`) %>% 
  filter(!(Day.Week %in% c('Saturday', 'Sunday'))) %>%
  group_by(Hour) %>% summarise(Hourly.Crime.Rate= n())

# Recreate plot from IQuantNY (roughly)
ggplot(data = tidy.fdata) + geom_line(aes(x = Hour, y = Hourly.Crime.Rate)
                                      , color= 'darkgreen')

# Now look at just 3pm data
new.fdata <- felony.data %>% filter(Offense == 'ROBBERY') %>% 
  select(Hour = `Occurrence Hour`,Day.Week = `Day of Week`, `Location 1`) %>% 
  filter(!(Day.Week %in% c('Saturday', 'Sunday')), Hour =='15')


# Read in school data ================================================
schooldf <- read_csv('data/Location_Information_Report.csv')
str(schooldf) # check out structure
unique(schooldf$`Location Category Description`) # look at different school types

# list of relevant school types
rel.schools <- c('High School', 'Secondary School', 'K-12 all grades')

# filter for those schools and choose appropriate colunms
tidy.schooldf <- schooldf %>% select(`Location Name`, `Location Category Description`, Latitude, Longitude) %>%
  filter(`Location Category Description` %in% rel.schools)
# 119 schools


# get long and lat coords from Location variable
new.fdata2 <- new.fdata %>%
  separate(`Location 1`, c('Lat_tmp', 'Long_tmp'),sep = ',') %>%
  mutate(Lat = as.numeric(gsub("\\(|\\)","", Lat_tmp))) %>%
  mutate(Long = as.numeric(gsub("\\(|\\)","", Long_tmp)))

# cluster long and lat coords with 150 centers
set.seed(666)
latlong_cluster <- kmeans(cbind(new.fdata2$Lat, new.fdata2$Long), centers  = 150)
# turn cluster nubers into columns with cluster centers
new.fdata3 <- cbind(new.fdata2, Cluster = latlong_cluster$cluster, 
                    Cluster.Lat = latlong_cluster$centers[latlong_cluster$cluster,1],
                    Cluster.Long = latlong_cluster$centers[latlong_cluster$cluster,2])

# count number of robberies within each cluster
new.fdata4 <- new.fdata3 %>% group_by(Cluster.Lat, Cluster.Long, Cluster) %>% summarise(Total.Counts= n())

# = Plot on Map
map <- get_map(location = 'New York City', zoom= 10,
               source = 'google', color = 'color')

map <- ggmap(map) + geom_point(data = new.fdata4, aes(x=Cluster.Long, y=Cluster.Lat, color = new.fdata4$Total.Counts)
                               , size = new.fdata4$Total.Counts/100) +
  scale_color_gradient(low = 'red', high = 'yellow')
map +scale_size(range = c(0,1))
  map + geom_jitter(data = tidy.schooldf, aes(x= Longitude, y= Latitude), shape = 4)

write.csv(new.fdata4, file = 'felonyData_clustered.csv')
write.csv(tidy.schooldf, file = 'schooldf.csv')

  
# Now investigate subway/LIRR stations 
# INCONCLUSIVE IGNORE FOR ANALYSIS
# Just keeping in here as it may be useful for other analysis
subwaydf <- read_csv('data/DOITT_SUBWAY_STATION_01_13SEPT2010.csv')  
new.subway <- subwaydf %>%
  separate(the_geom, c('point','Long_tmp', 'Lat_tmp'),sep = ' ') %>%
  mutate(Lat = as.numeric(gsub("\\(|\\)","", Lat_tmp))) %>%
  mutate(Long = as.numeric(gsub("\\(|\\)","", Long_tmp))) %>% 
  select(subway= NAME, Lat, Long)
head(new.subway)

lirrdf <- read_csv('data/LIRR_stops.csv')
head(lirrdf)
new.lirr <- lirrdf %>%
  separate(the_geom, c('point','Long_tmp', 'Lat_tmp'),sep = ' ') %>%
  mutate(Lat = as.numeric(gsub("\\(|\\)","", Lat_tmp))) %>%
  mutate(Long = as.numeric(gsub("\\(|\\)","", Long_tmp))) %>% 
  select(station= STOPNAME, Lat, Long) %>%
  filter(Long < -73.6)
head(new.lirr)

NYmap <- qmap(location = 'New York City', zoom= 10,
               source = 'google', color = 'color')

NYmap + geom_point(data = new.fdata4, aes(x=Cluster.Long, y=Cluster.Lat, color = new.fdata4$Total.Counts)
                               , size = new.fdata4$Total.Counts/100) +
  scale_color_gradient(low = 'red', high = 'yellow') + scale_size(range = c(0,1)) + 
  geom_jitter(data = tidy.schooldf, aes(x= Longitude, y= Latitude), shape = 4) +
  geom_jitter(data = new.subway, aes(x = Long, y = Lat), shape = 3, color = 'blue', size = 0.25) +
  geom_jitter(data = new.lirr, aes(x = Long, y = Lat), shape = 3, color = 'red', size = 0.5)

write.csv(new.subway, file = 'NYCsubwayLocation.csv')
write.csv(new.lirr, file = 'LIRRStationLocation.csv')