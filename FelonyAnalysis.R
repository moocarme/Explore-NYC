# Load in libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(caret)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(spatstat)

# Read in and filter data =======================================
felony.data <- read_csv('data/NYPD_7_Major_Felony_Incidents.csv')
str(felony.data) # check out structure
colnames(felony.data)
length(unique(felony.data$Identifier)) # almost all unique
# filter data for robberies on the weekdays 
tidy.fdata <- felony.data %>% 
  dplyr::filter(Offense == 'ROBBERY') %>% 
  dplyr::select(Hour = `Occurrence Hour`,Day.Week = `Day of Week`) %>% 
  dplyr::filter(!(Day.Week %in% c('Saturday', 'Sunday'))) %>%
  dplyr::group_by(Hour) 
%>% dplyr::summarise(Hourly.Crime.Rate= n())

# Recreate plot from IQuantNY (roughly)
ggplot(data = tidy.fdata) + geom_line(aes(x = Hour, y = Hourly.Crime.Rate)
                                      , color= 'darkgreen')

# Now look at just 3pm data
new.fdata <- felony.data %>% filter(Offense == 'ROBBERY') %>% 
  dplyr::select(Hour = `Occurrence Hour`,Day.Week = `Day of Week`, `Location 1`) %>% 
  dplyr::filter(!(Day.Week %in% c('Saturday', 'Sunday')), Hour =='15')


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
new.fdata4 <- new.fdata3 %>% 
  group_by(Cluster.Lat, Cluster.Long, Cluster) %>% 
  summarise(Total.Counts= n())

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

# Check close to 

# load shapefile and read in
shp <-'../Data-Incubator-Challenge/Vision-Zero/nybb_16b/nybb.shp'
ny <- readOGR(shp, ogrListLayers(shp)[1], stringsAsFactors=FALSE)
map <- spTransform(ny, CRS("+proj=longlat +datum=WGS84"))

num.Rands = 10000 # number of random numbers
# Outer longitude, latitude limits of NYC
long.Min = -74.2; long.Max = -73.65
lat.Min = 40.4; lat.Max = 40.95
# generate random numbers
random.Longs = runif(num.Rands, long.Min, long.Max)
random.Lats = runif(num.Rands, lat.Min, lat.Max)

# convert to dataframe 
dat <- data.frame(Longitude = random.Longs,
                  Latitude  = random.Lats)
LongLats <- dat
# convert to coordinates
coordinates(dat) <- ~ Longitude + Latitude
# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(dat) <- proj4string(map)

# select points only over manhattan boundry


rand.points <- cbind(over(dat, map), LongLats) %>% 
  dplyr::filter(!is.na(BoroName)) %>%
  dplyr::select(Latitude, Longitude)

# Use approx from http://www.movable-type.co.uk/scripts/latlong.html
# average kms in a lat or long degree
ave.km.per.Lat.NY <- 111.2
ave.km.per.Long.NY <- 84.24

tidy.schooldf<- filter(tidy.schooldf, !is.na(Latitude) & !is.na(Longitude))
# Convert data frames to lists for calculating geographical distances
felony_df_lst <- structure(list(lon = new.fdata2$Long*ave.km.per.Long.NY, 
                                 lat = new.fdata2$Lat*ave.km.per.Lat.NY),
                            .Names = c("lon", "lat"), 
                            row.names = c(NA, as.integer(nrow(new.fdata2))), 
                            class = "data.frame")

school_df_lst <- structure(list(name=tidy.schooldf$`Location Name`,
                                lon = tidy.schooldf$Longitude*ave.km.per.Long.NY, 
                                lat = tidy.schooldf$Latitude*ave.km.per.Lat.NY),
                           .Names = c("lon", "lat"), 
                           row.names = c(NA, as.integer(nrow(tidy.schooldf))), 
                           class = "data.frame")

rand.points_lst <- structure(list(lon = rand.points$Longitude*ave.km.per.Long.NY, 
                                  lat = rand.points$Latitude*ave.km.per.Lat.NY),
                             .Names = c("lon", "lat"), row.names = c(NA, nrow(rand.points)), 
                             class = "data.frame")

felony_df_sp <- SpatialPoints(felony_df_lst)
school_df_sp <- SpatialPoints(school_df_lst[,2:3])
rand.points_sp <- SpatialPoints(rand.points_lst)

rand.points_lst$nearest.school <- apply(gDistance(rand.points_sp, 
                                                        school_df_sp, byid=TRUE), 2, min)
felony_df_lst$nearest.school <- apply(gDistance(felony_df_sp, school_df_sp, byid=TRUE), 2, min)

felony_df_lst$which.school <- school_df_lst[apply(gDistance(felony_df_sp, school_df_sp, byid=TRUE), 2, which.min),1]

top.schools.robbery <- felony_df_lst %>% 
  dplyr::group_by(which.school) %>%
  dplyr::summarise(robbery.count = n()) %>%
  dplyr::arrange(desc(robbery.count))

head(top.schools.robbery, n=10)

rand_min_Dists_df <- data.frame(dists = rand.points_lst$nearest.school*1000)
felony_min_Dists_df <- data.frame(dists = felony_df_lst$nearest.school*1000)

mean.Randpts2school <- mean(log(rand_min_Dists_df$dists))
mean.felony2school <- mean(log(felony_min_Dists_df$dists))

ggplot() + 
  geom_histogram(data = log(rand_min_Dists_df), 
                 aes(x = dists, y=..count../sum(..count..), fill= 'green', color = 'green'), 
                 fill = "green", alpha = 0.2, bins = 70) +
  geom_histogram(data = log(felony_min_Dists_df), 
                 aes(x = dists, y=..count../sum(..count..), fill= 'red', color = 'red'), 
                 fill = "red",  alpha = 0.2, bins = 70) + 
  geom_vline(xintercept = (mean.Randpts2school), color = 'green') +
  geom_vline(xintercept = (mean.felony2school), color = 'red') +
  labs(list(Title = 'Log Distance to Closest Tourist Attraction'
                                , y = 'Normalized Count', x = 'Log distance(m)')) +
  # ylim(c(0, 0.042)) +
  scale_colour_manual(name="group", values=c("red" = "red", "green"="green"), 
                      labels=c("green"="Random Uniform Distribution", "red"="Felony Distribution")) +
  scale_fill_manual(name="group", values=c("red" = "red", "green"="green"), 
                    labels=c("green"="Random Uniform Distribution", "red"="Felony Distribution"))

t.test(rand_min_Dists_df, felony_min_Dists_df)
# # Now investigate subway/LIRR stations 
# # INCONCLUSIVE IGNORE FOR ANALYSIS
# # Just keeping in here as it may be useful for other analysis
# subwaydf <- read_csv('data/DOITT_SUBWAY_STATION_01_13SEPT2010.csv')  
# new.subway <- subwaydf %>%
#   separate(the_geom, c('point','Long_tmp', 'Lat_tmp'),sep = ' ') %>%
#   mutate(Lat = as.numeric(gsub("\\(|\\)","", Lat_tmp))) %>%
#   mutate(Long = as.numeric(gsub("\\(|\\)","", Long_tmp))) %>% 
#   select(subway= NAME, Lat, Long)
# head(new.subway)
# 
# lirrdf <- read_csv('data/LIRR_stops.csv')
# head(lirrdf)
# new.lirr <- lirrdf %>%
#   separate(the_geom, c('point','Long_tmp', 'Lat_tmp'),sep = ' ') %>%
#   mutate(Lat = as.numeric(gsub("\\(|\\)","", Lat_tmp))) %>%
#   mutate(Long = as.numeric(gsub("\\(|\\)","", Long_tmp))) %>% 
#   select(station= STOPNAME, Lat, Long) %>%
#   filter(Long < -73.6)
# head(new.lirr)
# 
# NYmap <- qmap(location = 'New York City', zoom= 10,
#                source = 'google', color = 'color')
# 
# NYmap + geom_point(data = new.fdata4, aes(x=Cluster.Long, y=Cluster.Lat, color = new.fdata4$Total.Counts)
#                                , size = new.fdata4$Total.Counts/100) +
#   scale_color_gradient(low = 'red', high = 'yellow') + scale_size(range = c(0,1)) + 
#   geom_jitter(data = tidy.schooldf, aes(x= Longitude, y= Latitude), shape = 4) +
#   geom_jitter(data = new.subway, aes(x = Long, y = Lat), shape = 3, color = 'blue', size = 0.25) +
#   geom_jitter(data = new.lirr, aes(x = Long, y = Lat), shape = 3, color = 'red', size = 0.5)
# 
# write.csv(new.subway, file = 'NYCsubwayLocation.csv')
# write.csv(new.lirr, file = 'LIRRStationLocation.csv')