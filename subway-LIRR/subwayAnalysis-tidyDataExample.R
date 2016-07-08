ubwaydf <- read_csv('data/DOITT_SUBWAY_STATION_01_13SEPT2010.csv')  
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