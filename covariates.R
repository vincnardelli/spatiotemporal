# Useful covariates from EUROSTAT

library(sf)
library(ggplot2)
library(dplyr)
library(eurostat)
library(rvest)
library(tidyr)
library(lubridate)

nuts3 <- read_sf("data/NUTS_RG_60M_2016_4326_LEVL_3.geojson")
countries <- c("PT", "ES", "FR", "CH", "DE", "AT", "BE", "NL", "DK", "IT")

nuts3 <- nuts3 %>% 
  filter(CNTR_CODE %in% countries) %>% 
  select(id, NUTS_NAME, geometry)


ggplot(nuts3) +
  geom_sf() +
  ylim(37, 60) +
  xlim(-10, 20) +
  coord_sf()


# densità della popolazione

pop_density <- get_eurostat('demo_r_d3dens', time_format = "num", filters = list(geoLevel="nuts3", time="2018")) %>% 
  select('id'=geo, 'pop_density'=values)

nuts3 <- nuts3 %>% 
  left_join(pop_density)


gdp <- get_eurostat('nama_10r_3gdp', time_format = "num", filters = list(geoLevel="nuts3", time="2017", unit = "EUR_HAB")) %>%
  select('id'=geo, 'gdp'=values)

nuts3 <- nuts3 %>%
  left_join(gdp)


popgdp <- get_eurostat('nama_10r_3popgdp', time_format = "num", filters = list(geoLevel="nuts3", time="2017")) %>% 
  select('id'=geo, 'popgdp'=values) # in thous

nuts3 <- nuts3 %>% 
  left_join(popgdp)



business_size <- get_eurostat('bd_size_r3', filters = list(geoLevel="nuts3", time="2017", indic_sb = "V11910", sizeclas = "TOTAL")) %>% 
  select('id'=geo, 'business_size'=values) # in thous

nuts3 <- nuts3 %>% 
  left_join(business_size)


freight_loading <- get_eurostat('road_go_na_rl3g', time_format = "num", filters = list(geoLevel="nuts3", time="2017")) %>% 
  select('id'=geo, 'freight_loading'=values) # in thous

nuts3 <- nuts3 %>% 
  left_join(freight_loading)


freight_unloading <- get_eurostat('road_go_na_ru3g', time_format = "num", filters = list(geoLevel="nuts3", time="2017")) %>% 
  select('id'=geo, 'freight_unloading'=values) # in thous

nuts3 <- nuts3 %>% 
  left_join(freight_unloading)





ggplot(nuts3) +
  geom_sf(aes(fill=pop_density, color=pop_density)) +
  ylim(37, 60) +
  xlim(-10, 20) +
  coord_sf() +
  ggtitle("Pop density")


ggplot(nuts3) +
  geom_sf(aes(fill=gdp, color=gdp)) +
  ylim(37, 60) +
  xlim(-10, 20) +
  coord_sf() +
  ggtitle("GDP")


ggplot(nuts3) +
  geom_sf(aes(fill=business_size, color=business_size)) +
  ylim(37, 60) +
  xlim(-10, 20) +
  coord_sf() +
  ggtitle("Business size")


ggplot(nuts3) +
  geom_sf(aes(fill=freight_loading, color=freight_loading)) +
  ylim(37, 60) +
  xlim(-10, 20) +
  coord_sf() +
  ggtitle("Freight loading")


ggplot(nuts3) +
  geom_sf(aes(fill=freight_unloading, color=freight_unloading)) +
  ylim(37, 60) +
  xlim(-10, 20) +
  coord_sf() +
  ggtitle("Freight unloading")



# covariate spazio-temporali mensili

# heating degree days
montly_hdd <- get_eurostat('nrg_chddr2_m', filters = list(geoLevel="nuts3", 
                                                          time=c("2018M01", "2018M02", "2018M03", 
                                                                 "2018M04", "2018M05", "2018M06", 
                                                                 "2018M07", "2018M08", "2018M09", 
                                                                 "2018M10", "2018M11", "2018M12"), 
                                                          indic_nrg="HDD")) %>%
  select('id'=geo, month=time, 'hdd'=values)

# cooling degree days
montly_cdd <- get_eurostat('nrg_chddr2_m', filters = list(geoLevel="nuts3", 
                                                          time=c("2018M01", "2018M02", "2018M03", 
                                                                 "2018M04", "2018M05", "2018M06", 
                                                                 "2018M07", "2018M08", "2018M09", 
                                                                 "2018M10", "2018M11", "2018M12"), 
                                                          indic_nrg="CDD")) %>%
  select('id'=geo, month=time, 'cdd'=values)





load('data_pm10.RData')
nuts3_data <- st_drop_geometry(nuts3)
nuts3_spatial <- select(nuts3, id, geometry)


stations <- META %>% 
  select(station=AirQualityStation, lon=Longitude, lat=Latitude, alt = Altitude, 
         area=AirQualityStationArea) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4979, agr = "constant") %>% 
  st_transform(4326) %>% 
  st_join(nuts3_spatial, join=st_within)

nrow(META)
nrow(stations)

# match con nuts più vicino
unmatched <- stations %>% 
  filter(is.na(id))

dist <- st_distance(unmatched, nuts3_spatial)

min_dist <- apply(dist, 1, which.min)
nutsmatch <- nuts3_spatial[min_dist, ]
nutsmatch$station <- unmatched$station
nutsmatch <- st_drop_geometry(nutsmatch)
stations[stations$station %in% nutsmatch$station, ]$id <- nutsmatch$id

stations <- stations %>% 
  left_join(nuts3_data)

sum(is.na(stations$id)) #nessun NA!


ggplot(nuts3) +
  geom_sf(aes(fill=pop_density, color=pop_density)) +
  geom_sf(data=stations, color="red", size=0.5) +
  ylim(37, 60) +
  xlim(-10, 20) +
  coord_sf() +
  theme_minimal() +
  ggtitle("Pop density and stations")


data <- as.data.frame(PM10)
data$date <- as_date(rownames(PM10))

data <- data %>% 
  pivot_longer(cols = starts_with("STA"), names_to = "station") %>% 
  mutate(month = floor_date(date, "month"))


nrow(data)

# qui succede qualcosa di strano, da controllare!
nrow(left_join(data, stations))
nrow(right_join(data, stations))


data <- left_join(data, stations)

# join per covariate spazio-temporale mensili
data <- left_join(data, montly_hdd)
data <- left_join(data, montly_cdd)




# controllo problema di Fabio!
data %>% 
  group_by(station) %>% 
  summarise(check = length(unique(date)) > 365) %>% 
  View()


data %>% 
  group_by(station, date) %>% 
  summarise(length(alt)) %>% 
  View()

