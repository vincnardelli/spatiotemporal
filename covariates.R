# Useful covariates from EUROSTAT

library(sf)
library(ggplot2)
library(dplyr)
library(eurostat)
library(rvest)
library(tidyr)
library(lubridate)
library(raster)
library(rgdal)

countries <- c("PT", "ES", "FR", "CH", "DE", "AT", "BE", "NL", "DK", "IT")
countries <- c("IT")


#load data and remove duplicate stations
load('data_pm10.RData')
duplicated <- colnames(PM10)[duplicated(colnames(PM10))]
PM10 <- PM10[, !(colnames(PM10) %in% duplicated)]
META <- META[!(META$AirQualityStation %in% duplicated), ]
META <- META[META$Countrycode == "IT", ]

data <- as.data.frame(PM10)
data$date <- as_date(rownames(PM10))

data <- data %>% 
  pivot_longer(cols = starts_with("STA"), names_to = "station") #%>% 
  #mutate(month = floor_date(date, "month"))









nuts3 <- read_sf("data/NUTS_RG_60M_2016_4326_LEVL_3.geojson")


nuts3 <- nuts3 %>% 
  filter(CNTR_CODE %in% countries) %>% 
  dplyr::select(id, NUTS_NAME, geometry)


ggplot(nuts3) +
  geom_sf() +
  coord_sf()


# densità della popolazione

pop_density <- get_eurostat('demo_r_d3dens', time_format = "num", filters = list(geoLevel="nuts3", time="2018")) %>% 
  dplyr::select('id'=geo, 'pop_density'=values)

nuts3 <- nuts3 %>% 
  left_join(pop_density)


gdp <- get_eurostat('nama_10r_3gdp', time_format = "num", filters = list(geoLevel="nuts3", time="2017", unit = "EUR_HAB")) %>%
  dplyr::select('id'=geo, 'gdp'=values)

nuts3 <- nuts3 %>%
  left_join(gdp)


popgdp <- get_eurostat('nama_10r_3popgdp', time_format = "num", filters = list(geoLevel="nuts3", time="2017")) %>% 
  dplyr::select('id'=geo, 'popgdp'=values) # in thous

nuts3 <- nuts3 %>% 
  left_join(popgdp)



business_size <- get_eurostat('bd_size_r3', filters = list(geoLevel="nuts3", time="2017", indic_sb = "V11910", sizeclas = "TOTAL")) %>% 
  dplyr::select('id'=geo, 'business_size'=values) # in thous

nuts3 <- nuts3 %>% 
  left_join(business_size)

# 
# freight_loading <- get_eurostat('road_go_na_rl3g', time_format = "num", filters = list(geoLevel="nuts3", time="2017")) %>% 
#   dplyr::select('id'=geo, 'freight_loading'=values) # in thous
# 
# nuts3 <- nuts3 %>% 
#   left_join(freight_loading)
# 
# 
# freight_unloading <- get_eurostat('road_go_na_ru3g', time_format = "num", filters = list(geoLevel="nuts3", time="2017")) %>% 
#   dplyr::select('id'=geo, 'freight_unloading'=values) # in thous
# 
# nuts3 <- nuts3 %>% 
#   left_join(freight_unloading)




# 
# ggplot(nuts3) +
#   geom_sf(aes(fill=pop_density, color=pop_density)) +
#   ylim(37, 47) +
#   xlim(5, 20) +
#   coord_sf() +
#   ggtitle("Pop density")
# 
# 
# ggplot(nuts3) +
#   geom_sf(aes(fill=gdp, color=gdp)) +
#   ylim(37, 47) +
#   xlim(5, 20) +
#   coord_sf() +
#   ggtitle("GDP")
# 
# 
# ggplot(nuts3) +
#   geom_sf(aes(fill=business_size, color=business_size)) +
#   ylim(37, 47) +
#   xlim(5, 20) +
#   coord_sf() +
#   ggtitle("Business size")
# 
# 
# ggplot(nuts3) +
#   geom_sf(aes(fill=freight_loading, color=freight_loading)) +
#   ylim(37, 47) +
#   xlim(5, 20) +
#   coord_sf() +
#   ggtitle("Freight loading")
# 
# 
# ggplot(nuts3) +
#   geom_sf(aes(fill=freight_unloading, color=freight_unloading)) +
#   ylim(37, 47) +
#   xlim(5, 20) +
#   coord_sf() +
#   ggtitle("Freight unloading")


# 
# # covariate spazio-temporali mensili
# 
# # heating degree days
# montly_hdd <- get_eurostat('nrg_chddr2_m', filters = list(geoLevel="nuts3", 
#                                                           time=c("2018M01", "2018M02", "2018M03", 
#                                                                  "2018M04", "2018M05", "2018M06", 
#                                                                  "2018M07", "2018M08", "2018M09", 
#                                                                  "2018M10", "2018M11", "2018M12"), 
#                                                           indic_nrg="HDD")) %>%
#   select('id'=geo, month=time, 'hdd'=values)
# 
# # cooling degree days
# montly_cdd <- get_eurostat('nrg_chddr2_m', filters = list(geoLevel="nuts3", 
#                                                           time=c("2018M01", "2018M02", "2018M03", 
#                                                                  "2018M04", "2018M05", "2018M06", 
#                                                                  "2018M07", "2018M08", "2018M09", 
#                                                                  "2018M10", "2018M11", "2018M12"), 
#                                                           indic_nrg="CDD")) %>%
#   select('id'=geo, month=time, 'cdd'=values)





nuts3_data <- st_drop_geometry(nuts3)
nuts3_spatial <- dplyr::select(nuts3, id, geometry)


stations <- META %>% 
  dplyr::select(station=AirQualityStation, lon=Longitude, lat=Latitude, alt = Altitude, 
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

# 
# ggplot(nuts3) +
#   geom_sf(aes(fill=pop_density, color=pop_density)) +
#   geom_sf(data=stations, color="red", size=0.5) +
#   ylim(37, 60) +
#   xlim(-10, 20) +
#   coord_sf() +
#   theme_minimal() +
#   ggtitle("Pop density and stations")

coordinates <- st_coordinates(stations)



new_extent <- extent(5, 20, 36, 48)
class(new_extent)



alt <- getData(name = "alt", country = "ITA", mask = FALSE)
alt_cropped <- crop(x = alt, y = new_extent)
alt_df <- as.data.frame(alt_cropped, xy = TRUE) 

ggplot() +
  geom_raster(data = alt_df , aes(x = x, y = y, fill = ITA_alt)) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  theme_void() +
  ggtitle("Altitudine")




#precipitazioni

prec <- raster("raster/wc2.1_2.5m_prec_2018-01.tif")
prec_cropped <- crop(x = prec, y = new_extent)
prec_df <- as.data.frame(prec_cropped, xy = TRUE)

ggplot() +
  geom_raster(data = prec_df , aes(x = x, y = y, fill = wc2.1_2.5m_prec_2018.01)) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  theme_void() +
  ggtitle("Precipitazioni in mm - Gennaio 2018")

#tempmin

tmin <- raster("raster/wc2.1_2.5m_tmin_2018-01.tif")
tmin_cropped <- crop(x = tmin, y = new_extent)
tmin_df <- as.data.frame(tmin_cropped, xy = TRUE)

ggplot() +
  geom_raster(data = tmin_df , aes(x = x, y = y, fill = wc2.1_2.5m_tmin_2018.01)) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  theme_void() +
  ggtitle("Temp min in C - Gennaio 2018")

#tempmmax

tmax <- raster("raster/wc2.1_2.5m_tmax_2018-01.tif")
tmax_cropped <- crop(x = tmax, y = new_extent)
tmax_df <- as.data.frame(tmax_cropped, xy = TRUE)

ggplot() +
  geom_raster(data = tmax_df , aes(x = x, y = y, fill = wc2.1_2.5m_tmax_2018.01)) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  theme_void() +
  ggtitle("Temp max in C - Gennaio 2018")

#wind

wind <- raster("raster/wc2.1_30s_wind_01.tif")
wind_cropped <- crop(x = wind, y = new_extent)
wind_df <- as.data.frame(wind_cropped, xy = TRUE)

ggplot() +
  geom_raster(data = wind_df , aes(x = x, y = y, fill = wc2.1_30s_wind_01)) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  theme_void() +
  ggtitle("Wind speed - Gennaio 2018")





stations$prec <- raster::extract(prec, coordinates)
stations$alt <- raster::extract(alt, coordinates)
stations$tmin <- raster::extract(tmin, coordinates)
stations$tmax <- raster::extract(tmax, coordinates)
stations$wind <- raster::extract(wind, coordinates)






data <- right_join(data, stations)







data <- data %>% 
  filter(date < dmy("10/01/2018"))
data$time <- as.numeric(data$date) - as.numeric(data$date)[1]+1
data$logPM10 <- log(ifelse(data$value == 0, 1, data$value))


ggplot(data) + 
  geom_density(aes(x=logPM10))

colnames(data)

to_scale <- c("alt", "prec", "tmin", "tmax", "wind", "gdp", "pop_density", "popgdp", "business_size")
mean_covariates <- apply(data[,to_scale],2,mean, na.rm = T)
sd_covariates <- apply(data[,to_scale],2,sd, na.rm = T)
data[,to_scale] <- scale(data[,to_scale],center=mean_covariates, scale=sd_covariates)


#rm(list=setdiff(ls(), c("data", "stations", "nuts3", "coordinates")))

