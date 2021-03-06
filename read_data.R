library(maps)
library(ggplot2)
library(fields)
library(leaflet)
library(MBA)

load('data_pm10.RData')

## plot monitoring stations
plot(META$Longitude,META$Latitude,pch=19,cex=0.9,xlab='Longitude',ylab='Latitude')
map("world",add=T)


## plot stations on the map
#Set up a leaflet basemap to help visualize the data.
#Leaflet is one of the most popular open-source JavaScript libraries for interactive maps. 
#We use  the pipe operator %>% to reduce clutter.

base.map <- leaflet(width = "100%") %>% 
  addProviderTiles("Stamen.Terrain", group = "Terrain") %>% 
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
  addLayersControl(baseGroup = c("Terrain", "Satellite"), options = layersControlOptions(collapsed = FALSE))

#This code below produces a clickable dynamic map.
base.map %>% 
  addCircleMarkers(lng = META$Longitude, lat = META$Latitude,
                   stroke = FALSE, radius = 5, fillOpacity = 0.9)



## plot PM10 concentrations at some days 
k = 61 #2018-03-02 
k = 62 #"2018-03-03
k = 100 #2018-04-10
k = 116 #2018-04-26

example = as.data.frame(cbind(PM10 = PM10[k,] ,
                              Longitude = META$Longitude,
                              Latitude = META$Latitude,
                              country = as.numeric(as.factor(META$Countrycode))))

# EU Contries
some.eu.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium",  "Netherlands",
  "Denmark",  "Italy")

some.eu.maps <- map_data("world", region = some.eu.countries)

ex = ggplot(aes(x = long, y = lat, group = group),data = some.eu.maps) +
  geom_polygon(fill="white", colour = "lightgray")+
  labs(x = "Longitude",y="Latitude")+
  geom_point(aes(x=Longitude, y=Latitude, color=PM10,group = country),data=example)+
  scale_color_gradientn(colours = tim.colors(5),limits=c(0,104),na.value ="white")+
  ggtitle(rownames(PM10)[k])+
  theme_bw()
ex

## example surface plot
k = 62 #"2018-03-03
example = as.data.frame(cbind(PM10 = PM10[k,] ,
                              Longitude = META$Longitude,
                              Latitude = META$Latitude,
                              country = as.numeric(as.factor(META$Countrycode))))
example2 = example[complete.cases(example),] #remove missing values
ex.surf=mba.surf(cbind(example2$Longitude,example2$Latitude,example2$PM10),
                 no.X = 300, no.Y = 300, 
                 extend = TRUE, sp = TRUE)$xyz.est
par(mar=c(1,1,1,1))
image.plot(ex.surf,zlim=range(ex.surf@data),horizontal = F)
map('world',add=T)

## plot the time series of PM10 concentration observed in Denmark
matplot(PM10[,META$Countrycode=='DK'],type='l',lty=1,
        ylab=expression(PM[10]),main='Denmark',axes=F)
axis(2)
axis(1,at=seq(1,365,l=30),lab=rownames(PM10)[seq(1,365,l=30)],
     las=2,cex.axis=0.7)

## plot the average PM10 concentration in Italy
plot(rowMeans(PM10[,META$Countrycode=='IT'],na.rm=T),type='l',lty=1,xlab='',
        ylab=expression(paste('average ', PM[10])),main='Italy',axes=F)
axis(2)
axis(1,at=seq(1,365,l=30),lab=rownames(PM10)[seq(1,365,l=30)],
     las=2,cex.axis=0.7)
box()

## histogram
par(mfrow=c(1,2),mar=c(3,2,1,1),mgp=c(2,1,0),cex.axis=0.8)
hist(c(PM10),xlab = expression(PM[10]),main="",ylab='',
     freq=T,breaks=30)
box()
hist(c(log(PM10)),xlab = expression(log(PM[10])),
     main='',ylab='',freq=T,breaks=30)
box()

## if you employ a logaritmic transformation, you could replace PM10 = 0 with PM10 = 1,
# to avoid numerical issues  

