install.packages("maps")
install.packages("geosphere")
install.packages("maptools")
library(maps)
library(maptools)
library(geosphere)
map("world", ylim = c(-60, 90), mar = c(0, 0, 0, 0),lwd=0.05,col="#f2f2f2", fill=TRUE, bg="white",)
urbanareasin <- readShapePoly("ne_10m_urban_areas\\ne_10m_urban_areas.shp")
plot(urbanareasin)
class(urbanareasin)

xlim <- c(-171.738281, -56.601563)
ylim <- c(12.039321, 71.856229)
map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)

lat_ca <- 39.164141
lon_ca <- -121.640625
lat_me <- 45.213004
lon_me <- -68.906250
inter <- gcIntermediate(c(lon_ca, lat_ca), c(lon_me, lat_me), n=50, addStartEnd=TRUE)
lines(inter)

lat_tx <- 29.954935
lon_tx <- -98.701172
inter2 <- gcIntermediate(c(lon_ca, lat_ca), c(lon_tx, lat_tx), n=50, addStartEnd=TRUE)
lines(inter2, col="red")

airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header=TRUE) 
flights <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/flights.csv", header=TRUE, as.is=TRUE)

fsub <- flights[flights$airline == "AA",]
for (j in 1:length(fsub$airline)) {
  air1 <- airports[airports$iata == fsub[j,]$airport1,]
  air2 <- airports[airports$iata == fsub[j,]$airport2,]
  
  inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE)
  
  lines(inter, col="black", lwd=0.8)
}

#################################################
pal <- colorRampPalette(c("#f2f2f2", "black"))
colors <- pal(100)

map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)

fsub <- flights[flights$airline == "AA",]
maxcnt <- max(fsub$cnt)
for (j in 1:length(fsub$airline)) {
  air1 <- airports[airports$iata == fsub[j,]$airport1,]
  air2 <- airports[airports$iata == fsub[j,]$airport2,]
  
  inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE)
  colindex <- round( (fsub[j,]$cnt / maxcnt) * length(colors) )
  
  lines(inter, col=colors[colindex], lwd=0.8)
}
######################################################
# Unique carriers
carriers <- unique(flights$airline)

# Color
pal <- colorRampPalette(c("#333333", "white", "#1292db"))
colors <- pal(100)

for (i in 1:length(carriers)) {
  
  pdf(paste("carrier", carriers[i], ".pdf", sep=""), width=11, height=7)
  map("world", col="#191919", fill=TRUE, bg="#000000", lwd=0.05, xlim=xlim, ylim=ylim)
  fsub <- flights[flights$airline == carriers[i],]
  fsub <- fsub[order(fsub$cnt),]
  maxcnt <- max(fsub$cnt)
  for (j in 1:length(fsub$airline)) {
    air1 <- airports[airports$iata == fsub[j,]$airport1,]
    air2 <- airports[airports$iata == fsub[j,]$airport2,]
    
    inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE)
    colindex <- round( (fsub[j,]$cnt / maxcnt) * length(colors) )
    
    lines(inter, col=colors[colindex], lwd=0.6)
  }
  
  dev.off()
}
####################################################
wrld<-c(geom_polygon(aes(long,lat,group=group), size = 0.1, colour= "#090D2A", fill="#090D2A", alpha=1, data=worldmap))
urb <-c( geom_polygon(aes(long, lat,group=group), size = 0.1, color = "#FDF5E6", fill= "#FDF5E6", alpha = 0.8, data = urbanareas))
xquiet <-  scale_x_continuous("", breaks=NULL）
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list( xquiet，yquiet)
# plot画图到文件plot2.png
png("plot.png", width=6000, height=2000)
ggplot() +wrld+
  urb+coord_equal()+
  quiet+
  theme(panel.background = element_rect(fill='#00001C',colour='#00001C'))
dev.off()
