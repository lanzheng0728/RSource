library(maptools)
library(ggplot2)
library(ggmap)
library(maps)
#library(rgeos)
#library(shapefiles)
library(geosphere)
library(plyr)
library(sp)
geocode("Beijing")
# 读取都市地图文件 读取版图地图文件
urbanareasin <- readShapePoly("ne_10m_urban_areas\\ne_10m_urban_areas.shp")
worldmapsin <- readShapePoly("ne_10m_admin_0_countries\\ne_10m_admin_0_countries.shp")
# 以下为格式转化
worldmap <- fortify(worldmapsin)
urbanareas <- fortify(urbanareasin)
gpclibPermit()

# 开始抽取机场数据
airports <- read.table("airports.dat", sep = ",", header = FALSE)
worldport <- airports[airports$V5 != "", c("V3", "V5", 
                                           "V7", "V8", "V9")]
names(worldport) <- c("city", "code", "lan", "lon", "att")
worldport$lan <- as.numeric(as.character(worldport$lan))
worldport$lon <- as.numeric(as.character(worldport$lon))

data3.redu2s <- read.table("routes.dat", sep = ",", header = FALSE)
colnames(data3.redu2s)[c(3, 5)] <- c("From",
                                     "To")
# 找出所有航线有标识的机场（这里的data3.redu2s是我个人的航线数据
# 读者可以用上文提到的航线数据routes.dat代替）
lineinworld <- (data3.redu2s$From %in% worldport$code)& 
  (data3.redu2s$To %in% worldport$code) & 
  (data3.redu2s$From == "PEK")
# 有453条航线无标识
table(lineinworld)
# colnames(data3.upro1)
worldline <- data3.redu2s[lineinworld, c("From", 
                                         "To")]
flights.ag <- ddply(worldline, c("From", 
                                 "To"), function(x) count(x$To))
# 计算三字码映射到机场
flights.ll <- merge(flights.ag, worldport, by.x = "From", 
                    by.y = "code", all.x = T)
flights.ll <- flights.ll[order(flights.ll$From, 
                               flights.ll$To), ]
flights.lf <- merge(flights.ag, worldport, by.x = "To", 
                    by.y = "code", all.x = T)
flights.lf <- flights.lf[order(flights.lf$From, 
                               flights.lf$To), ]
# beijing.ll <-
# c(worldport$lon[worldport$code=='PEK'], worldport$lan[worldport$code=='PEK'])
rts <- gcIntermediate(flights.lf[, c("lon", "lan")], 
                      flights.ll[, c("lon", "lan")], 100, breakAtDateLine = FALSE, 
                      addStartEnd = TRUE, sp = TRUE)
# rts.ff <-
# fortify.SpatialLinesDataFrame(rts)flights.lf[,c('lon', 'lan')]
rts <- as(rts, "SpatialLinesDataFrame")
# 航线坐标数据
rts.ff <- fortify(rts)
# 航线信息与航线坐标信息关联
flights.ll$id <- as.character(c(1:nrow(flights.ll)))

gcircles <- merge(rts.ff, flights.ll, all.x = T, by = "id")

flt <- c(geom_line(aes(long,lat,group=group, color=freq, alpha=freq), size=0.1, data= gcircles))

####绘制背景
wrld<-c(geom_polygon(aes(long,lat,group=group), size = 0.1, colour= "#090D2A", fill="#090D2A", alpha=1, data=worldmap))
urb <-c( geom_polygon(aes(long, lat,group=group), size = 0.1, color = "#FDF5E6", fill= "#FDF5E6", alpha = 0.8, data = urbanareas))
xquiet <-  scale_x_continuous("", breaks=NULL）
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list( xquiet，yquiet)
# plot画图到文件plot2.png
png("plot_flight.png", width=6000, height=2000)
ggplot() +wrld+ flt+
  urb+coord_equal()+
  quiet+
  theme(panel.background = element_rect(fill='#00001C',colour='#00001C'))
dev.off()