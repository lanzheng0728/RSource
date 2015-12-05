library(RCurl)
library(XML)
library(ggplot2)
library(ggmap)
library(geosphere)
library(reshape2)

setwd("C:\\Users\\alan\\Desktop\\R")

website  <-  "http://www.pm25x.com"
pm  <-  getURL(website,.encoding="UTF-8")
pmweb  <-  htmlParse(pm,encoding="UTF-8")
pmtotal  <-  getNodeSet(doc=pmweb,path="//dd/a")
pmurl <- xpathApply(doc=pmweb,function(x) c(xmlValue(x), xmlAttrs(x)[["href"]]),path="//dd/a")
pmcity = sapply(X=pmtotal,FUN=xmlValue)

#for( i in pmurl[])
#  print(i)

data = NULL
if(!file.exists(paste("pmdata_",format(Sys.Date(),"%Y%m%d"),".RData")))
{
  for( i in pmurl[])
  {
    city = i[1]
    cat(paste("Get data for",city,"... ...\n"))
    #url <- "http://www.pm25x.com/city/beijing.htm"
    #web <- getURL(url=url,.encoding="UTF-8")
    #web <- htmlParse(web,encoding="UTF-8")
    #pm <- xpathApply(doc=web,function(x) xmlValue(x),path="//div[@class='aqivalue']")
    cityweb = getURL(url=paste(website,i[2],sep="/"),.encoding="UTF-8")
    cityweb = htmlParse(cityweb,encoding="UTF-8")
    citypmnode = getNodeSet(doc=cityweb,path="//div[@class='aqivalue']")
    citypm=xpathApply(doc=cityweb,function(x) xmlValue(x),path="//div[@class='aqivalue']")
   
    if(length(cityweb) == 0)
    {
      cat("Date is incorrect... skip...\n")
      next
    }
    data = rbind(data,cbind(city=city,pm=as.numeric(as.character(citypm[[1]]))))
    Sys.sleep(time = 3)
  }
  save(data,file=paste("pmdata_",format(Sys.Date(),"%Y%m%d"),".RData"))
}else
  load("pmdata_ 20151201 .RData")

#data <- as.data.frame(data,stringsAsFactors=FALSE,.encoding="UTF-8")
geo <- NULL
if(!file.exists("geo190.RData"))
{
  for(j in 1:dim(data)[1])
  {
    d <- data[j,]
    cat(paste("Get lon&lat for",d[1],"... ...\n"))
  #  geo <- rbind(geo,geocode(d[1]))
  }
  save(geo,file="geo190.RData")
}else
  load("geo190.RData")
geodata <- cbind(data,geo)

geodata$pm = as.numeric(as.character(geodata$pm))
geodata <- geodata[complete.cases(geodata),]
pm.point <- c(geom_point(data=geodata,aes(lon,lat,color=pm),size=10 ,alpha=0.8))

#coordinates(geodata) <- c("lon","lat")
#coordinates(geodata)=~lon+lat
#spplot(geodata,"pm"£¬do.log=T)
#bubble(geodata,"pm")
#gridded(geodata)=TRUE

#library(gstat)
##############use poly function instead#######################
#lm.pm <- lm(pm~lon+lat+
#              I(lon*lat)+
#              I(lon^2)+I(lat^2)+
#              I(lon^2*lat)+I(lon*lat^2)+
#              I(lon^3)+I(lat^3)+
#              I(lon^5)+I(lat^5)+
#              I(lon^6)+I(lat^6),
#            data=geodata           
#            )
#############################################
#lm.pm <- lm(pm~poly(lon,lat,degree=8),geodata)

#geodata$fitted.s <- predict(lm.pm,geodata)-mean(predict(lm.pm,geodata))
#geodata$residuals <- residuals(lm.pm)
#spplot(geodata,c("fitted.s","residuals"))
#xlim <- c(84,129)
#ylim <- c(18,47)


#summary(lm.pm)
#x <- subset(geodata,select=c(lon,lat))
#y <- geodata$pm
#pred <- predict(lm.pm,x)
#tmp <- cbind(x,y,pred)
#plot(lm.pm)
#g <- ggplot(melt(ee),aes(x=Var1,y=Var2,fill=value))+geom_tile()
#g <- ggplot(geodata, aes(x=lon, y=lat, fill=pm)) + geom_tile()


library(maptools)
worldmapsin <- readShapePoly("ne_10m_admin_0_countries\\ne_10m_admin_0_countries.shp")
worldmap <- fortify(worldmapsin)
china <- worldmapsin[worldmapsin$NAME=="China",]
gpclibPermit()
#plot(worldmap),data=geodata
#length(worldmapsin)
#names(worldmapsin)
#table(iconv(worldmapsin$NAME, from = "GBK"))

##################################################
#x1 <- seq(from=73.60,to=134.77,length=100)
#x2 <- seq(from=15.77,to=53.56,length=50)
#pred.pm <- rep(0,5000)
#dim( pred.pm) <- c(100,50)
#for(i in 1:100)
#  for(j in 1:50)
#    pred.pm[i,j] <- predict(lm.pm,data.frame(lon=x1[i],lat=x2[j]))

#g <- ggplot(melt(pred.pm), aes(x=Var1,y=Var2,fill=value)) + geom_tile()
#m <- melt(pred.pm)
#plot(m$Var1,m$Var2)
#contour(m$value)
#################################################


china<-list(geom_polygon(aes(long,lat,group=group), size = 0.1, colour= "#bababa", fill="#bababa", alpha=1, data=china))
xquiet <-  scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list( xquiet,yquiet)


# plot»­Í¼µ½ÎÄ¼þplot2.png
png(paste("plot_pm25",format(Sys.Date(),"%Y%m%d"),".png",sep=""), width=1000, height=500)
g <- ggplot()+china+pm.point+
  scale_color_gradient2(high="red",low="green",mid = "yellow", midpoint = 150,space = "Lab" )+
  coord_equal()+
  quiet+
  theme(panel.background = element_rect(fill='#efefef',colour='#efefef'))
print(g)
dev.off()



