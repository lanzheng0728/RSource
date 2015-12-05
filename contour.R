ee<-array(rnorm(89*180),dim=c(89,180))
a <- array(rnorm(5),dim=c(2,2))
a
#If ee has 89 rows (corresponding to latitude I guess) then lati needs 89 values:
lati <- seq(-90,90,length=89) #Latitudes goes from -90 to 90 as far as I know :)
#Same thing with columns/longitude:
long <- seq(-180,180,length=180)

#you probably want your contour behind the continents so first an empty plot:
plot(NA, xlim=c(-180,180), ylim=c(-90,90), xlab="", ylab="", xaxs="i", yaxs="i")
#Then your contour (you need to transpose ee so that rows are longitudes):
contour(long, lati, t(ee), add=TRUE)

library(reshape2)
ggplot(melt(volcano), aes(x=Var1, y=Var2, fill=value)) + geom_tile()
class(volcano)
dim(volcano)
class(melt(volcano))
dim(melt(volcano))
