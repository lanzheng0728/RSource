library(ggplot2)
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds),100),]
?sample
str(dsmall)
qplot(carat,price,data=diamonds)
qplot(log(carat),log(price),data=diamonds)
qplot(carat,x*y*z,data=diamonds)
qplot(carat,price,data=dsmall,color=color)
qplot(carat,price,data=dsmall,shape=cut)
qplot(carat,price,data=diamonds,alpha=I(1/10),geom=c("point","smooth")) #se=TRUE

qplot(carat,price,data=dsmall,geom=c("point","smooth"),span=0.2)
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method="lm")
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method="lm",formula=y~ns(x,5))

qplot(color,price,data=diamonds,geom="jitter") #"boxplot"

qplot(carat,data=diamonds,geom="histogram",binwidth=0.1,fill=color) #"density"
qplot(carat,data=diamonds,geom="density",color=color) #"density

qplot(color,data=diamonds,geom="bar",weight=carat)


?economics
qplot(date,unemploy/pop,data=economics,geom="line")
qplot(date,uempmed,data=economics,geom="line")
str(economics)

qplot(unemploy/pop,uempmed,data=economics,geom=c("point","path"))
year <- function(x) as.POSIXlt(x)$year+1900
qplot(unemploy/pop,uempmed,data=economics,geom=c("path"),color=year(date))

qplot(carat,data=diamonds,facets=color~.,
      geom="histogram",binwidth=0.1)
qplot(carat,..density..,data=diamonds,facets=color~.,
      geom="histogram",binwidth=0.1) #显示密度而不是频度
qplot(carat,data=diamonds,facets=color~.,weight=price,
      geom="histogram",binwidth=0.1)

qplot(displ,hwy,data=mpg,color=factor(cyl))


 p <- ggplot(diamonds,aes(carat,price,color=cut))
p <- p+layer(geom="point")
p <- ggplot(diamonds,aes(x=carat))
p <- p +layer(
  geom="bar",
  geom_params=list(fill="steelblue"),
  stat="bin",
  stat_params=list(binwidth=2)
  )
ggplot(msleep,aes(sleep_rem/sleep_total,awake))+geom_point()
##等价于
qplot(sleep_rem/sleep_total,awake,data=msleep)

##qplot加图层
qplot(sleep_rem/sleep_total,awake,data=msleep)+geom_smooth()
#等价于
qplot(sleep_rem/sleep_total,awake,data=msleep,geom=c("point","smooth"))
#等价于
ggplot(msleep,aes(sleep_rem/sleep_total,awake))+
  geom_point()+geom_smooth()

p <- ggplot(mtcars,aes(mpg,wt,colour=cyl))+geom_point()
p
mtcars <- transform(mtcars,mpg=mpg^2)
p %+% mtcars #更改数据集
library(nlme)
qplot(age,height,data=Oxboys,geom=c("line"))
p <- ggplot(Oxboys,aes(age,height,group=Subject))+geom_line()
p
boysbox <- ggplot(Oxboys,aes(Occasion,height)) + geom_boxplot()
library(scales)
plot <- ggplot(economics,aes(x= date, y = psavert)) + geom_line() + ylab("Personal savings rate") +geom_hline(xintercept=0, colour="grey50")
plot +scale_x_date(limits=as.Date(c("2004-01-01","2005-01-01")))  
plot +scale_x_date(limits=as.Date(c("2004-01-01","2005-01-01")),labels=date_format("%m"))
plot +scale_x_date(limits=as.Date(c("2004-01-01","2005-01-01")),labels=date_format("%B"))
qplot(gdpPercap,lifeExp,data=X)

X <- read.delim("http://www.stat.ubc.ca/~rickw/gapminderDataFiveYear.txt")
str(X)
qplot(log(gdpPercap),lifeExp,data=X,color=year,size=pop,shape=continent)
