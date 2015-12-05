csv <- read.csv(file="C:\\Users\\alan\\Desktop\\R\\dingdantongji.csv",
                       header=TRUE)
summary(csv)
View(csv)
table(csv$option,csv$city)
row_num(csv)
sqldf("select distinct option from csv",row.names=TRUE)
head(csv)
dt_tradedate <- as.Date(csv[,7])
dt_duedate <- as.Date(csv[,11])
View(csv1)
min(csv1$amount)
sum(csv1$amount)
csv1 <- cbind(csv[,c(-7,-11)],dt_tradedate,dt_duedate)
summary(csv1)
min(csv1$dt_tradedate)
max(csv1$dt_tradedate)
csv1[which(as.character(csv1$dt_tradedate)=="2013-05-06"),]
csv1[which(as.character(csv1$dt_duedate)=="2013-07-19"),]

install.packages("zoo")
library(zoo)
csv_tradeamount <- csv1[,c(7,10)]
csv_dueamount <- csv1[,c(7,11)]
qplot(dt_tradedate,data=csv1,weight=amount/10000,binwidth=0.5,geom="bar",color=area)
#######
nrow(csv)
nrow(csv1)
nrow(csv_tradeamount)
head(csv_tradeamount)
order(csv_tradeamount$dt_tradedate)
############
csv_dueamount$amount <- csv_dueamount$amount/10000
dt_due <- csv_dueamount %>% 
  group_by(dt_duedate) %>% 
  summarise(amount=sum(amount)) %>%
  arrange(dt_duedate)
head(dt_due)

csv_tradeamount <- csv_tradeamount[order(csv_tradeamount$dt_tradedate)，]
min(csv_tradeamount$amount)
csv_tradeamount$amount <- csv_tradeamount$amount/10000
sum(csv_tradeamount$amount)

dt_csv <- aggregate(csv_tradeamount$amount,by=list(csv_tradeamount$dt_tradedate),FUN=sum)
#spcsv_tradeamount <- split(csv_tradeamount,csv_tradeamount[,c("dt_tradedate")])
#dt_csv <- lapply(spcsv_tradeamount,FUN=function(x) sum(x$amount))
head(dt_csv)
sum(dt_csv$x)

zcsv <- zoo(dt_csv$x,order.by = dt_csv$Group.1)
zdue <- zoo(dt_due$amount,order.by = dt_due$dt_duedate)

z <- merge(zcsv,zdue,fill = 0)
head(z)
View(z)
#z1 <- as.data.frame(z)
vol <- cumsum(z$zcsv-z$zdue)
z1 <- cbind(z,vol)

library(ggplot2)
###################
x.Date <- as.Date(paste(2003, 02, c(1, 3, 7, 9, 14), sep = "-")) # 构建数据对象
x <- zoo(rnorm(5), x.Date)
xlow <- x - runif(5)
xhigh <- x + runif(5)
z <- cbind(x, xlow, xhigh)

##################################
g<-ggplot(aes(x = Index, y = Value), data = fortify(x, melt = TRUE))
g<-g+geom_line()
g<-g+geom_line(aes(x = Index, y = xlow), colour = "red", data = fortify(xlow))
g<-g+geom_ribbon(aes(x = Index, y = x, ymin = xlow, ymax = xhigh), data = fortify(x), fill = "darkgray")
g<-g+geom_line()
g<-g+xlab("Index") + ylab("x")
g
##############################

z2 <- fortify(z1,melt-TRUE)

str(z2)
idx1 <- format(z2[,1],format = "%y-%m")
z3 <- cbind(z2,idx1)
z4 <- z3 %>%
  group_by(idx1) %>%
  summarize(monthamount=sum(vol)) %>%
  arrange(idx1) %>%
  select(idx1,monthamount) 
str(z4)
barplot(z4$monthamount~z4$idx1)

p <- ggplot(mtcars)+aes(x=wt,y=mpg)
p+geom_point(aes(colour = qsec))
