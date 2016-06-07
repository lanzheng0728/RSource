sessionInfo()
library(gbm)
library(stringr)
library(Hmisc)
library(ggplot2)
library(maps)
library(readr)
setwd("D:\\Git-lanzheng\\LendingClubData")
loans <- read_csv("LoanStats3a.csv", skip = 1)
 nrow(loans)

 str(loans)
 loans$Debt.To.Income.Ratio <- as.numeric(gsub("%", "", loans$Debt.To.Income.Ratio))
 
 library(jsonlite)
     jsoncars <- toJSON(mtcars,dataframe = "values",pretty=TRUE)
     
head(mtcars)
  
 
names(mtcars)
library(reshape2)
library(xlsx)
library(readxl)
library(plyr)
library(dplyr)

m <- mtcars[1:20,c(1,2,5)]

table(m[,3])
setwd("D:\\Git-lanzheng\\LendingClubData")
ttt <- fromJSON("t.json")
class(ttt)
str(ttt)
toJSON(ttt,pretty=TRUE)
tt <- as.data.frame(ttt)
stack(tt)
tttt <- melt(tt)
dcast(t,.~variable)
t(tt)

#{
#  "variable": ["纬度", "经度"],
#  "阿城财富一部": [45.5479, 126.9821],
#  "安庆财富二部": [30.5502, 117.0702],
#  "安庆财富一部": [30.5428, 117.0736],
#  "安丘财富一部": [36.4326, 119.2333],
#  "鲅鱼圈财富一部": [40.2505, 122.1369],
#  "蚌埠财富二部": [32.9375, 117.3816]
#}

e <- read_excel("mapdata.xlsx")
head(e)
eee <- e[,c(1,2,3)]
head(t(eee))
ee <- t(eee)
eeee <- as.data.frame(ee,row.names = F )
colnames(eeee) <- ee[1,]
toJSON(as.list(eeee[c(2,3),]))


mm <- melt(e[,c(1:3)],id.vars = "门店")
m <- dcast(mm,variable~门店)
mmm <- as.list(m)
n <- toJSON(mmm,pretty=TRUE)



#{
#  "people":
#  [
#    {"name":"梅州财富一部","coord":[116.13588741692,24.304541570301]},
#    {"name":"桂林财富一部","coord":[111.08221659918, 25.940385668447]}
#    ]
#} 

e <- read_excel("mapdata.xlsx")
colnames(e) <- NULL

f <- list(data.frame(name=e[,1],coord=list(e[,2],e[,3])))

str(f)

toJSON(list(store=f),pretty=TRUE)





