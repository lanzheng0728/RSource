require(dplyr)    
df <- data.frame(A = c(1, 1, 2, 3, 3), B = c(2, 3, 3, 5, 6))
df %>% group_by(A) %>% summarise(B = sum(B))

## Source: local data frame [3 x 2]
## 
##   A  B
## 1 1  5
## 2 2  3
## 3 3 11
x1 <- 0:10
x2 <- 10:0
x3 <- seq(10)
x4 <- seq(30,0,by=-3) #counting down by -3
x5 <- c(1,2,3,4,5,6,7)
x6 <- scan()
rm(list=ls())
ls()
#ctrl+l to clean the screen
#load file
trends.txt <- read.table(file="C:\\Users\\alan\\Desktop\\R\\Exercise Files\\Ch01\\01_07\\final\\GoogleTrends.txt",
                         header=TRUE,sep="\t")
str(trends.txt)
View(trends.txt)
trends.csv <- read.csv(file="C:\\Users\\alan\\Desktop\\R\\Exercise Files\\Ch01\\01_07\\final\\GoogleTrends.csv",
                       header=TRUE)
?UCBAdmissions
str(UCBAdmissions)
UCBAdmissions
View(UCBAdmissions)
margin.table(UCBAdmissions,1) #Admit
admit.dept <- margin.table(UCBAdmissions,3)
str(admit.dept)
barplot(admit.dept)
admit.dept
round(prop.table(admit.dept),2)*100
admit1 <- as.data.frame.table(UCBAdmissions)
admit2 <- lapply(admit1,function(x)rep(x,admit1$Freq))
admit3 <- as.data.frame(admit2)
admit4 <- admit2[,-4]

colors()
barplot(x,col=color()[102])

barplot(x,col=rgb(.54,.0,.0))

x <- c(12,3,21,17,13,9)
barplot(x)
browseURL("http://colorbrewer2.org")
require("RColorBrewer")
display.brewer.all()
display.brewer.pal(8,"Spectral")
blue <- brewer.pal(6,"Blues")
barplot(x,col=blue)

y=USArrests
View(y)
class(y)
c=cor(y)
class(c)
View(c)

e <- eigen(c)

m <- matrix(1,nrow=3,ncol=4)
n <- matrix(2,nrow=4,ncol=5)
diag(n)

x <- 1:3
y <- diag(x);y
#      [,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    2    0
#[3,]    0    0    3
diag(y)
#[1] 1 2 3
diag(3)
#      [,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1


 