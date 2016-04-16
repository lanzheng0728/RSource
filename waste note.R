

#   group_by(playerID) %>%    summarise(total = sum(G)) %>%    arrange(desc(total)) %>%    head(5)
1*1/6+5*1/6+10*1/6+20*1/6+50*1/6+100*1/6
sample(c(1:6),size =2,replace = F )

h <- list(a=rnorm(3),b ="this shouldn't print")
class(h)
print(h)
class(h) <- "myclass"
print.myclass <- function(x){
  cat("A is:",x$a,"\n")
}

print(h)
#查看类可用的泛型函数
methods(class="myclass")
methods(class="data.frame")

#查看反省函数可处理的类
methods(plot)

#查看源码
coef

#UseMethed暗示这是一个泛型函数，使用methods()
methods(coef)

getAnywhere(coef.aov)
getS3method("coef","aov")


##编写泛型函数
#1编写函数
xpos <- function(x,...) UseMethod("xpos")
xpos.xypoint <- function(x) x$x
xpos.rthetapoint <- function(x) x$r*cos(x$theta)

xpos
#2改变数据的类
x <- list(x=c(1,2))
x
x$x
class(x) <- "xypoint"

#3 调用泛型函数
xpos(x)

require(pryr)
otype(x)
class(x)

#修改
edit(mean)

####S4

setClass(Class = "Person", representation(name = "character",age = "numeric"))
setClass(Class = "Reporter", representation(title = "character"),contains = "Person")
yuchen <- new("Reporter", name = "yuchen", age = 22, title = "mgr")
class(yuchen)
yuchen@title
slot(yuchen, "age")

setClass("Person", representation(name = "character", age = "numeric"),
         prototype(name = NA_character_, age = NA_real_))
new("Person", name = "yuchen")@age

CheckAge <- function(object) {
  if (object@age <= 0) {
    stop("Age is negative.")
  }
}
setClass("Person", representation(name = "character", age = "numeric"),
         validity = CheckAge)
new("Person", age = -5)

setGeneric("prepare", function(object) {
  standardGeneric("prepare")
})
setMethod("prepare", signature(object = "Person"), function(object) {
  cat("Got Materials.\n")
})
prepare(new("Person"))


setMethod("prepare", signature(object = "Reporter"), function(object) {
  callNextMethod()
  cat("Slides are ready.\n")
})
prepare(new("Reporter"))


getClass("Person")
getClass("Reporter")
getClass("vector")

showMethods("prepare")
findMethods("prepare")
###############################编写自己的函数##############################
"%ooo%" <- function(x,y) x[!x %in% y]
(1:10) %ooo% c(3,4,8,12)

"%i%" <-  intersect
x <- 1:10
y <- 1:10
z <- 1:10
x %i% y %i% z

#异常
stop("stop")
warning("warning")

parse(text="0==1")
eval(parse(text="0==1"))


#########################数据类型####################

x <- rbinom(n = 10,size=2,p=c(0.2,0.3,0.5))
f <- factor(x)
factor(f,levels = 0:5)
factor(f,labels = c("a","b","c"))

t <- table(x)
o <- ordered(x)

Lst <- list(name="Fred",wife="Mary",no.children=3,child.ages=c(4,7,9))
Lst[[2]]
Lst$wife
dim(z) <- c(3,5,100)
rm(z)

d <- matrix(1:9,nc=3)
d[1:5]
seq <- "GGGGCCCG"
seq <- strsplit(seq,"")
seq
class(seq)
seq[[1]]=="G"
s <- c("a","b")
s
paste(s)
paste(s,collapse = "+",sep="")

nchar(c("ASDF","asdf"))

#分数 转化为近似分数
require(MASS)
f <- fractions(0.291667)
class(f)
fractions(pi)
#法里数列
#0 1 
#(2):---3counts
#0 1/2 1 
#(3):---5counts
#0 1/3 1/2 2/3 1 
#(4):---7counts
#0 1/4 1/3 1/2 2/3 3/4 1 
#(5):---11counts
#0 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1 

#日期
date()
Sys.time()
Sys.Date()

class(Sys.time())
#POSIXct/POSIXlt ct是从1970年开始的秒数 lt是个list

s <- "08:10:00"
z <- strptime(s,"%H:%M:%S")
z
class(z)
z$year

###读取文件
#剪贴板
read.table(file="clipboard",sep = "\t",header=T)

write.table(data,file="clipboard",sep="\t",col.names = NA)

save(s,y,z,file="xyz.RData")
load("xyz.RData")
#重定向
sink() #定向到控制台
search() #载入包

#require(foreign) #读取spss格式
#require(xtable) #转换latex格式

dist.euclidean <- function(x,y){
    res <- sqrt(sum((x-y)^2))
    res
}

X <- data.frame(x1=c(4,1,3,3,7,4,5,3,4,6,6,2),
                x2=c(3,3,3,7,4,1,5,6,4,3,5,4),
                g=c(rep(1,5),rep(2,7)))
X

plot(x2~x1,col=c("red","blue")[g],data=X)
points(6,5,pch=8,cex=3)

d <- apply(X[,1:2], 1,FUN=dist.euclidean,y=c(6,5))
class(d)
  
###多元数据操作####
require(multilevel)
data(package="multilevel")
data(cohesion)
cohesion

group.size <- data.frame(UNIT=c("1044B","1044B","1044C","1044C"),
                          PLATOON=c("1ST","2ND","1ST","2ND"),
                          PSIZE=c(3,3,2,3))
group.size  
new.cohesion <- merge(cohesion,group.size,by=c("UNIT","PLATOON"))
new.cohesion
data(bhr2000)
names(bhr2000)
nrow(bhr2000)
length(unique(bhr2000$GRP))


x=1+2i
x
eval(parse(text = "0==1"))


######################数学计算#######################
12.1/3
12.1%/%3
12.1%%3

17%in%1:100

Re(x) #实部
Im(x) #虚部
Mod(x) #模
Arg(x) #角度
Conj(x) #共轭

#插值

n <- 10
x <- seq(0,1,length=n)
y <- 1-2*x+.3*rnorm(n)
plot(x,y)
x
y
plot(spline(x,y,n=100),type="l")
abline(lm(y~x))

#排列组合
choose(4,2)
combn(5,2)


#积分
integrand <- function(x){1/((x+1)*sqrt(x))}
integrate(integrand,lower=0,upper=Inf)


#解方程
#y=cos(x)-2x 即y=0时x的值
#画图
curve(x^2,-10,10)
curve(cos(x)-2*x,-10,10)
abline(h = 0,lty=2)


u <- uniroot(f=function(x) cos(x)-2*x,interval=c(-10,10))

#1000=y*(3+x)*(1+y)^4

eq <- function(y,x){
  (1000-y*(3+x)*(1+y)^4)
}

r <- rep(0,100)
x <- 1:100
for(i in x){
  r[i] <- uniroot(eq,c(-100,100),x=i)$root
}
plot(r~x)
require(rootSolve)
uniroot.all(f=function(x) x^2-1,interval=c(-10,10))


#非线性方程组
model <- function(x){
  c(F1=x[1]^2+x[2]^2-1,F2=x[1]^2-x[2]^2+0.5)
}
ss <- multiroot(f=model,start = c(1,1))
ss




#QR分解
#正交矩阵Q和上三角矩阵R
#Q%*%t(Q)==I

x <- matrix(1:20,nr=4)
x
q <- qr(x)
q
#其中$qr矩阵上三角为QR分解的R矩阵
#下三角为正交矩阵Q的部分信息
#$qraux为Q的附加信息
Q <- qr.Q(q)
Q
R <- qr.R(q)
R
qr.X(q)
#重构x
Q%*%R

#svd
A <- t(matrix(c(1:8,10),nr=3))
s=svd(A)
s
s$u %*% diag(s$d) %*% t(s$v)


#谱分解
#Q可以分解为 Q <- UAU^-1
#U是非奇异矩阵 
x <- matrix(c(1,-1,-1,1),nr=2)
x

y <- eigen(x)
y
a <- y$values
u <- y $vectors
u%*%diag(a)%*%solve(u)
Q%*%t(Q)

#lsfit()最小二乘估计

x <- seq(0.0,0.8,by=0.2)
y <- c(0.9,1.9,2.8,3.3,4.2)
l <- lsfit(x,y)
l

#QR分解
X <- matrix(c(rep(1,5),x),nc=2)
X
qr(X)

example(expm1)


#中心化和标准化

x <- 1:10
scale(x)
y <- scale(x)
mean(y)
y
var(y)
sd(y)

#误差传播！！！！！

#距离系数
#exclidean maximum manhattan canberra binary minkowski

x <- matrix(c(0,1,2,3,0,1,2,3),nc=2)

x
dist(x,diag=T,method = "manhattan")
dist(x,diag=T,method = "euclidean")

dist(x,diag=T,method="minkowski",p=0.5)
dist(x,diag=T,method="minkowski",p=1)
dist(x,diag=T,method="minkowski",p=2)

dist(x,diag=T,method="maximum")

dist(x,diag=T,method="canberra")


#相似系数
x1 <- 1:10
x2 <- 11:20
#余弦系数
a <- sum(x1*x2)
b <- sqrt(sum(x1^2)*sum(x2^2))
r <- a/b
r

x3 <- 2*x1#x3 与x1完全相似
a <- sum(x3*x1)
b <- sqrt(sum(sum(x1^2)*sum(x3^2)))
r <- a/b
r

x4 <- -2*x1#负相关
a <- sum(x1*x4)
b <- sqrt(sum(x1^2)*sum(x4^2))
r <- a/b
r

#相关系数
cor(x1,x2)
cor(x1,x3)
cor(x1,x4)


##########################基本统计分析##########################

x <- exp(seq(-1,3,by=0.1))
plot(x)

#D(X)=E(X-E(X))^2   => D(X)=E(X^2)-E(X)^2

var(x)
sd(x)

# 累积
cummax(x)
cummin(x)
plot(cummax(x))

#差分
diff(x)
plot(diff(x))


#偏度
skewness <- function(x){
  sum((x-mean(x))^3/length(x))
  
}

skewness(x)

#峰度
#4阶中心距
#2阶中心距
#峰度 <- m4/m2^2 -3

kurtosis <- function(x){
  a <- mean(x)
  n <- length(x)
  m4 <- sum((x-a)^4)/n
  m2 <- sum((x-a)^2)/n
  kurt <- m4/m2^2 -3
  kurt
  
}

kurtosis(x)
  
plot(x)

#变异系数 coeffocoent of varoabolity
#CV=sd(x)/mean(x)

CV <- function(x){
  sd(x)/mean(x)
}

CV(x)
boxplot(x)

#离散数据
x <- c(1,1,2,0,2,0,0,1,1,0)
x
y <- sample(c("y","n"),10,replace=T)
y
table(x)
table(y)
table(x,y)
x <- c("Yes","No","No","Yes","Yes")
table(x)
y <- 1:9
factor(x)
factor(y)
#gl()产生因子
gl(n,k,length=n*k,labels=1:n,ordered=F)

#example:抽烟学生时间少
x <- NULL
x$smokes <- sample(c("Y","N"),10,replace = T) #抽烟与否
x$study <- c(1,2,2,3,3,1,2,1,3,2) #学习时间
table(x)
tmp <- table(x)
str(tmp)
options("digits") # 默认打印字符长度 7
options(digits = 3)
prop.table(tmp,1)
options(digits = 7)


#伯努力分布
n <- 200
x <- sample(c(-1,1),n,replace = T,prob = c(.2,.8))
plot(cumsum(x),type='l')
x

#二项分布随机数
rbinom(5,10,.5)
rbinom(5,10,.1)

dbinom(5,10,p=.5)
x <- rbinom(10000,9,0.5)
table(x)
table(rbinom(10000,10,.3))
#设100次实验A发生的概率为0.3 A发生20次的概率是
dbinom(20,100,0.3)
#A发生20< <60的概率
sum(dbinom(20:60,100,0.3))

dbinom(1,2,.5)

#超几何分布
plot(phyper(30:130,934,10000-934,1000),t="l")
plot(dhyper(30:120,934,13000-934,1000),t="l")



#星期替换
days <- sample(c("星期一","星期二","星期三","星期四","星期五","星期六","星期日"),20,,replace = T)
days[days=="星期一"] <- 1
days[days=="星期二"] <- 2
days[days=="星期三"] <- 3
days[days=="星期四"] <- 4
days[days=="星期五"] <- 5
days[days=="星期六"] <- 6
days[days=="星期日"] <- 7
days

load("abc.rdata")
View(date_freq)
View(train)
View(date)


train <- date[format(date$Var1,"%m") %in% c(11,12),]
date_freq <- date
date_freq2 <- date
date_freq3 <- date

date_freq$Var1 <-as.POSIXlt(date_freq$Var1+ as.difftime(1,units = "days")) 
date_freq2$Var1 <-as.POSIXlt(date_freq2$Var1+ as.difftime(2,units = "days")) 
date_freq3$Var1 <-as.POSIXlt(date_freq3$Var1+ as.difftime(3,units = "days")) 


a <- merge(train,date_freq,by="Var1",all.x = T,sort = F)
b <- merge(a,date_freq2,by="Var1",all.x = T,sort = F)
c <- merge(b,date_freq3,by="Var1",all.x = T,sort = F)

c
str(date_freq)
str(train)



