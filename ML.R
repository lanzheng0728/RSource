library(lattice)
library(ggplot2)
library(caret)
data(faithful)
set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,
                               p=0.5,
                               list=FALSE)
trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]
head(trainFaith) 
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue")
lml <- lm(eruptions~waiting,data=trainFaith)
class(lml)
summary(lml)
lines(x = trainFaith$waiting,y = lml$fitted,lwd=3)
coef(lml)[1]+coef(lml)[2]*80


library(ISLR)
data(Wage)
Wage <- subset(Wage,select=-c(logwage))
summary(Wage)

Y=ts(rnorm(100,mean=0,sd=1))
plot(Y,family="simhei",main="白噪声",type="b",col="red")
abline(h=0)

Y = ts(rnorm(100, mean=0, sd=1));
for (i in 2:length(Y)) {
  Y[i] = Y[i] + Y[i-1];
}
plot(Y, family="simhei", main="随机游走", type="b", col="red");
abline(h=0)

nrow(trees)
plot(Volume~Girth,data=trees,pch=16,col="red")
model <- lm(Volume~Girth,data=trees)
summary(model)
abline(model,lty=2)

par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

plot(sqrt(Volume)~Girth,data=trees,pch=16,col="red")
model2 <- lm(sqrt(Volume)~Girth,data=trees)
summary(model2)


par(mfrow=c(2,2))
plot(model2)
par(mfrow=c(1,1))

data.pre <- data.frame(predict(model2,interval = "prediction"))
class(data.pre)
summary(data.pre)
str(data.pre)

lines(data.pre$lwr~trees$Girth,col="blue",lty=2)
lines(data.pre$upr~trees$Girth,col="blue",lty=2)


newiris <- iris
pairs(iris)
newiris$Species <- NULL
(kc <- kmeans(newiris,3))
table(iris$Species,kc$cluster)
plot(newiris[c("Sepal.Length","Sepal.Width")],col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex=2)


library(RODBC)
con <- odbcConnect("ADDB")
df_TM <- as.data.frame(sqlQuery(con,
          "SELECT vTargetMail.CustomerKey, 
          vTargetMail.MaritalStatus, vTargetMail.Gender, 
          vTargetMail.TotalChildren, vTargetMail.NumberChildrenAtHome, 
          vTargetMail.EnglishEducation as Education, 
          vTargetMail.EnglishOccupation as Occupation, 
          vTargetMail.HouseOwnerFlag, vTargetMail.NumberCarsOwned, 
          vTargetMail.Region, vTargetMail.Age, vTargetMail.BikeBuyer
          FROM AdventureWorksDW2014.dbo.vTargetMail vTargetMail"),
          stringAsFactors=FALSE )

close(con)
summary(df_TM)

colMeans(df_TM["NumberCarsOwned"])
aggregate(CustomerKey~Gender+Education,
          data=df_TM,FUN=length)

library(ggplot2)
ggplot(df_TM,aes(Region,fill=Education))+geom_bar()


ggplot(df_TM,aes(Occupation))+ geom_histogram(color="White")+facet_grid(MaritalStatus ~.)


###############
#Naive Bayes  #
###############
install.packages("e1071")
library(e1071)


#build the naive Bayes model
TMNB <- naiveBayes(df_TM[,2:11],df_TM[,12])

#Apriori probabilities for the target varible "BikeBuyer"
TMNB$apriori

TMNB$tables

df_PR <- as.data.frame(predict(TMNB,df_TM,type="raw"))

df_TM_PR <- cbind(df_TM,df_PR)

table(predict(TMNB,df_TM[,2:11]),df_TM[,12],dnn = list("predict","actual"))
###################appendix################
# 数据集来自Tom Mitchell's book "Machine Learning".
#定义数据矩阵matrix，matrix(vector, nrow=r, ncol=c, 
#byrow=logical_value,
#dimnames=list(char_vector_rownames, char_vector_colnames))
#nrow表示行数
#ncol表示列数
#byrow表示矩阵组织方式，按行或者按列
#dimnames表示行标识，列标识
data <- matrix(c("sunny","hot","high","weak","no",
                 "sunny","hot","high","strong","no",
                 "overcast","hot","high","weak","yes",
                 "rain","mild","high","weak","yes",
                 "rain","cool","normal","weak","yes",
                 "rain","cool","normal","strong","no",
                 "overcast","cool","normal","strong","yes",
                 "sunny","mild","high","weak","no",
                 "sunny","cool","normal","weak","yes",
                 "rain","mild","normal","weak","yes",
                 "sunny","mild","normal","strong","yes",
                 "overcast","mild","high","strong","yes",
                 "overcast","hot","normal","weak","yes",
                 "rain","mild","high","strong","no"), byrow = TRUE,
               dimnames = list(day = c(),
              condition = c("outlook","temperature",
              "humidity","wind","playtennis")), nrow=14, ncol=5);
data <- data.frame(data)
#统计yes，no出现的概率                 
prior.yes = sum(data[,5] == "yes") / length(data[,5]);
prior.no = sum(data[,5] == "no") / length(data[,5]);

prnb <- naiveBayes(data[,1:4],data[,5])
predict(prnb,data.frame(outlook="rain",temperature="hot",
                        humidity="high",wind="strong"))
predict(prnb,data.frame(outlook="rain",temperature="hot",
                        humidity="high",wind="strong"),type="raw")

table(predict(prnb,data[,-5]),data[,5],dnn = list("predict","actual"))
#################
#Decision Tree  # 
#################

install.packages("party")
library(party)
TMDT <- ctree(BikeBuyer~NumberCarsOwned + Region,
              data=df_TM)
plot(TMDT,type="simple")


#build-in package version of decision trees
library(rpart)
TMDT <- rpart(BikeBuyer~NumberCarsOwned + Region,method="class",
              data=df_TM)
summary(TMDT)
plot(TMDT,uniform=TRUE)
text(TMDT,use.n=TRUE,all=TRUE,cex=1)


####################
# Regression Tree  #
####################
con <- odbcConnect("ADDB")
df_TM <- as.data.frame(sqlQuery(con,
                                "SELECT vTargetMail.CustomerKey, 
          vTargetMail.MaritalStatus, vTargetMail.Gender, 
          vTargetMail.TotalChildren, vTargetMail.NumberChildrenAtHome, 
          vTargetMail.EnglishEducation as Education, 
          vTargetMail.EnglishOccupation as Occupation, 
          vTargetMail.HouseOwnerFlag, vTargetMail.NumberCarsOwned, 
          vTargetMail.Region, vTargetMail.Age, 
          vTargetMail.YearlyIncome,vTargetMail.CommuteDistance, 
          vTargetMail.BikeBuyer
          FROM AdventureWorksDW2014.dbo.vTargetMail vTargetMail"),
                       stringAsFactors=FALSE )

close(con)
TMRT <- rpart(YearlyIncome~Age+NumberCarsOwned+Region+Age,
              method="anova",data=df_TM)
#summary(TMRT)
plot(TMRT,uniform=TRUE,main="Regression Tree")
text(TMRT,use.n=TRUE,all=TRUE,cex=1)

######################
#Linear Regession   #
#####################
TMLinR <- lm(YearlyIncome~Age+NumberCarsOwned+Region+Age,data=df_TM)
summary(TMLinR)
plot(TMLinR)


#################
#Support Vector Machines
#################

library(e1071)

ds <- df_TM[1:200,]
x <- subset(ds,select=c(NumberCarsOwned,Region,CommuteDistance,Age))
y <- ds$BikeBuyer

TMSVM <- svm(BikeBuyer~NumberCarsOwned+Region+CommuteDistance+Age,
             data=ds)

summary(TMSVM)


pred <- predict(TMSVM,x)
table(pred,y)


#Exaple of plot of a svm model based on the iris data

data(iris)
m2 <- svm(Species~.,data=iris)
plot(m2,iris,Petal.Width~Petal.Length,
     slice=list(Sepal.Width=3,Sepal.Length=4))

########################
#neural network and logistic regression
######################
TMLogR <- glm(BikeBuyer ~ YearlyIncome + Age + NumberCarsOwned,
              data=df_TM,family=binomial())

summary(TMLogR)

confint(TMLogR)

df_LogR <- as.data.frame(predict(TMLogR,type="response"))
df_TM_PR <- cbind(df_TM,df_LogR)

install.packages("neuralnet")
library(neuralnet)
ds <- df_TM[1:500,]
TMNN <- neuralnet(BikeBuyer~YearlyIncome + Age + NumberCarsOwned,
                  ds,hidden=1)
print(TMNN)

#######################
#  Time Series
#######################

con <- odbcConnect("ADDB")
df_TS <- as.data.frame(sqlQuery(con,
    "Select TimeIndex,SUM(Amount) AS Amount
    From dbo.vTimeSeries
    Group by TimeIndex
    Order by TimeIndex"),stringAsFactors=FALSE)
close(con)
ts(data=df_TS[,2],
   start=c(2010,12),
   frequency=12)
tsdata <- ts(data=df_TS[,2],
             start=c(2010,12),
             frequency=12)

plot(tsdata)
arima(tsdata,order = c(0,0,1))
arima(tsdata,order = c(1,0,1))
arima(tsdata,order = c(1,1,1))
arima(tsdata,order = c(2,1,1))

TS <- arima(tsdata,order = c(1,0,1))
predict(TS,6)

tspredict <- predict(TS,6,NULL,FALSE)

c(as.vector(df_TS[,2]),as.vector(tspredict))

tsdataforcase <- ts(data=c(as.vector(df_TS[,2]),
                           as.vector(tspredict)),
                    start=c(2010,12),frequency=12)

plot(tsdataforcase)

########################3
# R
#####################
x <- seq(0,2800,400)
y <- seq(0,2400,400)
z <- scan()
1180 1320 1450 1420 1400 1300 700 900
1230 1390 1500 1500 1400 900 1100 1060
1270 1500 1200 1100 1350 1450 1200 1150
1370 1500 1200 1100 1550 1600 1550 1380
1460 1500 1550 1600 1550 1600 1600 1600
1450 1480 1500 1550 1510 1430 1300 1200
1430 1450 1470 1320 1280 1200 1080 940
Z <- matrix(z,nrow = 8)
contour(x,y,Z,levels=seq(min(z),max(z),by=80))
persp(x,y,Z,theta = 30,phi = 45,expand=.5,col = "green",shade = 0.5)
#####################################

x <- y <- seq(-2*pi,2*pi,pi/15)
f <- function(x,y) sin(x)*sin(y)
z <- outer(x,y,f)
contour(x,y,z,col = "blue")
persp(x,y,z,theta=30,phi=15,expand=.5,col="lightblue")


#####################################
# 2 varients (X,Y)
#####################################

ore <- data.frame(
  x=c(67,54,72,64,39,22,58,43,46,34),
  y=c(24,15,23,19,16,11,20,16,17,13)
  )
ore.m <- data.frame(x=mean(ore$x),y=mean(ore$y))
ore.s <- cov(ore)
ore.r <- cor(ore)
cor.test(ore$x,ore$y)

#################
#   multivarient
#################
x <- scan()
65 45 27.6
70 45 30.7
70 48 31.8
69 46 32.6
66 50 31.0
67 46 31.3
68 47 37.0
72 43 33.6
66 47 33.1
68 48 34.2
#X <- matrix(x,nrow=10,byrow=TRUE)
X <- data.frame(X1=x[seq(from = 1,to=28,by = 3)],
X2=X[seq(from = 2,to=29,by = 3)],
X3=X[seq(from = 3,to=30,by = 3)])
apgply(X,2,mean)
