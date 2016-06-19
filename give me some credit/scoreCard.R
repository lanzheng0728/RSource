## 一、数据准备
setwd("C:\\github-lanzheng0728\\RSource\\give me some credit")
library("readr")
library("VIM")
library(DMwR)

traindata <- read_csv("cs-training.csv")

## 二、数据处理
matrixplot(traindata)

names(traindata)
length(which(is.na(traindata$MonthlyIncome)))
length(which(is.na(traindata$NumberOfDependents)))


traindata<-knnImputation(traindata,k=10,meth ="weighAvg")  

na_idx_monthlyIncome <- which(is.na(traindata$MonthlyIncome))
median_monthlyIncome <- median(traindata$MonthlyIncome,na.rm = T)
traindata$MonthlyIncome[na_idx_monthlyIncome] <- median_monthlyIncome


na_idx_numOfDependent <- which(is.na(traindata$NumberOfDependents))
median_numOfDependents <- median(traindata$NumberOfDependents,na.rm = T)
traindata$NumberOfDependents[na_idx_numOfDependent] <- median_numOfDependents

boxplot(traindata$MonthlyIncome)
boxplot(traindata$MonthlyIncome,outline = F)

unique(traindata$age)

boxplot(traindata$age)
boxplot(traindata$age,outline = F)


traindata<-traindata[-which(traindata$age==0),]  

boxplot(traindata$`NumberOfTime30-59DaysPastDueNotWorse`,traindata$`NumberOfTime60-89DaysPastDueNotWorse`,traindata$NumberOfTimes90DaysLate)


traindata<-traindata[-which(traindata$`NumberOfTime30-59DaysPastDueNotWorse`==96),]  
traindata<-traindata[-which(traindata$`NumberOfTime30-59DaysPastDueNotWorse`==98),]  

## 三、变量分析

hist(traindata$age,breaks = 40)
hist(traindata$MonthlyIncome,breaks =100)



library(corrplot)
M <- cor(traindata)
corrplot(M,method = "circle" )


## 四、切分数据集
train_data <- traindata[,-1]
colnames(train_data) <- c("y","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10")
str(train_data)
table(train_data$y)

#采用SMOTE算法，用R对稀有事件进行超级采样。
#我们利用caret包中的createDataPartition（数据分割功能）函数将数据随机分成相同的两份。
library(caret)

set.seed(1234)   
splitIndex<-createDataPartition(train_data$y,time=1,p=0.5,list=FALSE)   
train<-train_data[splitIndex,]  
test<-train_data[-splitIndex,]  



## 五、Logistic回归
fit<-glm(y~.,train,family = "binomial") 
summary(fit)



fit2<-glm(y~x2+x3+x5+x7+x8+x9+x10,train,family = "binomial")  
summary(fit2)  

library(pROC)

pre <- predict(fit2,test)  

modelroc <- roc(test$y,pre)  
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),  
     grid.col=c("green", "red"), max.auc.polygon=TRUE,  
     auc.polygon.col="skyblue", print.thres=TRUE)  


## 六、WOE转换
#### 1、进行分箱
#### age变量(x2)：
cutx2= c(-Inf,30,35,40,45,50,55,60,65,75,Inf)  
plot(cut(train$x2,cutx2)) 


#### NumberOfTime30-59DaysPastDueNotWorse变量(x3)：  
cutx3 = c(-Inf,0,1,3,5,Inf)  
plot(cut(train$x3,cutx3))  


#### MonthlyIncome变量(x5)：  
cutx5 = c(-Inf,1000,2000,3000,4000,5000,6000,7500,9500,12000,Inf)  
plot(cut(train$x5,cutx5))  


#### NumberOfTimes90DaysLate变量(x7)：  
cutx7 = c(-Inf,0,1,3,5,10,Inf)  
plot(cut(train$x7,cutx7))  


#### NumberRealEstateLoansOrLines变量(x8)：  
cutx8= c(-Inf,0,1,2,3,5,Inf)  
plot(cut(train$x8,cutx8))  


#### NumberOfTime60-89DaysPastDueNotWorse变量(x9)：  
cutx9 = c(-Inf,0,1,3,5,Inf)  
plot(cut(train$x9,cutx9))  


#### NumberOfDependents变量(x10)：  
cutx10 = c(-Inf,0,1,2,3,5,Inf)  
plot(cut(train$x10,cutx10))  


#### 2、计算WOE值
totalgood = as.numeric(table(train$y))[1]  
totalbad = as.numeric(table(train$y))[2]  
getWOE <- function(a,p,q)  
{  
  Good <- as.numeric(table(train$y[a > p & a <= q]))[1]  
  Bad <- as.numeric(table(train$y[a > p & a <= q]))[2]  
  WOE <- log((Bad/totalbad)/(Good/totalgood),base = exp(1))  
  return(WOE)  
}


Agelessthan30.WOE=getWOE(train$x2,-Inf,30)  
Age30to35.WOE=getWOE(train$x2,30,35)  
Age35to40.WOE=getWOE(train$x2,35,40)  
Age40to45.WOE=getWOE(train$x2,40,45)  
Age45to50.WOE=getWOE(train$x2,45,50)  
Age50to55.WOE=getWOE(train$x2,50,55)  
Age55to60.WOE=getWOE(train$x2,55,60)  
Age60to65.WOE=getWOE(train$x2,60,65)  
Age65to75.WOE=getWOE(train$x2,65,75)  
Agemorethan.WOE=getWOE(train$x2,75,Inf)  
age.WOE=c(Agelessthan30.WOE,Age30to35.WOE,Age35to40.WOE,Age40to45.WOE,Age45to50.WOE,  
          Age50to55.WOE,Age55to60.WOE,Age60to65.WOE,Age65to75.WOE,Agemorethan.WOE)  
age.WOE  





tmp.age <- 0  
for(i in 1:nrow(train)) {  
  if(train$x2[i] <= 30)  
    tmp.age[i] <- Agelessthan30.WOE  
  else if(train$x2[i] <= 35)  
    tmp.age[i] <- Age30to35.WOE  
  else if(train$x2[i] <= 40)  
    tmp.age[i] <- Age35to40.WOE  
  else if(train$x2[i] <= 45)  
    tmp.age[i] <- Age40to45.WOE  
  else if(train$x2[i] <= 50)  
    tmp.age[i] <- Age45to50.WOE  
  else if(train$x2[i] <= 55)  
    tmp.age[i] <- Age50to55.WOE  
  else if(train$x2[i] <= 60)  
    tmp.age[i] <- Age55to60.WOE  
  else if(train$x2[i] <= 65)  
    tmp.age[i] <- Age60to65.WOE  
  else if(train$x2[i] <= 75)  
    tmp.age[i] <- Age65to75.WOE  
  else  
    tmp.age[i] <- Agemorethan.WOE  
}  

table(tmp.age)  
