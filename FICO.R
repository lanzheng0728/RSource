setwd("D:\\WORK\\风控征信\\FICO评分结果\\评分结果0510_A")
library(digest)
library(readr)
library(dplyr)
file = list.files(".\\") 
df <- NULL
for(i in 1:length(file)) 
{
  print(file[i])
  d <- read.csv(file[i],stringsAsFactors = F,colClasses=c("character"))
  df <- rbind(df,d)
  
}
md5 <- function(x){
  digest(x,algo = "md5",serialize = F )
}



df[which(nchar(df$身份证号)<18),"身份证号"] <- NA
df$brithday <- substr(df$身份证号,7,14)
df$sex <- as.integer(substr(df$身份证号,17,17))%%2

df$Phone <- sapply(df$电话号码,md5)
df$First3DigitPhone <- substr(df$电话号码,1,3)
df$Id <- sapply(df$身份证号,md5)
df$CardNumber <- sapply(df$银行卡,md5)
df$Application_dt <- substr(df$申请单添加日期,1,10)
df$booked <- ifelse(df$是否放款=="是","YES","NO")
df$PBOC <- "NO"
df$Event_Flag <- ifelse(df$booked=="YES",ifelse(df$好坏定义=="好","3",ifelse(df$好坏定义=="坏","4","5")),"7")
df$Acct_Filter <- ""

write.csv(df2[,c(8,12:22)],file = "..\\FICO_SAMPLE_20160510.csv")


setwd("C:\\Users\\Administrator\\Desktop\\FICO评分结果\\评分结果0510_B")
file = list.files(".\\") 
df1 <- NULL
for(i in 1:length(file)) 
{
  print(file[i])
  d <- read.csv(file[i],stringsAsFactors = F,colClasses=c("character"))
  df1 <- rbind(df1,d)
}


df2 <- left_join(df,df1[,c("订单编号","好坏定义")],by="订单编号")



########################################################
df2[which(nchar(df2$身份证号)<18),"身份证号"] <- NA
df2$brithday <- substr(df2$身份证号,7,14)
df2$sex <- as.integer(substr(df2$身份证号,17,17))%%2

df2$Phone <- sapply(df2$电话号码,md5)
df2$First3DigitPhone <- substr(df2$电话号码,1,3)
df2$Id <- sapply(df2$身份证号,md5)
df2$CardNumber <- sapply(df2$银行卡,md5)
df2$Application_dt <- substr(df2$申请单添加日期,1,10)
df2$booked <- ifelse(df2$是否放款=="是","YES","NO")
df2$PBOC <- "NO"
#df2$Event_Flag <- ifelse(df2$booked=="YES",ifelse(df2$好坏定义=="好","3",ifelse(df2$好坏定义=="坏","4","5")),"7")
df2$Acct_Filter <- ""

write.csv(df2[,c(12:20)],file = "..\\FICO_SAMPLE_20160510.csv")



########################################################
#20160607 added fraud flag
#flag1 具体定义为：第1个月未还款，后续一直都没有还款
#flag2 具体定义为：第2个月未还款，后续一直都没有还款（不管第1个月是否还款）
#flag3 具体定义为：第4个月未还款，后续一直都没有还款（不管第1，2,3个月是否还款）
#字段名
#手机号（md5加密）
#申请日期
#是否是首次还款未付
#是否是第二个月还款未付
#是否是第四个
########################################################
library(RMySQL)
library(chron)
library(dplyr)
if(!file.exists("./overdue.RDATA"))
{
  conn<-dbConnect(MySQL(),dbname="fortune_copy",username="root",password="testpass",host="192.163.2.240",port=3306)
  dbSendQuery(conn,'SET NAMES utf8')
  overdue<-dbGetQuery(conn,"SELECT * FROM pf_overduedetailed")
  save(overdue,file="./overdue.RDATA")
}else
{
  load("./overdue.RDATA")
}
# 检查发生过逾期的订单数量
length(unique(overdue$Ordernum))
nrow(filter(overdue,Payperidos==1,DisposeStatus==0))
nrow(filter(overdue,Payperidos==2,DisposeStatus==0))
nrow(filter(overdue,Payperidos==4,DisposeStatus==0))
nrow(filter(overdue,Payperidos==1))
nrow(filter(overdue,Payperidos==2))
nrow(filter(overdue,Payperidos==4))

flag <- filter(overdue,Payperidos==1,DisposeStatus==0)
flag$Flag1 <- "Y"
flag <- flag[,c("Ordernum","Flag1")]
df$Ordernum <- df$订单编号
dt <- left_join(df[,c("Ordernum","电话号码","申请单添加日期")],flag,by="Ordernum")
dt[which(is.na(dt$Flag1)),"Flag1"] <- "N"


flag <- filter(overdue,Payperidos==2,DisposeStatus==0)
flag$Flag2 <- "Y"
flag <- flag[,c("Ordernum","Flag2")]
dt <- left_join(dt,flag,by="Ordernum")
dt[which(is.na(dt$Flag2)),"Flag2"] <- "N"

flag <- filter(overdue,Payperidos==4,DisposeStatus==0)
flag$Flag3 <- "Y"
flag <- flag[,c("Ordernum","Flag3")]
dt <- left_join(dt,flag,by="Ordernum")
dt[which(is.na(dt$Flag3)),"Flag3"] <- "N"


dt$Phone <- sapply(dt$电话号码,md5)
dt$Application_dt <- substr(dt$申请单添加日期,1,10)
write.csv(dt[,c("Phone","Application_dt","Flag1","Flag2","Flag3")],file = "..\\FICO_Fraud_20160607.csv")

table(substr(dt$Application_dt,1,7))
table(dt$Flag1,substr(dt$Application_dt,1,7))
table(dt$Flag2,substr(dt$Application_dt,1,7))
table(dt$Flag3,substr(dt$Application_dt,1,7))



