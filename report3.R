library(RMySQL)
library(ggplot2)
library(plyr)
conn<-dbConnect(MySQL(),dbname="fortune",username="root",password="testpass",host="192.168.1.222",port=3306)
tabs<-dbGetQuery(conn,"show tables")
dbSendQuery(conn,'SET NAMES utf8')

employee<-dbGetQuery(conn,"select a.id,b.store_id,a.employee_entrytime,a.employee_leavetime,a.employee_state,a.employee_type from PF_employee_primary a,pf_employee_position b where a.id=b.employee_id")
head(employee)
nrow(employee)
order<-dbGetQuery(conn,"select * from PF_finorder ")
head(employee)

colnames(employee)

emp <- employee[employee$employee_type=="0" & employee$employee_state!=2 & employee$employee_leavetime>employee$employee_entrytime,c("id","store_id","employee_entrytime","employee_leavetime")]
nrow(filter(emp,as.Date(employee_leavetime)>Sys.Date() | employee_leavetime=="0000-00-00 00:00:00"))
nrow(emp[which(as.Date(emp$employee_leavetime)>Sys.Date() | emp$employee_leavetime=="0000-00-00 00:00:00"),])

emp[which(as.Date(emp$employee_leavetime)>Sys.Date() | emp$employee_leavetime=="0000-00-00 00:00:00"),"employee_leavetime"] <- as.character(Sys.time())

emp$employee_entrytime <- as.Date(paste(substring(emp$employee_entrytime,0,7),"-01",sep = ""))
emp$employee_leavetime <- as.Date(emp$employee_leavetime)

View(emp)
View(employee[employee$id==46,])

#t <- data.frame()
#for (i in 1:nrow(emp))
#{
#  e <- emp[i,]
#  m <- format(seq(from=e$employee_entrytime,to=e$employee_leavetime,by='1 months'),"%Y%m")
#  id <- rep(e$id,length(m))
#  store_id <- rep(e$store_id,length(m))
  
#  t <- rbind(t,data.frame(id,store_id,m))
#  print(i)
#}

t1 <- adply(emp,1,function(e){
  m <- format(seq(from=e$employee_entrytime,to=e$employee_leavetime,by='1 months'),"%Y%m")
  id <- rep(e$id,length(m))
  store_id <- rep(e$store_id,length(m))
  data.frame(id,store_id,m,stringsAsFactors = F)
})

zz <- t1%>% group_by(store_id,m) %>% summarise(count=n()) %>% arrange(store_id,m)

#zaizhi <- t %>% group_by(store_id,m) %>% summarise(count=n())
#write.csv(zaizhi,file = "zaizhi.csv")

ggplot(arrange(zz,store_id,m),aes(x=m,y=count,colour=store_id,group=store_id))+geom_line()

View(filter(zz,store_id==3339))
