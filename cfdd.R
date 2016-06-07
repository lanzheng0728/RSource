library(RMySQL)
library(chron)
library(lubridate)
detach( "package:lubridate")
library(dplyr)
library("highcharter")
conn<-dbConnect(MySQL(),dbname="source_fortune",username="root",password="p@z0l!w808q.ip?6zmk",host="112.74.39.145",port=3306)
dbSendQuery(conn,'SET NAMES utf8')
cf<-dbGetQuery(conn,"SELECT a.`id`,a.Fin_number,a.Fin_time,a.Fin_Amounts ,b.`Pro_Cycle`,a.Fin_state FROM cf_finorder a,cf_pro_details b
WHERE a.`Fin_Proid`=b.`id` and  a.Fin_state IN ('4','5','6')")
cf$dates <- as.chron(cf$Fin_time)
range(cf$dates)

table(years(cf$dates))
table(cf$Fin_state)
table(cut(dates(cf$dates),"months"))

dts <- dates("07/01/78") + trunc(50 * rnorm(30))
seq.dates("01/01/92", "12/31/02", by = "months")
seq.dates("02/29/92", "12/31/02", by = "years")
tt <- times(c("12:13:14", "15:46:17"))
trunc(tt, "minutes")
seq.dates(dates("07/01/78"),by="months",length. = 12)

duedate <- function(dates,durition){
  seq <- chron::seq.dates(dates,by="months",length.=durition+1)
  return(seq[durition+1])
}
cf$duedate <- as.chron(mapply(duedate,cf$dates,cf$Pro_Cycle))
cf$duedate <- as.chron(cf$duedate)
cf$duemonth <- format(cf$duedate,"%Y-%m")
dueamount <- cf %>% group_by(duemonth) %>% summarise(amount=round(sum(Fin_Amounts)/10000,2))



hc <- highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "订单到期") %>% 
  hc_xAxis(categories = dueamount$duemonth) %>% 
  hc_add_series(data = dueamount$amount,
                name = "金额(万元)")

hc


