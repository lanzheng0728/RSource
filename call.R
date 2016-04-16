library(readr)
call<- read.csv("calling.csv",stringsAsFactors = FALSE,header = TRUE)

head(call)
str(call)

call$calltime <- as.POSIXlt(call$calltime)
call$answertime <- as.POSIXlt(call$answertime)
call$duration <- as.difftime(call$duration)
call$quetime <- as.difftime(call$quetime)
call$hagtime <- as.POSIXlt(call$hagtime)


table(call$status)
table(call$number)
View(call)

call$calltime.date <- format(call$calltime,format = "%Y-%m-%d")
call$calltime.time <- format(call$calltime,format = "%H:%M:%S")


