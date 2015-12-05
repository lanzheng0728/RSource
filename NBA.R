nba <- read.table("nba_en.txt", sep="\t", header=T, as.is=c(T,F,T,T,T,F))
nba <- na.omit(nba)
nba$date <- as.Date(nba$date)
nba$win[nba$diff < 0] <- -1 #主场赢
nba$win[nba$diff > 0] <- 1  #客场赢

#原分差有误，重新计算分差：客场－主场
nba$diff <- sapply(strsplit(nba$score, ":"), FUN=function(x){return(as.integer(x[1])-as.integer(x[2]))})

#设置日期限制，一边计算的数据能更好地预测
start.date <- "2013-11-23"
nba <- nba[nba$date >= start.date,]
print(start.date)
#计算主场胜率排行和客场胜率排行榜
win.lose.zhu <- table(nba$zhudui, nba$win)
win.rate.zhu <- sort(win.lose.zhu[,"-1"]/rowSums(win.lose.zhu), decreasing=T)  #主场胜率排行榜
print("主场胜率排行")
print(win.rate.zhu)
win.lose.ke <- table(nba$kedui, nba$win)
win.rate.ke <- sort(win.lose.ke[,"1"]/rowSums(win.lose.ke), decreasing=T) #客场胜率排行榜
print("客场胜率排行")
print(win.rate.ke)
#rm(win.lose)

#计算胜率排行榜
win.rate.all <- (win.lose.ke[,"1"]+win.lose.zhu[,"-1"])/(rowSums(win.lose.ke)+rowSums(win.lose.zhu))
win.rate.all <- sort(win.rate.all, decreasing=T)
print("胜率总排行")
print(win.rate.all)

#对一些球队，主客场对胜负影响很大
win.rate.diff <- win.rate.zhu[sort(names(win.rate.zhu))] - win.rate.ke[sort(names(win.rate.ke))]
win.rate.diff <- sort(win.rate.diff, decreasing=T)  #主客场胜率差排行榜
print("主客场胜率差排行")
print(win.rate.diff)

#一只球队的战绩
schedule.oneteam <- function(team){
  schedule.zhu <- nba[nba$zhudui==team, c("date", "kedui", "diff", "win")]
  schedule.zhu$diff <- -schedule.zhu$diff
  schedule.zhu$win <- -schedule.zhu$win
  names(schedule.zhu)[2] <- "rival"
  schedule.ke<- nba[nba$kedui==team, c("date", "zhudui", "diff", "win")]
  names(schedule.ke)[2] <- "rival"
  schedule <- rbind(schedule.zhu, schedule.ke)
  schedule$zhuke <- c(rep("主场", nrow(schedule.zhu)), rep("客场", nrow(schedule.ke)))
  return(schedule[order(schedule$date),])
}
schedule.oneteam("密尔沃基雄鹿")

#nba无弱旅，弱队能赢强队，看看弱队赢球队的强弱分布，使用饼图
#require("ggplot2")
win.distribution <- function(team){
  zhu.win.team <- nba$kedui[nba$zhudui==team & nba$win==-1]
  ke.win.team <- nba$zhudui[nba$kedui==team & nba$win==1]
  win.team <- c(zhu.win.team, ke.win.team)
  #zhu.ke <- c(rep("主场", length(zhu.win.team)), rep("客场", length(ke.win.team)))
  data <- win.rate.all[win.team]
  data <- cut(data, c(0, 0.3, 0.7, 1))
  #print(qplot(win.rate.all[win.team], colour = zhu.ke, position="dodge", geom="bar"))
  #pie <- ggplot(data, aes(x=factor(1), fill=factor(data))) + geom_bar() + coord_polar(theta="y")
  print(pie(table(data)/length(data), main=team))
}
#sapply(names(tail(win.rate.all, n=5)), win.distribution)

#计算某球队某日期前后的胜率对比
#n为场次
win.rate.qianhou <- function(team, date, n){
  schedule <- nba[nba$zhudui==team | nba$kedui==team,]
  schedule.qian <- tail(schedule[schedule$date<date,], n)
  win.rate.qian <- (sum(schedule.qian$zhudui==team & schedule.qian$diff<0)+sum(schedule.qian$kedui==team & schedule.qian$diff>0))/n
  schedule.hou <- head(schedule[schedule$date>=date,], n)
  win.rate.hou <- (sum(schedule.hou$zhudui==team & schedule.hou$diff<0)+sum(schedule.hou$kedui==team & schedule.hou$diff>0))/n 
  return(sprintf("%s 在%s前后%d场的胜率对比：%f:%f", team, date, n, win.rate.qian, win.rate.hou))
}
win.rate.qianhou("布鲁克林篮网", "2013-12-10", 5)
#win.rate.qianhou("休斯顿火箭", "2013-12-10", 5)

#极稳：一个强队不会允许连输三场，尤其第三场在自己的主场。如马刺，热火，火箭
#这个函数算连败记录，非常巧妙，想了很长时间
hot <- schedule.oneteam("迈阿密热火")
streak <- function(team, lose=T){
  schedule <- schedule.oneteam(team)
  lose.str <- paste(schedule$win, collapse="")
  lose.str <- gsub("-1", "0", lose.str)
  split <- if(lose) "1+" else "0+"
  type <- if(lose) "连败" else "连胜"
  lose.counts <- nchar(unlist(strsplit(lose.str, split)))
  lose.max <- max(lose.counts)
  max.num <- sum(lose.counts %in% lose.max)
  return(sprintf("%s %s记录为 %d 场，出现过 %d 次", team, type, lose.max, max.num))
}
streak("迈阿密热火", lose=F)
sapply(names(head(win.rate.all, n=15)), streak)
sapply(names(tail(win.rate.all, n=15)), FUN=function(x,lose=F)streak(x, lose))

#计算主客场输赢分差均值
diff.zhu.win <- sapply(names(win.rate.zhu), function(x)return(-mean(nba$diff[nba$zhudui==x & nba$diff<0])))
diff.ke.win <- sapply(names(win.rate.ke), function(x)return(mean(nba$diff[nba$kedui==x & nba$diff>0])))
diff.zhu.lose <- sapply(names(win.rate.zhu), function(x)return(-mean(nba$diff[nba$zhudui==x & nba$diff>0])))
diff.ke.lose <- sapply(names(win.rate.ke), function(x)return(mean(nba$diff[nba$kedui==x & nba$diff<0])))
diff.all <- cbind(diff.zhu.win, diff.zhu.lose, diff.ke.win, diff.ke.lose)
diff.all[names(win.rate.all),]

