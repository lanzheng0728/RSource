require(rvest)
require(stringr)

url <- "http://www.wettfreunde.net/em-2016-quoten/"

# Darmstadt
# 1/2.25 
# 1/2.3

mv <- html(url)

tab <- mv %>% html_table
tab <- tab[[2]]

tab <- tab[3:nrow(tab),]
colnames(tab) <- c("Team", "intervetten", "bet365", "tipico", "comeon", "betsafe")

tab$intervetten<-as.numeric(str_replace_all(tab$intervetten, ",", "."))
tab$tipico<- as.numeric(str_replace_all(tab$tipico, ",", "."))
tab$bet365<- as.numeric(str_replace_all(tab$bet365, ",", "."))
tab$betsafe<- as.numeric(str_replace_all(tab$betsafe, ",", "."))
tab$comeon<- as.numeric(str_replace_all(tab$comeon, ",", "."))
View(tab)


tab$intervetten<- 1/(tab$intervetten+1)
tab$tipico<- 1/(tab$tipico+1)
tab$bet365<- 1/(tab$bet365+1)
tab$betsafe<- 1/(tab$betsafe+1)
tab$comeon<- 1/(tab$comeon+1)
View(tab)

######### 
sum(tab$intervetten, na.rm=T)
sum(tab$tipico, na.rm=T)
sum(tab$bet365, na.rm=T)
sum(tab$betsafe, na.rm=T)
sum(tab$comeon, na.rm=T)

##########################
########## normalizse odds!
tab$intervetten<- tab$intervetten / sum(tab$intervetten, na.rm=T) * 2
tab$tipico<- tab$tipico / sum(tab$tipico, na.rm=T) * 2
tab$bet365<- tab$bet365 / sum(tab$bet365, na.rm=T) * 2
tab$betsafe<- tab$betsafe / sum(tab$betsafe, na.rm=T) * 2
tab$comeon<- tab$comeon / sum(tab$comeon, na.rm=T) * 2



