
user<- read.csv("user.txt",sep='\t', header=FALSE)
names(user) <- c("mon","vol","id")

fortune<- read.csv("fortune_new.txt",sep='\t', header=FALSE)
names(fortune) <- c("mon","vol","id")

t <- data.frame(mon=c(1,2,3,4,5,6,7,8),
	vol=c(10,20,30,40,50,60,70,80),
	id=c(1,1,1,1,1,1,1,1))

table(fortune$mon)

isStepUp <- function(u,cont,DEBUG=FALSE){
  #	u <- user[user$id==2,]
  #	u  <- t
  n <- nrow(u)
  if(n<2)
    return(FALSE)
  
  vol_last <- u[1,2]
  tag <- 0
  tmp_tag <- 0
  for(i in 2:n)
  {
    #print(u[i,])
    vol <- u[i,2]
    
    step <- (vol-vol_last)/vol_last
    if(DEBUG) print(paste("step",i,":",step,sep=""))
    if(step>0.05)
    {
      tag <- tag+1
    }else {
      if(tag>tmp_tag) tmp_tag <- tag
      tag <- 0
      #	print(paste("tag:",tag,sep=""))
      #if(tag>0)	{
      #	break
      #}else next
    }
    vol_last <- vol
  }
  if(tag>tmp_tag) tmp_tag <- tag
  if(DEBUG) print(paste("tag:",tmp_tag,sep=""))
  if(tmp_tag>=cont)
    return(TRUE)#print("TURE") #TRUE
  else return(FALSE)#print("FALSE") #FALSE
}

for(p in 1:993)
{
  #print(paste("#######[userid=",p,"]#########",sep=""))
  #u <- user[user$id==209,]
  u <- user[user$id==p,]
  if(isStepUp(u,5))
    print(p)
}


for(p in 1:120)
{
  #print(paste("#######[userid=",p,"]#########",sep=""))
  #f <- fortune[fortune$id==13,]
  f <- fortune[fortune$id==p,]
  if(isStepUp(f,6))
    print(p)
}

