#################图片下载################
library(RCurl)
url <- "http://img.woyaogexing.com/touxiang/nv/20140212/54a77a633bb0119a!200x200.jpg"
filepath <- "download.jpg"
bin <- getBinaryURL(url)

con <- file(filepath, open = "wb")
writeBin(bin, con)
close(con)


library(jpeg)
library(stringr)
orgpic=readJPEG("./unknown/---.jpg")
orgpic[1,1,] <- c(0,0,0)
#writeJPEG(orgpic, target="securitycode.jpg", quality = 0.95)
dim(orgpic)

#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
#orgpic=readJPEG("./securitycode/6加9等于几 (2).jpg")
#orgpic=readJPEG("./colortest.jpg")
longImage<-melt(orgpic)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")

newpic <- filter(rgbImage,value.1<0.6)
with(newpic,plot(X2, -X1, col = rgb(rgbImage[,c("value.1","value.2","value.3")])))
with(rgbImage,plot(X2, -X1, col = rgb(rgbImage[,c("value.1","value.2","value.3")])))


with(rgbImage,plot(value.1,value.2))
with(rgbImage,hist(value.1))
with(rgbImage,hist(value.2))
with(rgbImage,hist(value.3))

with(filter(rgbImage,cluster==1),hist(value.1))
with(filter(rgbImage,cluster==2),hist(value.1))
with(filter(rgbImage,cluster==3),hist(value.1))

#############开始##################
rm(list=ls())

################# 读取图片 ####################
sample <- NULL
orgpic <- NULL
#picname <- "0x1等于几.jpg"
#picname <- "8减1等于几.jpg"
#picname <- "2x1等于几.jpg"
#picname <- "伍乘壹等于几.jpg"
#picname <- "贰乘以柒等于几.jpg"
picname <- "./unknown/---.jpg"
orgpic=readJPEG(picname)
name <-strsplit(x=picname,split = "[.]")[[1]][1]
label<- str_split(name,"")


################# kmeans去背景 ####################
longImage<-melt(orgpic)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
newpic <- rgbImage
newpic$X1 <- NULL
newpic$X2 <- NULL
#newpic$value.3 <- NULL
km <- kmeans(x = newpic,centers = 3 )
rgbImage$cluster <- km$cluster


f1 <- filter(rgbImage,cluster==1)
f2 <- filter(rgbImage,cluster==2)
f3 <- filter(rgbImage,cluster==3)

with(filter(rgbImage,cluster==1),plot(X2, -X1,col =  rgb(rgbImage[,c("value.1","value.2","value.3")])))
with(rgbImage,plot(X2, -X1,col =  rgb(rgbImage[,c("value.1","value.2","value.3")])))

#################找到文字的类别##################
minmean <- 300
fchar <- NULL
for(i in 1:3)
{
  f <- filter(rgbImage,cluster==i)
  m <- mean(f$X2)
  if(m<minmean)
  {
    minmean <- m
    fchar <- f
  }
}
#################################
#hist(f2$X2)
#summary(f1$X2)
#summary(f2$X2)
#summary(f3$X2)

#with(f3,hist(value.1))
#with(f3,hist(value.2))
#with(f3,hist(value.3))
################################

################生成数组画图##################
charpic <- array(rep(1,50*300),dim = c(50,300,3))
for(i in 1:nrow(fchar))
{
  charpic[fchar$X1[i],fchar$X2[i],] <- c(fchar$value.1[i],fchar$value.2[i],fchar$value.3[i])
}
writeJPEG(charpic, target="1_去背景.jpg", quality = 1)

if(minmean > 90){
#####################二次kmeans#######################
longImage<-melt(charpic)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
newpic <- rgbImage
newpic$X1 <- NULL
newpic$X2 <- NULL
#newpic$value.3 <- NULL
km <- kmeans(x = newpic,centers = 3 )
rgbImage$cluster <- km$cluster


f1 <- filter(rgbImage,cluster==1)
f2 <- filter(rgbImage,cluster==2)
f3 <- filter(rgbImage,cluster==3)
summary(f1$X2)
summary(f2$X2)
summary(f3$X2)

with(filter(rgbImage,cluster==3),plot(X2, -X1,col =  rgb(rgbImage[,c("value.1","value.2","value.3")])))
#with(rgbImage,plot(X2, -X1,col =  rgb(rgbImage[,c("value.1","value.2","value.3")])))

#################二次 找到文字的类别##################
minmean <- 300
fchar <- NULL
for(i in 1:3)
{
  f <- filter(rgbImage,cluster==i)
  m <- mean(f$X2)
  if(m<minmean)
  {
    minmean <- m
    fchar <- f
  }
}
################二次 生成数组画图##################
charpic <- array(rep(1,50*300),dim = c(50,300,3))
for(i in 1:nrow(fchar))
{
  charpic[fchar$X1[i],fchar$X2[i],] <- c(fchar$value.1[i],fchar$value.2[i],fchar$value.3[i])
}
writeJPEG(charpic, target="1_去背景.jpg", quality = 1)

}

###################反错切#########################
orgpic <- charpic
a <- tan(21.5*pi/180)
H <- 50
W <- 300
warp <- array(rep(1,H*W),dim = c(50,300,3))
for (i in 1:H)
{
  for (j in 1:W)
  {
    x <- j+i*a
    if( x<300)
    {
        x <- as.integer(x)
        if(orgpic[i,j,1]!=1 | orgpic[i,j,2]!=1 | orgpic[i,j,3]!=1)
        {
          warp[i,x,] <-c(0,0,0)
        }
        #if(orgpic[i,j,1]<(90/255) & orgpic[i,j,2]<(120/255) & orgpic[i,j,3]>0)
        #{
          #warp[i,x,] <-orgpic[i,j,] 
        #  warp[i,x,] <-c(0,0,0)
        #}
        #else
        #  warp[i,x,] <-c(1,1,1)
    }
  }
}
writeJPEG(warp, target="2_反错切.jpg", quality = 1)


#with(rgbImage,plot(X2, -X1, col = rgb(rgbImage[,c("value.1","value.2","value.3")])))

######################画像素直方图#########################
rwarp <- warp
longImage<-melt(warp)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
for(p in 1:300)
{
    n <- rgbImage %>% filter(X2==p,value.3==0) %>% summarise(count=n()) 
    if(n$count>0)
    {
        for(q in 1:n$count)
        {
          rwarp[q,p,] <- c(1,0,0)
        }
    }
}

havechar <- 0
for(p in 1:50)
{
  n <- rgbImage %>% filter(X1==p,value.3==0) %>% summarise(count=n()) 
  if(n$count>0)
  {
    havechar <- havechar+1
    for(q in 1:n$count)
    {
      rwarp[p,q,] <- c(1,0,0)
    }
  }
}
head <- as.integer((50-19)/2)-1
writeJPEG(rwarp, target="3_直方图.jpg", quality = 1)



####################切分文字变量##############################
breaks <- data.frame(start <- c(),length <- c(),breaks <- c())
#for(m in 1:50)
#{
  m <- 1
  before <- 1
  s <- 0
  l <- 0
  b <- 0
  merg <- F
  df.merg <- NULL
  for (n in 1:300 )
  {
    if(rwarp[m,n,2]!=before)
    {
      #遇到红色柱状图，一个新字符开始
      if(rwarp[m,n,2]==0)
      {
        #n <- 67
        #规则1
        #nred <- length(rwarp[rwarp[,n,1]==1 & rwarp[,n,2]==0,n,2]) 
        #if(nred>10)
        #  merg <- T
        
        s <- n
      }
      #红色柱状图结束，间隔开始
      else if(rwarp[m,n,2]==1)
      {
        #n <- 65
        #规则1
        #nred <- length(rwarp[rwarp[,n,1]==1 & rwarp[,n,2]==0,n,2]) 
        #if(nred>10)
        #  merg <- T
        #if(merg)
        #{
          df.merg <- data.frame(s,l,b)
          breaks <- rbind(breaks,data.frame(s,l,b))
          l <- 0
          b <- 1
        #}
      }
      before <- rwarp[m,n,2]
    }
    else if(before==0)
    {
      l <- l+1
    }
    else if(before==1)
    {
      b <- b+1
    }
  }
##############调整不合理切割################
  ##扔掉太短的字符
  tb <- NULL
  for(i in 1:nrow(breaks))
  {
    print(i)
    if(breaks$l[i]<2)
    {
      next
    }
    else
    {
      tb <- rbind(tb,breaks[i,])
    }
  }
  breaks <- tb
  ##避免文字中间被切断
  tb <- NULL
  for(i in 1:nrow(breaks))
  {
    print(i)
    #if(i!=1 & breaks$l[i]<6)#0x1等于几.jpg
    #if(i!=1 & breaks$l[i]<3)#8减1等于几.jpg
    if(i!=1 & breaks$l[i]<3)
    {
      tb$l[nrow(tb)] <- breaks$l[i-1]+breaks$l[i]+breaks$b[i]+1
    }
    else
    {
      tb <- rbind(tb,breaks[i,])
    }
  }
  breaks <- tb
  
  ##避免文字没切断
  tb <- NULL
  for(i in 1:nrow(breaks))
  {
    if(breaks$l[i]>35)
    {
      l <- breaks[i,]
      l$l <- breaks[i,]$l/2-l$b
      tb <- rbind(tb,l)
      
      nl <- breaks[i,]
      nl$s <-  l$s+l$l+nl$b+1
      nl$l <- breaks[i,]$l/2-nl$b
      tb <- rbind(tb,nl)
    }
    else
    {
      tb <- rbind(tb,breaks[i,])
    }
  }
  breaks <- tb
#  if(nrow(breaks)<6)
#    next
#  else
#    break
#}
  
if(nrow(breaks) != length(label[[1]]))
  print("失败")
#if(nrow(breaks) > 7 | nrow(breaks) <6)
#{
#  print("失败")
#}
  
########################画切线###########################
iwrap <- warp
c <- data.frame()
for(l in 1:nrow(breaks) )
{
  
  for(j in 1:50)
  {
    iwrap[j,breaks$s[l],] <- c(1,0,0)
    iwrap[j,breaks$s[l]+breaks$l[l],] <- c(1,0,0)
  }

}
writeJPEG(iwrap, target="4_画切线.jpg", quality = 1)

######################切成小图##########################
for(l in 1:nrow(breaks) )
{
 
  char <- array(rep(1,25*25),dim = c(25,25,3))
  if(breaks$l[l] < 6) next
  
  for(p in 1:25)
  {
    for(q in 1:25)
    {
      if(q>breaks$l[l]) next
      char[p,q,] <- warp[p+head,q+breaks$s[l],]
    }
  }
  
  charbyte <- array(char[,,1],dim=c(1,625))
  c <- cbind(label[[1]][l],charbyte)
  sample <- rbind(sample,c)
  dir <- paste("./sample/",as.character( label[[1]][l]),"/",sep="")
  if(!file.exists(dir))
    dir.create(dir)
  writeJPEG(char, target=paste(dir,label[[1]][l],"_",paste(sample(c(LETTERS,1:9),5),collapse = ""),".jpg",sep = ""), quality = 1)
}


##################################################
#                学习和预测
#################################################

#  library(randomForest)
#  library(readr)

set.seed(0)
numTrain <- 10000
numTrees <- 25
train <- read.csv("sample.csv")

train <- train[-126,]
labels <- as.factor(train[,1])
train <- train[,-1]
rf <- randomForest(train, labels, ntree=numTrees)

##################################
m = matrix(unlist(train[4,-1]),nrow = 25,byrow = T)
# Plot that matrix
image(m,col=grey.colors(255))
rotate <- function(x) t(apply(x, 2, rev)) # reverses (rotates the matrix)
##################################
# Plot a bunch of images
par(mfrow=c(2,3))
lapply(1:6, 
       function(x) image(
         rotate(matrix(unlist(train[x,-1]),nrow = 25,byrow = T)),
         col=grey.colors(255),
         xlab=train[x,1]
       )
)
par(mfrow=c(1,1)) # set plot options back to default
###############################################
#rows <- sample(1:nrow(train), numTrain)
#labels <- as.factor(train[rows,1])
#train <- train[rows,-1]
###############################################


#################进行预测#####################
wrap <- pic_shear
words <- NULL
for(l in 1:nrow(breaks) )
{
  char <- array(rep(1,25*25),dim = c(25,25,3))
  #if(breaks$l[l] < 6) next
  
  for(p in 1:25)
  {
    for(q in 1:25)
    {
      if(q>breaks$l[l]) next
      char[p,q,] <- wrap[p+5,q+breaks$s[l],]
    }
  }
  charbyte <- array(char[,,1],dim=c(1,625))
  c <- predict(rf,newdata = charbyte,type="response")
  words <- paste(words,as.character(c),sep="")
}
