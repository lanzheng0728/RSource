########创建sample######
pic = list.files("securitycode") 
for(i in 1:length(pic)) 
{
  rec("./securitycode/",pic[i])
} 

######进行预测########
#  library(randomForest)
#  library(readr)
unknown = list.files("unknown") 
set.seed(0)
numTrain <- 10000
numTrees <- 50
train <- read.csv("sample.csv")

table(is.na(train))
train[is.na(train),c(1)]


labels <- as.factor(train[,1])
train <- train[,-1]
rforrest <- randomForest(train, labels, ntree=numTrees)

for(i in 1:length(unknown)) 
{
  file <- paste("./unknown/",unknown[i],sep = "")
  char <- regonizePic(rforrest,file)

  file.rename(file,paste("./unknown/",char,".jpg",sep = ""))
  print(char)
} 

rm(list=ls())
################################
regonizePic <- function(rforrest,filename)
{
  #path <- "./unknown/"
  #picname <- "1减3等于几.jpg"
  print(filename)
  orgpic=readJPEG(filename)
  
  ##去背景
  pic_nbg <- erasebackgroud(orgpic,3)
  writeJPEG(pic_nbg, target="1_去背景.jpg", quality = 1)
  
  ##反错切
  pic_shear <- shear(pic_nbg,50,300,22)
  
  ##像素直方图
  pic_hist <- charhist(pic_shear)
  
  ##切分文字
  breaks <- findChar(pic_hist)
  if(nrow(breaks) > 7 | nrow(breaks) <6)
    print("失败")
  
  c <- readsample(rforrest,pic_shear,breaks)
  return(c)
}

rec <- function(path,picname)
{
  #path <- "./securitycode/"
  #picname <- "叁加伍等于几.jpg"
  print(picname)
  orgpic=readJPEG(paste(path,picname,sep=""))
  name <-strsplit(x=picname,split = "[.]")[[1]][1]
  label<- str_split(name,"")
  
  ##去背景
  pic_nbg <- erasebackgroud(orgpic,3)
  writeJPEG(pic_nbg, target="1_去背景.jpg", quality = 1)
  
  ##反错切
  pic_shear <- shear(pic_nbg,50,300,22)
  
  ##像素直方图
  pic_hist <- charhist(pic_shear)
  
  ##切分文字
  breaks <- findChar(pic_hist)
  #if(nrow(breaks) > 7 | nrow(breaks) <6)
  if(nrow(breaks) != length(label[[1]]))
  {
    print("失败")
  }
  else
  {
    #drawcutline(pic_shear,breaks,paste("./cut/",picname,sep=""))
    writesample(pic_shear,breaks,picname,"sample.csv")
  }
  
  
}
################# kmeans去背景 ####################
erasebackgroud <- function(orgpic,center)
{
  longImage<-melt(orgpic)
  rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
  
  newpic <- rgbImage
  newpic$X1 <- NULL
  newpic$X2 <- NULL
  #newpic$value.3 <- NULL
  km <- kmeans(x = newpic,centers = center )
  rgbImage$cluster <- km$cluster
  
  ##找到文字的类别
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
  ##生成数组画图
  charpic <- array(rep(1,50*300),dim = c(50,300,3))
  for(i in 1:nrow(fchar))
  {
    charpic[fchar$X1[i],fchar$X2[i],] <- c(fchar$value.1[i],fchar$value.2[i],fchar$value.3[i])
  }
  if(minmean>90)
    charpic <- erasebackgroud(charpic,3)
  return(charpic)
}

shear <- function(orgpic,H,W,theta)
{
  a <- tan(theta*pi/180)
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
        
      }
    }
  }
  return(warp)
}

charhist <- function(rwarp)
{
  longImage<-melt(rwarp)
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
  
  for(p in 1:50)
  {
    n <- rgbImage %>% filter(X1==p,value.3==0) %>% summarise(count=n()) 
    if(n$count>0)
    {
      for(q in 1:n$count)
      {
        rwarp[p,q,] <- c(1,0,0)
      }
    }
    
  }
  return(rwarp)
}

findChar <- function(pic_hist)
{
  breaks <- data.frame(start <- c(),length <- c(),breaks <- c())
  #for(m in 1:50)
  #{
  m <- 1
  before <- 1
  s <- 0
  l <- 0
  b <- 0
  for (n in 1:300 )
  {
    if(pic_hist[m,n,2]!=before)
    {
      if(pic_hist[m,n,2]==0)
      {
        s <- n
      }
      else if(pic_hist[m,n,2]==1)
      {
        breaks <- rbind(breaks,data.frame(s,l,b))
        l <- 0
        b <- 1
        
      }
      before <- pic_hist[m,n,2]
      
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
  #  if(nrow(breaks)<6)
  #    next
  #  else
  #    break
  #}
  ##############处理不合理切割################
  ##扔掉太短的字符
  tb <- NULL
  for(i in 1:nrow(breaks))
  {
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
 
  ###############################
  return(tb)
}

drawcutline <- function(wrap,breaks,filename)
{
  iwrap <- wrap
  c <- data.frame()
  for(l in 1:nrow(breaks) )
  {
    
    for(j in 1:50)
    {
      iwrap[j,breaks$s[l],] <- c(1,0,0)
      iwrap[j,breaks$s[l]+breaks$l[l],] <- c(1,0,0)
    }
    
  }
  writeJPEG(iwrap, target=filename, quality = 1)
}

writesample <- function(wrap,breaks,picname,filename)
{
  name <-strsplit(x=picname,split = "[.]")[[1]][1]
  label<- str_split(name,"")
  c <- data.frame()
  sample <- NULL
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
    if(!is.na(label[[1]][l]))
    {
      charbyte <- array(char[,,1],dim=c(1,625))
      c <- cbind(label[[1]][l],charbyte)
      
      sample <- rbind(sample,c)
      
      #################字符名称##################
      #print(paste("字符名称：",label[[1]][l]))
      dir <- paste("./sample/",as.character( label[[1]][l]),"/",sep="")
      if(!file.exists(dir))
        dir.create(dir)
      writeJPEG(char, target=paste(dir,label[[1]][l],"_",picname,"_",paste(sample(c(LETTERS,1:9),5),collapse = ""),".jpg",sep = ""), quality = 1)
      
    }
    else
    {
      print(paste("错误名称：",picname))
    }
    
  }
  #write.csv(sample,file ="sample.csv",quote = F,row.names = F,col.names = F)
  write_csv(as.data.frame(sample),path=filename,append = T,col_names = F)
}

readsample <- function(rf,wrap,breaks)
{
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
  return(words)
  #write.csv(sample,file ="sample.csv",quote = F,row.names = F,col.names = F)
  #write_csv(as.data.frame(sample),path=filename,append = T,col_names = F)
}