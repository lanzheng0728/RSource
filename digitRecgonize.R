library(jpeg)
orgpic=readJPEG("./securitycode/securitycode1.jpg")
orgpic[1,1,] <- c(0,0,0)
writeJPEG(orgpic, target="securitycode.jpg", quality = 0.95)
dim(orgpic)

#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
#longImage<-melt(orgpic)
#rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
#with(rgbImage,plot(X2, -X1, col = rgb(rgbImage[,c("value.1","value.2","value.3")])))


orgpic=readJPEG("./securitycode/securitycode5.jpg")
a <- tan(21*pi/180)
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
        if(orgpic[i,j,1]<(90/255) & orgpic[i,j,2]<(120/255) & orgpic[i,j,3]>0)
        {
          #warp[i,x,] <-orgpic[i,j,] 
          warp[i,x,] <-c(0,0,0)
          
        }
        else
          warp[i,x,] <-c(1,1,1)
          
    }
  }
}
writeJPEG(warp, target="ddddd.jpg", quality = 1)

longImage<-melt(warp)
rgbImage<-reshape(longImage,timevar="X3",idvar=c("X1","X2"),direction="wide")
#with(rgbImage,plot(X2, -X1, col = rgb(rgbImage[,c("value.1","value.2","value.3")])))

for(p in 1:300)
{
    n <- rgbImage %>% filter(X2==p,value.3==0) %>% summarise(count=n()) 
    if(n$count>0)
    {
        for(q in 1:n$count)
        {
          warp[q,p,] <- c(1,0,0)
        }
    }
}
writeJPEG(warp, target="eeee.jpg", quality = 1)


m <- 1
for(m in 1:50)
{
  before <- 1
  breaks <- data.frame(start <- c(),length <- c(),breaks <- c())
  s <- 0
  l <- 0
  b <- 0
  for (n in 1:300 )
  {
    if(warp[m,n,2]!=before)
    {
      if(warp[m,n,2]==0)
      {
        s <- n
      }
      else if(warp[m,n,2]==1)
      {
        breaks <- rbind(breaks,data.frame(s,l,b))
        l <- 0
        b <- 1
        
      }
      before <- warp[m,n,2]
        
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
}
