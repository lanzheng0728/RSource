a <- matrix(data = runif(n = 10,min = 0,1),nrow = 2)

plot.tree <- function(x1,y1,x2,y2,n,xlim=c(-1,1), ylim=c(0,2), col="blue", add=FALSE){
  if(!add)
    plot(0,0,xlim=xlim, ylim=ylim, type="n",xlab="", ylab="",asp=1)
  tree <- function(x1,y1,x2,y2,n){
    flag <- 0;
    theta <- pi/6;
    if (x2<x1) flag <- 1;
    if (n>1){
      tree(x1,y1,(2*x1+x2)/3,(2*y1+y2)/3,n-1);
      tree((2*x1+x2)/3,(2*y1+y2)/3,(2*x2+x1)/3,(2*y2+y1)/3,n-1);
      tree((2*x2+x1)/3,(2*y2+y1)/3,x2,y2,n-1);
      tree((2*x1+x2)/3,(2*y1+y2)/3,(2*x1+x2)/3+sin(pi/2-atan((y2-y1)/(x2-x1))-theta+flag*pi)*sqrt(((y2-y1)^2+(x2-x1)^2)/3),(2*y1+y2)/3+cos(pi/2-atan((y2-y1)/(x2-x1))-theta+flag*pi)*sqrt(((y2-y1)^2+(x2-x1)^2)/3),n-1);
      tree((2*x2+x1)/3,(2*y2+y1)/3,(2*x2+x1)/3+sin(pi/2-atan((y2-y1)/(x2-x1))+theta+flag*pi)*sqrt(((y2-y1)^2+(x2-x1)^2)/3),(2*y2+y1)/3+cos(pi/2-atan((y2-y1)/(x2-x1))+theta+flag*pi)*sqrt(((y2-y1)^2+(x2-x1)^2)/3),n-1);
    } else {
      x <- c(x1,x2);
      y <- c(y1,y2);
      xx <- c((2*x1+x2)/3,(2*x1+x2)/3+sin(pi/2-atan((y2-y1)/(x2-x1))-theta+flag*pi)*sqrt(((y2-y1)^2+(x2-x1)^2)/3));
      yy <- c((2*y1+y2)/3,(2*y1+y2)/3+cos(pi/2-atan((y2-y1)/(x2-x1))-theta+flag*pi)*sqrt(((y2-y1)^2+(x2-x1)^2)/3));
      xxx <- c((2*x2+x1)/3,(2*x2+x1)/3+sin(pi/2-atan((y2-y1)/(x2-x1))+theta+flag*pi)*sqrt(((y2-y1)^2+(x2-x1)^2)/3));
      yyy <- c((2*y2+y1)/3,(2*y2+y1)/3+cos(pi/2-atan((y2-y1)/(x2-x1))+theta+flag*pi)*sqrt(((y2-y1)^2+(x2-x1)^2)/3));
      lines(x, y, type="l",col=col)
      lines(xx, yy, type="l",col=col)
      lines(xxx, yyy, type="l",col=col)
    }
  }
  tree(x1,y1,x2,y2,n)
}

## example
for(i in 1:5){
  plot.tree(0,0,0,1.5,i,col=i)
  Sys.sleep(1)
}

##################################
plot.tri <- function(n = 1000, col ="blue", ani=FALSE, cex=1.2){
  p <- runif(n);
  X <- rbind(rep(0, n), rep(0, n))
  B <- cbind(c(0,0),c(0.25,0.433),c(0.5,0))
  if(ani) plot(0,0,xlim=c(0,1),ylim=c(0,0.85),type="n",xlab="",ylab="")
  for(i in 2:n){
    pp <- p[i];
    ind <- rank(c(c(1/3,2/3,1), pp), ties.method="min")[4]
    X[,i] <- 0.5*X[,i-1] + B[,ind]
    if(ani) points(X[1,i], X[2,i],pch = ".", cex = 1, col = col)
  }
  if(!ani) plot(X[1,], X[2,],pch = ".", cex = cex, col = col, xlab="", ylab="")
}
plot.tri(10, ani=TRUE)

##################################
plot.koch <- function(k,col="blue"){
  plot(0,0,xlim=c(0,1), ylim=c(-sqrt(3)/6,sqrt(3)/2), asp = 1,type="n",xlab="", ylab="")
  
  plotkoch <- function(x1,y1,x2,y2,n){
    if (n > 1){
      plotkoch(x1,y1,(2*x1+x2)/3,(2*y1+y2)/3,n-1);
      plotkoch((2*x1+x2)/3,(2*y1+y2)/3,(x1+x2)/2-(y1-y2)*sqrt(3)/6,(y1+y2)/2-(x2-x1) *sqrt(3)/6,n-1);
      plotkoch((x1+x2)/2-(y1-y2)*sqrt(3)/6,(y1+y2)/2-(x2-x1)*sqrt(3)/6,(2*x2+x1)/3,(2 *y2+y1)/3,n-1);
      plotkoch((2*x2+x1)/3,(2*y2+y1)/3,x2,y2,n-1)
    }    else {
      x=c(x1,(2*x1+x2)/3,(x1+x2)/2-(y1-y2)*sqrt(3)/6,(2*x2+x1)/3,x2);
      y=c(y1,(2*y1+y2)/3,(y1+y2)/2-(x2-x1)*sqrt(3)/6,(2*y2+y1)/3,y2);
      lines(x,y,type="l",col=col)
    }
  }
  plotkoch(0,0,1,0,k)
  plotkoch(0.5,sqrt(3)/2,0,0,k)
  plotkoch(1,0,0.5,sqrt(3)/2,k)
}

## example
for(i in 1:5){
  plot.koch(i,col=i)
  Sys.sleep(1)
}

plot.koch(1,col=1)

k <- 1
col <- "blue"
x1 <- 0
y1 <- 0
x2 <- 1
y2 <- 0

x
y
k##############################################
library(insol)

plot.dragon <-  function(k,col="red"){
  plot(0,0,xlim=c(0,1), ylim=c(-1,1), asp = 1,type="n",xlab="", ylab="")
  normalvector <- function(x1,y1,x2,y2){
    p <- c(1/2,1/2,1)
    #o原点在坐标系的坐标
    o <- c(x1,y1,1)
    x <- c(x2,y2,1)
    #平移矩阵
    cmtrx <- matrix(c(1,0,0 ,0,1,0 ,o),nrow=3)
    cx <- solve(cmtrx)%*%x
    #旋转矩阵
    theta <- -pi/2
    rmtrx <- matrix(c(cos(theta),sin(theta),0 ,-sin(theta),cos(theta),0 ,0,0,1 ),nrow=3)
    cy <- solve(rmtrx)%*%cx
    #变换矩阵
    mmtrix <- matrix(c(cx,cy,0,0,1),nrow = 3)
    cp <- mmtrix%*%p
    #平移回去
    return(cmtrx%*%c(cp[1:2],1))
    
  }
  plotdra <- function(x1,y1,x2,y2,n){
    p <- normalvector(x1,y1,x2,y2)
    if(n>1){
      plotdra(x1,y1,p[1],p[2],n-1)
      plotdra(x2,y2,p[1],p[2],n-1)
    } else {
      x=c(x1,p[1],x2)
      y=c(y1,p[2],y2)
      lines(x,y,type="l",col=col)
    }
  }
  plotdra(0,0,1,0,k)
}

## example
for(i in 1:12){
  plot.dragon(i)
  Sys.sleep(1)
}


##############Matrix test#######################
#  b * c = I * a
# => b^-1 * b *c = b^-1 * I * a
# => c = b^-1 * a
a <- c(-2,2)
b <- matrix(c(-1/2,3/2,-1,-1),nrow = 2)


#solve求逆
solve(b)%*%as.matrix(a,nrow(2))
solve(b)%*%a
a%*%solve(t(b))
b%*%solve(b)


o <- c(1/2,1/2)
cor <- matrix(c(1,0,0,1),nrow = 2)
solve(cor)
solve(cor)%*%o


homoo <- c(1/2,1/2,1)
homocoor <- matrix(c(1,0,0, 0,1,0, 2,2,1),nrow = 3)
solve(homocoor)
solve(homocoor)%*%homoo

############原点重合情况############


#计算Y轴 从X轴逆时针旋转90
theta <- pi/2
#x轴在坐标系的坐标
xcoor <- c(1/2,1/2)

#旋转矩阵
ymtrx <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2)

#生成Y坐标
ycoor <- solve(ymtrx)%*%xcoor
#生成转换矩阵
coor <- cbind(xcoor,ycoor)

solve(coor)%*%xcoor

#生成坐标系对应点坐标
x <- c(1,0)
coor%*% x

x <- c(1/2,1/2)
coor%*% x

###################################################
####################测试平移######################
#x轴在坐标系的坐标
xcoor <- c(1/2,1/2,1)

#o原点在坐标系的坐标
ocoor <- c(1,0,1)

#旋转矩阵
ymtrx <- matrix(c(1,0,0 ,0,1,0 ,ocoor),nrow=3)


ycoor <-  solve(ymtrx)%*%xcoor

#################旋转90#####################
theta <- -pi/2
ymtrx <- matrix(c(cos(theta),sin(theta),0 ,-sin(theta),cos(theta),0 ,0,0,1 ),nrow=3)
ycoor <- solve(ymtrx)%*%ycoor

################平移回来#############

ymtrx <- matrix(c(1,0,0 ,0,1,0 ,ocoor),nrow=3)
ycoor <- ymtrx%*%ycoor
#####################################################
####################################################

##############原点不重合情况####################
#计算Y轴 从X轴逆时针旋转90
theta <- -pi/2
#x轴在坐标系的坐标
xcoor <- c(1/2,1/2,1)

#o原点在坐标系的坐标
ocoor <- c(1,0,1)

#旋转矩阵
ymtrx <- matrix(c(cos(theta),sin(theta),0 ,-sin(theta),cos(theta),0 ,ocoor),nrow=3)

#生成Y坐标
ycoor <- solve(ymtrx)%*%xcoor
################原点不重合一不步做很麻烦，分步来做###########################


normalvector <- function(x1,y1,x2,y2){
  p <- c(1/2,1/2,1)
  
  #o原点在坐标系的坐标
  o <- c(x1,y1,1)
  x <- c(x2,y2,1)
  
  #平移矩阵
  cmtrx <- matrix(c(1,0,0 ,0,1,0 ,o),nrow=3)
  cx <- solve(cmtrx)%*%x
  
  #旋转矩阵
  theta <- -pi/2
  rmtrx <- matrix(c(cos(theta),sin(theta),0 ,-sin(theta),cos(theta),0 ,0,0,1 ),nrow=3)
  cy <- solve(rmtrx)%*%cx
  
  #变换矩阵
  mmtrix <- matrix(c(cx,cy,0,0,1),nrow = 3)
  cp <- mmtrix%*%p
  
  #平移回去
  return(cmtrx%*%c(cp[1:2],1))
  
}







