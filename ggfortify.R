install.packages("devtools")
library(ggplot2)
library(MASS)
A <- matrix(data = c(5,5,0,5,
                     5,0,3,4,
                     3,4,0,3,
                     0,0,5,3,
                     5,4,4,5,
                     5,4,5,5),nrow=6,ncol=4,byrow=TRUE)
B <- matrix(data=c(5,2,3,4),nrow=2)
svd(A)
svd(B)
I <- eigen(B)
dim(B) 
B %*% I$vectors
I$vectors * I$values

c <- 1:9
dim(c) <- c(3,3)
i <- eigen(c)
det(c)
rank(i$vectors*i$values)
rank(c*i$vectors)
a <- matrix(c(2,0,1,1),2,2)
i <- eigen(a)
v <- matrix(i$vectors[,2])
q <- i$vectors
a%*%v
v*i$values[2]

#for(i in 1:10) {a[i]<-(i-0.375)/(10+0.25)}
#Q*sigma*inverse(Q) 可以推出原来矩阵
#Q <- [v1,v2,v3]
#sigma  <-  diag([lambda1,lambda2,lambda3])
sigma <- diag(i$values)
q%*%sigma%*%solve(q)
b <- svd(a)
b
b$u%*% diag(b$d)%*%t(b$v)

A <- scan()
4 4 5 5 2 3 2 3.75
5 5 5 4 2 2 3 1
5 4 4 5 2 3 1 2
5 4 5 5 3 2 1 2
4 5 5 4 2 1 3 2
1 2 3 2 5 3.875 5 5
3 1 2 2 4 5 4 4
2 1 3 2 4 5 4 5
2 2 3 1 5 5 5 4
1 3 2 1 4 5 4 5

A <- matrix(A,nrow=10,byrow=TRUE)
svd <- svd(A)

Aex <- svd$u[,1:2]%*%diag(svd$d[1:2])%*%t(svd$v[,1:2])

Aex
