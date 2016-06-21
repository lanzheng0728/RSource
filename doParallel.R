# 并行计算euler14问题
# 自定义函数以返回原始数值和步数
func <- function(x) {
  n = 1
  .raw <- list(x)
  while (x > 1) {
    x <- ifelse(x%%2==0,x/2,3*x+1)
    .raw <- c(.raw,x)
    n = n + 1
  }
  #barplot(unlist(.raw))
  return(list(unlist(.raw),n))
}

a=c(1,2,3,4,5,6)
barplot(a)


library(foreach)
# 非并行计算方式，类似于sapply函数的功能
system.time(
  x <- foreach(x=1:10000,.combine='rbind') %do% func(x)
)


library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)
# 并行计算方式
system.time(
  x <- foreach(x=1:10000,.combine='rbind') %dopar% func(x)
)

stopCluster(cl)

