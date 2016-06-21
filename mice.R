#dat <- read.csv(url("http://goo.gl/19NKXV"), header=TRUE, sep=",")
#write.csv(dat,file = "19NKXV.csv")
setwd("C:\\Users\\Administrator\\Desktop\\R")
dat <- read.csv(url("19NKXV.csv"), header=TRUE, sep=",")
sapply(dat, function(x) sum(is.na(x)))
original <- dat

set.seed(10)
dat[sample(1:nrow(dat), 20), "Cholesterol"] <- NA
dat[sample(1:nrow(dat), 20), "Smoking"] <- NA
dat[sample(1:nrow(dat), 20), "Education"] <- NA
dat[sample(1:nrow(dat), 5), "Age"] <- NA
dat[sample(1:nrow(dat), 5), "BMI"] <- NA


library(dplyr) 
dat <- dat %>%
  mutate(Smoking = as.factor(Smoking)) %>% 
  mutate(Education = as.factor(Education)) %>% 
  mutate(Cholesterol = as.numeric(Cholesterol))

str(dat)


library(mice)
init = mice(dat, maxit=0) 
meth = init$method
predM = init$predictorMatrix


###################################
install.packages("tidyr")
library(tidyr)
library(dplyr)

messy <- data.frame(
  name = c("Wilbur", "Petunia", "Gregory"),
  a = c(67, 80, 64),
  b = c(56, 90, 50)
)
messy
messy %>%   gather(drug,heartrate,a:b)


set.seed(10)
messy <- data.frame(
  id = 1:4,
  trt = sample(rep(c('control', 'treatment'), each = 2)),
  work.T1 = runif(4),
  home.T1 = runif(4),
  work.T2 = runif(4),
  home.T2 = runif(4)
)
messy
tidier <- messy %>% gather(key,time,-id,-trt)
tidier %>% head(8)


tidy <- tidier %>%
  separate(key, into = c("location", "time"), sep = "\\.") 
tidy %>% head(8)