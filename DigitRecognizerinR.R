#install.packages("readr")
library(readr)
train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")
head(train[1:10])

##   label pixel0 pixel1 pixel2 pixel3 pixel4 pixel5 pixel6 pixel7 pixel8
## 1     1      0      0      0      0      0      0      0      0      0
## 2     0      0      0      0      0      0      0      0      0      0
## 3     1      0      0      0      0      0      0      0      0      0
## 4     4      0      0      0      0      0      0      0      0      0
## 5     0      0      0      0      0      0      0      0      0      0
## 6     0      0      0      0      0      0      0      0      0      0

# Create a 28*28 matrix with pixel color values
m = matrix(unlist(train[10,-1]),nrow = 28,byrow = T)
# Plot that matrix
image(m,col=grey.colors(255))

rotate <- function(x) t(apply(x, 2, rev)) # reverses (rotates the matrix)

# Plot a bunch of images
par(mfrow=c(2,3))
lapply(1:6, 
       function(x) image(
         rotate(matrix(unlist(train[x,-1]),nrow = 28,byrow = T)),
         col=grey.colors(255),
         xlab=train[x,1]
       )
)

par(mfrow=c(1,1)) # set plot options back to default

#install.packages("h2o")
library(h2o)

## Loading required package: methods
## Loading required package: statmod
## 
## ----------------------------------------------------------------------
## 
## Your next step is to start H2O and get a connection object (named
## 'localH2O', for example):
##     > localH2O = h2o.init()
## 
## For H2O package documentation, ask for help:
##     > ??h2o
## 
## After starting H2O, you can use the Web UI at http://localhost:54321
## For more information visit http://docs.h2o.ai
## 
## ----------------------------------------------------------------------

## start a local h2o cluster
localH2O = h2o.init(max_mem_size = '6g', # use 6GB of RAM of *GB available
                    nthreads = -1) # use all CPUs (8 on my personal computer :3)



## 
## H2O is not running yet, starting it now...
## 
## Note:  In case of errors look at the following log files:
##     /tmp/RtmpqLbvlg/h2o__started_from_r.out
##     /tmp/RtmpqLbvlg/h2o__started_from_r.err
## 
## 
## ..Successfully connected to http://127.0.0.1:54321/ 
## 
## R is connected to the H2O cluster: 
##     H2O cluster uptime:         2 seconds 536 milliseconds 
##     H2O cluster version:        3.2.0.3 
##     H2O cluster name:           H2O_started_from_R_root_ojv699 
##     H2O cluster total nodes:    1 
##     H2O cluster total memory:   5.33 GB 
##     H2O cluster total cores:    16 
##     H2O cluster allowed cores:  16 
##     H2O cluster healthy:        TRUE


## MNIST data as H2O
train[,1] = as.factor(train[,1]) # convert digit labels to factor for classification
train_h2o = as.h2o(train)

test_h2o = as.h2o(test)

## set timer
s <- proc.time()

## train model
model =
  h2o.deeplearning(x = 2:785,  # column numbers for predictors
                   y = 1,   # column number for label
                   training_frame = train_h2o, # data in H2O format
                   activation = "RectifierWithDropout", # algorithm
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(100,100), # two layers of 100 nodes
                   momentum_stable = 0.99,
                   nesterov_accelerated_gradient = T, # use it for speed
                   epochs = 15) # no. of epochs

## print confusion matrix
h2o.confusionMatrix(model)

s - proc.time()

## classify test set
h2o_y_test <- h2o.predict(model, test_h2o)

## convert H2O format into data frame and  save as csv
df_y_test = as.data.frame(h2o_y_test)
df_y_test = data.frame(ImageId = seq(1,length(df_y_test$predict)), Label = df_y_test$predict)
write.csv(df_y_test, file = "submission-r-h2o.csv", row.names=F)

## shut down virutal H2O cluster
h2o.shutdown(prompt = F)