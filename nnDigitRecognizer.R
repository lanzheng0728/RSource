# You can write R code here and then click "Run" to run it on our platform

library(readr)
library(caret)

# The competition datafiles are in the directory ../input
# Read competition data files:
train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")

# Write to the log:
cat(sprintf("Training set has %d rows and %d columns\n", nrow(train), ncol(train)))
cat(sprintf("Test set has %d rows and %d columns\n", nrow(test), ncol(test)))

# Generate output files with write_csv(), plot() or ggplot()
# Any files you write to the current directory get shown as outputs


# Creates a simple k-nearest neighbour  benchmark
library(FNN)
# Create training and testing set

trainIndex <- createDataPartition(train$label, p = .9,
                                  list = FALSE,
                                  times = 1)

Train <- train[ trainIndex,]
Test  <- train[-trainIndex,]

cat("Get the count ","\n")
print(table(Train$label))

cat("\n")
cat("Model using K-nn ","\n")
pc <- proc.time()
model.fnn <- FNN::knn(Train[,-1],Test[,-1],Train$label,k=10,algorithm = "cover_tree")
proc.time() - pc

cat("\n")
cat("Performance of the Knn ","\n")
print(confusionMatrix(model.fnn,Test$label))