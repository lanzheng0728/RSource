library(recommenderlab)

m <- matrix(sample(c(as.numeric(0:5), NA), 30,replace = TRUE, prob = c(rep(0.5/6,6), 0.5)), 
            ncol = 6, dimnames = list(user = paste("u", 1:5, sep = ""), 
                                      item = paste("i", 1:6, sep = "")))
m.real <- as(m, "realRatingMatrix")
m.real
str(m.real)
rating <- m.real@data
n.real <- normalize(m.real)
n.real
image(m.real, main = "Raw rating")
image(n.real, main = "Normalized rating")
data(MovieLense)

class(MovieLense)
m <- as(MovieLense,"data.frame")
View(m)
image(MovieLense)
ratings.movie <- data.frame(ratings = getRatings(MovieLense))
summary(ratings.movie$ratings)
library(ggplot2)
ggplot(ratings.movie, aes(x = ratings)) + geom_histogram(fill = "beige", color = "black",  binwidth = 1, alpha = 0.7) + xlab("rating") + ylab("count")

ratings.movie1 <- data.frame(ratings = getRatings(normalize(MovieLense, method = "Z-score")))
summary(ratings.movie1$ratings)
ggplot(ratings.movie1, aes(x = ratings)) + geom_histogram(fill = "beige", color = "black", 
                                                          alpha = 0.7) + xlab("rating") + ylab("count")
# 用户的电影点评数
movie.count <- data.frame(count = rowCounts(MovieLense))
ggplot(movie.count, aes(x = count)) + geom_histogram(fill = "beige", color = "black",
                                                     alpha = 0.7) + xlab("counts of users") + ylab("counts of movies rated")

View(movie.count )

# 电影的平均评分
rating.mean <- data.frame(rating = colMeans(MovieLense))
ggplot(rating.mean, aes(x = rating)) + geom_histogram(fill = "beige", color = "black", 
                                                      alpha = 0.7) + xlab("rating") + ylab("counts of movies ")

# 先看可以使用的方法
recommenderRegistry$get_entries(dataType = "realRatingMatrix")



m.recomm <- Recommender(MovieLense[1:940], method = "IBCF")
m.recomm

ml.predict <- predict(m.recomm, MovieLense[941:943], n = 3)
str(ml.predict)
as(ml.predict, "list")#预测结果




scheme <- evaluationScheme(MovieLense, method = "split", train = 0.9, k = 1, given = 10, goodRating = 4)
algorithms <- list(popular = list(name = "POPULAR", param = list(normalize = "Z-score")), 
                   ubcf = list(name = "UBCF", param = list(normalize = "Z-score", method = "Cosine", nn = 25, minRating = 3)), 
                   ibcf = list(name = "IBCF", param = list(normalize = "Z-score")))
results <- evaluate(scheme, algorithms, n = c(1, 3, 5, 10, 15, 20))
plot(results, annotate = 1:3, legend = "topleft") #ROC
plot(results, "prec/rec", annotate = 3)#precision-recall



# 按照评价方案建立推荐模型
model.popular <- Recommender(getData(scheme, "train"), method = "POPULAR")
model.ibcf <- Recommender(getData(scheme, "train"), method = "IBCF")
model.ubcf <- Recommender(getData(scheme, "train"), method = "UBCF")
# 对推荐模型进行预测
predict.popular <- predict(model.popular, getData(scheme, "known"), type = "ratings")
predict.ibcf <- predict(model.ibcf, getData(scheme, "known"), type = "ratings")
predict.ubcf <- predict(model.ubcf, getData(scheme, "known"), type = "ratings")

predict.err <- rbind(calcPredictionError(predict.popular, getData(scheme, "unknown")), 
                     calcPredictionError(predict.ubcf, getData(scheme, "unknown")),
                     calcPredictionError(predict.ibcf, getData(scheme, "unknown")))
rownames(predict.err) <- c("POPULAR", "UBCF", "IBCF")



####################################################################
ml100k <- read.table("u.data", header = F, stringsAsFactors = T)
ml100k <- ml100k[, -4]
table(ml100k[,3])
prop.table(table(ml100k[,3]))
library(reshape)
ml <- cast(ml100k, V1 ~ V2, value = "V3")
summary(ml)
str(ml)
ml[1:3, 1:6]
class(ml) <- "data.frame"
ml.useritem <- as.matrix(ml)
ml.ratingMatrix <- as(ml.useritem, "realRatingMatrix")  ##转换为realRatingMatrix
ml.ratingMatrix

ml.df <- as(ml.ratingMatrix,"data.frame")
View(ml.df)
colnames(ml.ratingMatrix)
colnames(ml.ratingMatrix) <- paste("M", 1:1683, sep = "")
dim(ml.ratingMatrix)
ml.recommModel <- Recommender(ml.ratingMatrix[1:800], method = "IBCF")
ml.recommModel
ml.predict1 <- predict(ml.recommModel, ml.ratingMatrix[801:803], n = 5)
ml.predict1
as( ml.predict1,"list")  ##显示三个用户的Top3推荐列表
##用户对item的评分预测

ml.predict2 <- predict(ml.recommModel, ml.ratingMatrix[801:803], type = "ratings")

ml.predict2

## 查看三个用于对M1-6的预测评分
## 注意：实际的预测评分还要在此基础上加上用户的平均评分

as(ml.predict2, "matrix")[1:3, 1:6]
model.eval <- evaluationScheme(ml.ratingMatrix[1:943], method = "split",train = 0.9, given = 15, goodRating = 5)

model.eval
model.random <- Recommender(getData(model.eval, "train"), method = "RANDOM")
model.ubcf <- Recommender(getData(model.eval, "train"), method = "UBCF")
model.ibcf <- Recommender(getData(model.eval, "train"), method = "IBCF")

predict.random <- predict(model.random, getData(model.eval, "known"), type = "ratings")
predict.ubcf <- predict(model.ubcf, getData(model.eval, "known"), type = "ratings")
predict.ibcf <- predict(model.ibcf, getData(model.eval, "known"), type = "ratings")

error <- rbind(
  
  + calcPredictionError(predict.random, getData(model.eval, "unknown")),
  
  + calcPredictionError(predict.ubcf, getData(model.eval, "unknown")),
  
  + calcPredictionError(predict.ibcf, getData(model.eval, "unknown")))


###################################################################
data(Jester5k)
Jester5k
j5k.df <-as(Jester5k,"data.frame") 
View(j5k.df)
r <- sample(Jester5k,1000)
r
rowCounts(r[1,])
summary(rowMeans(r))
(getRatings(r))

hist(getRatings(r), breaks=100)
