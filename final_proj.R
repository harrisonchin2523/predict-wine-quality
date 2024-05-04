setwd("/Users/David/Desktop/stsci4740/predict-wine-quality/")
library("boot")
# databind
#red_wine <- read.csv("./wine+quality/winequality-red.csv", header = TRUE, sep = ";")
#white_wine <- read.csv("./wine+quality/winequality-white.csv", header = TRUE, sep = ";")
red_wine <- read.csv("~/STSCI4740/final_proj/wine+quality/winequality-red.csv", header = TRUE, sep = ";")
white_wine <- read.csv("~/STSCI4740/final_proj/wine+quality/winequality-white.csv", header = TRUE, sep = ";")
str(red_wine)
wine_data <- rbind(red_wine, white_wine)

# baseline regression model
set.seed(1)  # for reproducibility
rw_idxs <- sample(seq_len(nrow(red_wine)), size = 0.8 * nrow(red_wine))
ww_idxs <- sample(seq_len(nrow(red_wine)), size = 0.8 * nrow(red_wine))

rw_train_data <- red_wine[rw_idxs, ]
rw_test_data <- red_wine[-rw_idxs, ]

ww_train_data <- white_wine[ww_idxs, ]
ww_test_data <- white_wine[-ww_idxs, ]

model_red <- lm(quality ~ ., data = rw_train_data)
model_white <- lm(quality ~ ., data = ww_train_data)

print(summary(model_white))
print(summary(model_red))
red_yhat = predict(model_red, newdata=rw_test_data)
red_err = mean((red_yhat - red_wine[-rw_idxs, "quality"])^2)
red_err

white_yhat = predict(model_white, newdata=ww_test_data)
white_err = mean((white_yhat - white_wine[-ww_idxs, "quality"])^2)
white_err

# run regression trees
install.packages("tree")
library("tree")

tree.red = tree(quality~., data=rw_train_data)
tree.white = tree(quality~. , data=ww_train_data)
summary(tree.red)
summary(tree.white)
plot(tree.red)
text(tree.red, pretty=0)
plot(tree.white)
text(tree.white, pretty=0)

red_yhat = predict(tree.red, newdata = rw_test_data)
white_yhat = predict(tree.white, newdata=ww_test_data)
red_err = mean((red_yhat - red_wine[-rw_idxs, "quality"])^2)
red_err
white_err = mean((white_yhat - white_wine[-ww_idxs, "quality"])^2)
white_err

cv.red = cv.tree(tree.red)
plot(cv.red$size, cv.red$dev, type = 'b')
# now try it with a pruned tree
prune.red = prune.tree(tree.red, best=11)
red_yhat = predict(prune.red, newdata = rw_test_data)
red_err = mean((red_yhat - red_wine[-rw_idxs, "quality"])^2)
red_err

summary(prune.red)
plot(prune.red)
text(prune.red, pretty=0)

cv.white = cv.tree(tree.white)
plot(cv.white$size, cv.white$dev, type = 'b')
prune.white = prune.tree(tree.white, best=10)
summary(prune.white)
plot(prune.white)
text(prune.white, pretty=0)


white_yhat = predict(prune.white, newdata=ww_test_data)
white_err = mean((white_yhat - white_wine[-ww_idxs, "quality"])^2)
white_err

# trying bagging and random forests
install.packages("randomForest")
library(randomForest)
bag.red = randomForest(quality ~., data=red_wine, subset = rw_idxs, mtry=10)
yhat.bag.red = predict(bag.red, newdata = rw_test_data)
mean((yhat.bag.red - rw_test_data[, "quality"])^2)
importance(bag.red)

bag.white = randomForest(quality ~., data=white_wine, subset = ww_idxs, mtry=10)
yhat.bag.white = predict(bag.white, newdata = ww_test_data)
mean((yhat.bag.white - ww_test_data[, "quality"])^2)
plot(bag.white)
text(bag.white, pretty=0)
importance(bag.white)

# install.packages("FNN")
# library(FNN)
# 
# knn_err = rep(0, 60)
# for(num_neighbors in c(1:60)) {
#   pred = knn.reg(rw_train_data[,c("fixed.acidity", "volatile.acidity", "citric.acid",  "residual.sugar", "chlorides", "free.sulfur.dioxide",
#                                   "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")],
#                  rw_test_data[,c("fixed.acidity", "volatile.acidity", "citric.acid",  "residual.sugar", "chlorides", "free.sulfur.dioxide",
#                                   "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")],
#                  rw_test_data[,"quality"],k=num_neighbors)
#   knn_err = c(knn_err, mean((pred$pred - red_wine[-rw_idxs,"quality"])^2))
# }
# knn_err
# which.min(knn_err)
# 
# cv
