setwd("/Users/David/Desktop/stsci4740/predict-wine-quality/")

# databind
#red_wine <- read.csv("./wine+quality/winequality-red.csv", header = TRUE, sep = ";")
#white_wine <- read.csv("./wine+quality/winequality-white.csv", header = TRUE, sep = ";")
red_wine <- read.csv("~/STSCI4740/final_proj/wine+quality/winequality-red.csv", header = TRUE, sep = ";")
white_wine <- read.csv("~/STSCI4740/final_proj/wine+quality/winequality-white.csv", header = TRUE, sep = ";")

wine_data <- rbind(red_wine, white_wine)

# baseline regression model
set.seed(1)  # for reproducibility
indexes <- sample(seq_len(nrow(wine_data)), size = 0.8 * nrow(wine_data))

rw_train_data <- red_wine[indexes, ]
rw_test_data <- red_wine[-indexes, ]

ww_train_data <- white_wine[indexes, ]
ww_test_data <- white_wine[-indexes, ]

model_red <- lm(quality ~ ., data = rw_train_data)
model_white <- lm(quality ~ ., data = ww_train_data)

print(summary(model_white))
print(summary(model_red))
red_yhat = predict(model_red, newdata=rw_test_data)
red_err = mean((red_yhat - red_wine[-indexes, "quality"])^2)
red_err

white_yhat = predict(model_white, newdata=ww_test_data)
white_err = mean((white_yhat - white_wine[-indexes, "quality"])^2)
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
red_err = mean((red_yhat - red_wine[-indexes, "quality"])^2)
red_err
white_err = mean((white_yhat - white_wine[-indexes, "quality"])^2)
white_err

# now try it with a pruned tree
prune.red = prune.tree(tree.red, best=5)
summary(prune.red)
plot(prune.red)
text(prune.red, pretty=0)

prune.white = prune.tree(tree.white, best=5)
summary(prune.white)
plot(prune.white)
text(prune.white, pretty=0)

red_yhat = predict(prune.red, newdata = rw_test_data)
white_yhat = predict(prune.white, newdata=ww_test_data)

red_err = mean((red_yhat - red_wine[-indexes, "quality"])^2)
red_err
white_err = mean((white_yhat - white_wine[-indexes, "quality"])^2)
white_err
