setwd("/Users/David/Desktop/stsci4740/predict-wine-quality/")

# databind
red_wine <- read.csv("./wine+quality/winequality-red.csv", header = TRUE, sep = ";")
white_wine <- read.csv("./wine+quality/winequality-white.csv", header = TRUE, sep = ";")
wine_data <- rbind(red_wine, white_wine)



# baseline regression model
set.seed(1)  # for reproducibility
indexes <- sample(seq_len(nrow(wine_data)), size = 0.8 * nrow(wine_data))

rw_train_data <- red_wine[indexes, ]
rw_test_data <- red_wine[-indexes, ]

ww_train_data <- white_wine[indexes, ]
rw_test_data <- white_wine[-indexes, ]

model_red <- lm(quality ~ ., data = rw_train_data)
model_white <- lm(quality ~ ., data = ww_train_data)

print(summary(model_white))
print(summary(model_red))










