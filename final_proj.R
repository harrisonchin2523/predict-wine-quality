

# databind
red_wine <- read.csv("./wine+quality/winequality-red.csv", header = TRUE, sep = ";")
white_wine <- read.csv("./wine+quality/winequality-white.csv", header = TRUE, sep = ";")

wine_data <- rbind(red_wine, white_wine)





model <- lm(quality ~ ., data = wine_data)


print(summary(model))
