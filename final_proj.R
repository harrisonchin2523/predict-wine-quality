wines <- read.csv("~/STSCI4740/final_proj/wine+quality/wine-quality-white-and-red.csv", header = TRUE)
View(wines)

# replace wine strings with numbers; white --> 2, red --> 1
wines$type = factor(wines$type)
wines$type =as.numeric(wines$type)
View(wines)

pairs(wines)
correlations = cor(wines)
correlations

install.packages("corrplot")
library("corrplot")
corrplot(correlations, method = "color")
