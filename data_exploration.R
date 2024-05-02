all_wines <- read.csv("~/STSCI4740/final_proj/wine+quality/wine-quality-white-and-red.csv", header = TRUE)
red_wine <- read.csv("~/STSCI4740/final_proj/wine+quality/winequality-red.csv", header = TRUE, sep = ";")
white_wine <- read.csv("~/STSCI4740/final_proj/wine+quality/winequality-white.csv", header = TRUE, sep = ";")

#install.packages("dplyr")
#library("dplyr")
#distinct(all_wines)
#unique(red_wine)
#unique(white_wine)
View(wines)
View(red_wine)
View(white_wine)

# replace wine strings with numbers; white --> 2, red --> 1
wines$type = factor(wines$type)
wines$type =as.numeric(wines$type)

pairs(red_wine)
red_wines_correlations = cor(red_wine)
red_wines_correlations

pairs(white_wine)
white_wines_correlations = cor(white_wine)
white_wines_correlations

pairs(wines)
all_wines_correlations = cor(wines)
all_wines_correlations

install.packages("corrplot")
library("corrplot")
corrplot(all_wines_correlations, method = "pie")
corrplot(red_wines_correlations, method = "pie")
corrplot(white_wines_correlations, method = "pie")

# visualize the ranges of the data and see if there are potential outliers
summary(white_wine)
summary(red_wine)




