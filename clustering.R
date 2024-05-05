setwd("/Users/David/Desktop/stsci4740/predict-wine-quality/")

# data load
red_wine <- read.csv("./wine+quality/winequality-red.csv", header = TRUE, sep = ";")
white_wine <- read.csv("./wine+quality/winequality-white.csv", header = TRUE, sep = ";")