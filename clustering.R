setwd("/Users/David/Desktop/stsci4740/predict-wine-quality/")
library(class)
library(caret)
library(ggplot2)
library(randomForest)

# data load
red_wine <- read.csv("./wine+quality/winequality-red.csv", header = TRUE, sep = ";")
white_wine <- read.csv("./wine+quality/winequality-white.csv", header = TRUE, sep = ";")

red_wine$wine_type <- 'red'
white_wine$wine_type <- 'white'
wine_data <- rbind(red_wine, white_wine)

set.seed(1)
train_indices <- sample(seq_len(nrow(wine_data)), nrow(wine_data)*0.7)
train_set <- wine_data[train_indices, ]
test_set <- wine_data[-train_indices, ]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_set_norm <- as.data.frame(lapply(train_set[,1:12], normalize)) # normalize only the features, not the label
test_set_norm <- as.data.frame(lapply(test_set[,1:12], normalize))

wine_type_pred <- knn(train = train_set_norm, test = test_set_norm, cl = train_set$wine_type, k = 3)

wine_type_pred <- as.factor(wine_type_pred)
test_set$wine_type <- as.factor(test_set$wine_type)

pca_result <- prcomp(test_set_norm[,1:11], center = TRUE, scale. = TRUE) # exclude the wine_type column

pca_df <- as.data.frame(pca_result$x)
pca_df$wine_type <- wine_type_pred

test_error <- sum(wine_type_pred != test_set$wine_type) / length(wine_type_pred)
print(test_error)

# ggplot(data = pca_df, aes(x = PC1, y = PC2, color = wine_type)) +
#   geom_point(alpha = 0.4) +
#   ggtitle("PCA of Wine Data") +
#   theme_minimal() +
#   theme(aspect.ratio = (1 / 1), 
#         axis.title = element_text(size = 15),  
#         axis.text = element_text(size = 15), 
#         plot.title = element_text(size = 18, hjust = 0.5))


# knn for but select features


features <- c('sulphates', 'alcohol', 'citric.acid', 'volatile.acidity', 'total.sulfur.dioxide', 'density', 'residual.sugar', 'quality')
train_set_norm_select <- train_set_norm[, features]
test_set_norm_select <- test_set_norm[, features]

wine_type_pred_select <- knn(train = train_set_norm_select, test = test_set_norm_select, cl = train_set$wine_type, k = 3)

wine_type_pred_select <- as.factor(wine_type_pred_select)

pca_result_select <- prcomp(test_set_norm_select, center = TRUE, scale. = TRUE)

pca_df_select <- as.data.frame(pca_result_select$x)
pca_df_select$wine_type <- wine_type_pred_select

test_error_select <- sum(wine_type_pred_select != test_set$wine_type) / length(wine_type_pred_select)
print(test_error_select)

ggplot(pca_df_select, aes(x = PC1, y = PC2, color = wine_type)) +
  geom_point() +
  labs(title = "PCA on Selected Features") +
  theme_minimal() +
  theme(aspect.ratio = (1 / 1), 
        axis.title = element_text(size = 15),  
        axis.text = element_text(size = 15), 
        plot.title = element_text(size = 18, hjust = 0.5))

