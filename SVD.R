setwd("/Users/hzdy1994/Desktop/Kaggle")

load("data/one-hot-coding.RData")
train = train_test[train_test$data == "train",-2]
test = train_test[train_test$data != "train",-2]

library(Matrix)
library(dplyr)
library(ggplot2)

train_matrix = as.matrix(train[,c(3:126)])

train_svd = svd(train_matrix)
train_u = train_svd$u
S = diag(train_svd$d ^ 0.5)

user_similarity = train_u %*% S
user_similarity_t = t(user_similarity)
# user_correlation = cor(data.frame(user_similarity_t))
user_similarity_df = data.frame(user_similarity)
user_similarity_df = cbind(train$target, user_similarity_df)
user_similarity_df %>%
    group_by(train$target) %>%
    summarise(avg_x1 = sum(X1) / n(),
              avg_x2 = sum(X2) / n(),
              avg_x3 = sum(X3) / n(),
              avg_x4 = sum(X4) / n(),
              avg_x5 = sum(X5) / n())

user_similarity_1 = filter(user_similarity_df, train$target == 1)
user_similarity_0 = filter(user_similarity_df, train$target == 0)
user_similarity_1 = t(user_similarity_1[,-1])
user_similarity_0 = t(user_similarity_0[,-1])

cor(user_similarity_1[, 1:20])
cor(user_similarity_1[, 1:20], user_similarity_0[, 1:20])
