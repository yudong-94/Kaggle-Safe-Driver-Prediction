setwd("/Users/hzdy1994/Desktop/Kaggle")

library(parallel)
library(Matrix)
library(lightgbm)

load("data/new_feature_added.RData")

# remove all the calc features
train_test = train_test[,-(31:50)]

# fill in all the missing values with -1: should be some way to process this in lightGBM...
train_test[is.na(train_test)] = -1

train = train_test[train_test$data == "train", -2]
test = train_test[train_test$data != "train", -2]


train_matrix = sparse.model.matrix(target ~ .-1, data = train[, c(2:60)])
dlgb_train = lgb.Dataset(data = train_matrix, label = train$target)
test_matrix = as.matrix(test[,c(3:60)])

# cv
start = Sys.time()
param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 50, 
              max_depth = 5,
              min_data_in_leaf = 2000,
              num_threads = 3)

cv = lgb.cv(param,
            dlgb_train,
            nrounds = 1500,
            nfold = 5,
            eval = "auc",
            verbose = 1,
            early_stopping_rounds = 20)
Sys.time() - start


# training
start = Sys.time()
lgb_model <- lgb.train(data = dlgb_train, 
                       objective = "binary", 
                       learning_rate = 0.01,
                       nrounds = 1000, 
                       num_leaves = 1024, 
                       max_depth = 8,
                       nthread = 2)
Sys.time() - start


pred <- predict(lgb_model, test_matrix)
prediction <- data.frame(cbind(test$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction.csv", row.names = FALSE)

