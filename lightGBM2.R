setwd("/Users/hzdy1994/Desktop/Kaggle")

#library(parallel)
library(Matrix)
library(lightgbm)

load("data/new_feature_added.RData")

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
# 9.4min
# auc = 0.641807, gini = 0.283614

# training
start = Sys.time()
lgb_model <- lgb.train(data = dlgb_train, 
                       objective = "binary", 
                       learning_rate = 0.01,
                       nrounds = 1156, 
                       num_leaves = 50, 
                       max_depth = 5,
                       min_data_in_leaf = 2000,
                       num_threads = 3)
Sys.time() - start
# 1.8min

pred <- predict(lgb_model, test_matrix)
prediction <- data.frame(cbind(test$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction.csv", row.names = FALSE)
# 0.278

importance = lgb.importance(lgb_model)


start = Sys.time()
param <- list(objective = "binary", 
              learning_rate = 0.001,
              num_leaves = 50, 
              max_depth = 5,
              min_data_in_leaf = 2000,
              num_threads = 3)

cv = lgb.cv(param,
            dlgb_train,
            nrounds = 7000,
            nfold = 5,
            eval = "auc",
            verbose = 1,
            early_stopping_rounds = 20)
Sys.time() - start
# 0.01: 0.641807, n = 1156
# 0.005: 0.642777, n = 2857
# 0.003: 0.642995, n = 3969, 30min
# 0.001: 0.642033, n = 10730, 1.5hr


#########################
# with the dataset excluding multicollinearity
load("data/new_feature_no_corr.RData")

train_test[is.na(train_test)] = -1

train = train_test[train_test$data == "train", -2]
test = train_test[train_test$data != "train", -2]

train_matrix = sparse.model.matrix(target ~ .-1, data = train[, c(2:42)])
dlgb_train = lgb.Dataset(data = train_matrix, label = train$target)
test_matrix = as.matrix(test[,c(3:42)])

start = Sys.time()
param <- list(objective = "binary", 
              learning_rate = 0.0025,
              num_leaves = 50, 
              max_depth = 5,
              min_data_in_leaf = 2000,
              min_sum_hessian_in_leaf = 50,
              num_threads = 3)

cv = lgb.cv(param,
            dlgb_train,
            nrounds = 10000,
            nfold = 5,
            eval = "auc",
            verbose = 1,
            early_stopping_rounds = 20)
Sys.time() - start

# 0.642729, min_hessian = 20, n = 5000
# 0.643045, min_hessian = 50, n = 5811
# 0.0.642427, min_hessian = 100, n = 4630

# training
start = Sys.time()
lgb_model <- lgb.train(data = dlgb_train, 
                       objective = "binary", 
                       learning_rate = 0.0025,
                       nrounds = 5811,
                       num_leaves = 50, 
                       max_depth = 5,
                       min_data_in_leaf = 2000,
                       min_sum_hessian_in_leaf = 50,
                       num_threads = 3)
Sys.time() - start
# 7 min

pred <- predict(lgb_model, test_matrix)
prediction <- data.frame(cbind(test$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction.csv", row.names = FALSE)
# test gini = 0.286
# 0.279

importance = lgb.importance(lgb_model)
write.csv(importance, "importance2.csv", row.names = FALSE)

########################
# with the dataset with more features
load("data/new_feature_no_corr2.RData")

train_test[is.na(train_test)] = -1

train = train_test[train_test$data == "train", -2]
test = train_test[train_test$data != "train", -2]

train_matrix = sparse.model.matrix(target ~ .-1, data = train[, c(2:45)])
dlgb_train = lgb.Dataset(data = train_matrix, label = train$target)
test_matrix = as.matrix(test[,c(3:45)])

start = Sys.time()
param <- list(objective = "binary", 
              learning_rate = 0.0025,
              num_leaves = 25, 
              max_depth = 4,
              min_data_in_leaf = 2000,
              min_sum_hessian_in_leaf = 50,
              num_threads = 3)

cv = lgb.cv(param,
            dlgb_train,
            nrounds = 10000,
            nfold = 5,
            eval = "auc",
            verbose = 1,
            early_stopping_rounds = 20)
Sys.time() - start
# max_depth = 5, min_hessian = 50, min_data_in_leaf = 2000, num_leaves = 50: n = 5163, 0.642076
# max_depth = 4, min_hessian = 50, min_data_in_leaf = 2000, num_leaves = 50: n = 5807, 0.642406
# max_depth = 4, min_hessian = 75, min_data_in_leaf = 2000, num_leaves = 50: n = 5955, 0.642052
# max_depth = 4, min_hessian = 50, min_data_in_leaf = 2500, num_leaves = 50: n = 6055, 0.641268
# max_depth = 4, min_hessian = 50, min_data_in_leaf = 2000, num_leaves = 40: n = 6001, 0.642397
# max_depth = 4, min_hessian = 50, min_data_in_leaf = 2000, num_leaves = 30: n = 6362, 0.642553
# max_depth = 4, min_hessian = 50, min_data_in_leaf = 2000, num_leaves = 25: n = 5805, 0.641549


# training
start = Sys.time()
lgb_model <- lgb.train(data = dlgb_train, 
                       objective = "binary", 
                       learning_rate = 0.0025,
                       nrounds = 6362,
                       num_leaves = 30, 
                       max_depth = 4,
                       min_data_in_leaf = 2000,
                       min_sum_hessian_in_leaf = 50,
                       num_threads = 3)
Sys.time() - start
# 11min


pred <- predict(lgb_model, test_matrix)
prediction <- data.frame(cbind(test$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction.csv", row.names = FALSE)
# test gini = 0.2851
# 0.279

importance = lgb.importance(lgb_model)
write.csv(importance, "importance.csv", row.names = FALSE)

# save the predictions for the training set
pred <- predict(lgb_model, train_matrix)
prediction <- data.frame(cbind(train$id, pred, train$target))
colnames(prediction) = c("id", "pred", "target")
write.csv(prediction, "prediction_train_lightgbm4.csv", row.names = FALSE)


########################
# train with the same set of new features with one-hot-coding

load("data/one_hot_encoding_ver.RData")

train_test = train_test3
rm(train_test3)

train_test$ps_ind_10_bin = NULL
train_test$ps_ind_11_bin = NULL
train_test$ps_ind_13_bin = NULL
train_test$avg_car13_on_car10 = NULL
train_test$car14_car13 = NULL
train_test$avg_car13_on_car08 = NULL
train_test$avg_car13_on_car06 = NULL
train_test$ps_ind_12_bin = NULL
train_test$avg_car13_on_ind08 = NULL
train_test$avg_car13_on_ind09 = NULL
train_test$avg_car13_on_ind10 = NULL
train_test$avg_car13_on_ind11 = NULL
train_test$avg_car13_on_ind12 = NULL
train_test$avg_car13_on_ind13 = NULL
train_test$avg_car13_on_ind14 = NULL
train_test$avg_car13_on_ind15 = NULL
train_test$avg_car13_on_ind16 = NULL
train_test$avg_car13_on_ind17 = NULL
train_test$avg_car13_on_ind18 = NULL
train_test$avg_car13_on_reg01 = NULL
train_test$avg_car13_on_ind01 = NULL
train_test$avg_car13_on_ind03 = NULL
train_test$avg_car13_on_reg02 = NULL
train_test$avg_car13_on_ind06 = NULL
train_test$avg_car13_on_ind07 = NULL

train_test[is.na(train_test)] = -1
train = train_test[train_test$data == "train", -2]
test = train_test[train_test$data != "train", -2]


train_matrix = sparse.model.matrix(target ~ .-1, data = train[, c(2:147)])
dlgb_train = lgb.Dataset(data = train_matrix, label = train$target)
test_matrix = as.matrix(test[,c(3:147)])

# cv
start = Sys.time()
param <- list(objective = "binary", 
              learning_rate = 0.002,
              num_leaves = 30, 
              max_depth = 4,
              min_data_in_leaf = 2000,
              min_sum_hessian_in_leaf = 50,
              num_threads = 3)

cv = lgb.cv(param,
            dlgb_train,
            nrounds = 10000,
            nfold = 5,
            eval = "auc",
            verbose = 1,
            early_stopping_rounds = 50)
Sys.time() - start
# stuck around 0.6406 - not as good as impact encoding


########################
#train with 3rd phase new features

load("data/new_feature_no_corr3.RData")

train_test[is.na(train_test)] = -1

train = train_test[train_test$data == "train", -2]
test = train_test[train_test$data != "train", -2]

train_matrix = sparse.model.matrix(target ~ .-1, data = train[, c(2:51)])
dlgb_train = lgb.Dataset(data = train_matrix, label = train$target)
test_matrix = as.matrix(test[,c(3:51)])

start = Sys.time()
param <- list(objective = "binary", 
              learning_rate = 0.0025,
              num_leaves = 25, 
              max_depth = 4,
              min_data_in_leaf = 2000,
              min_sum_hessian_in_leaf = 50,
              num_threads = 3)

cv = lgb.cv(param,
            dlgb_train,
            nrounds = 10000,
            nfold = 5,
            eval = "auc",
            verbose = 1,
            early_stopping_rounds = 50)
Sys.time() - start

# n = 5508, auc = 0.642151, 0.2843
# lb = 0.279

cv_tunning = data.frame(num_leaves = numeric(0), 
                        max_depth = numeric(0),
                        min_data_in_leaf = numeric(0),
                        best_itr = numeric(0), 
                        best_gini = numeric(0))

for (n_leaves in c(25, 35, 50)) {
    for (max_depth in c(4, 5)) {
        for (min_leaf in c(2000, 2500)) {
            param <- list(objective = "binary", 
                          learning_rate = 0.0025,
                          num_leaves = n_leaves, 
                          max_depth = max_depth,
                          min_data_in_leaf = min_leaf,
                          min_sum_hessian_in_leaf = 50,
                          num_threads = 3)
            
            cv = lgb.cv(param,
                        dlgb_train,
                        nrounds = 10000,
                        nfold = 5,
                        eval = "auc",
                        verbose = 1,
                        early_stopping_rounds = 50)
            
            cv_tunning[nrow(cv_tunning)+1, ] = c(n_leaves, 
                                                 max_depth,
                                                 min_leaf,
                                                 cv$best_iter, 
                                                 cv$best_score)
            write.csv(cv_tunning, "tunning.csv", row.names = FALSE)
        }
    }
}
# best: 50, 4, 2000: n = 7578, auc = 0.6426

cv_tunning = data.frame(min_hessian = numeric(0), 
                        best_itr = numeric(0), 
                        best_gini = numeric(0))

for (min_hessian in c(25, 50, 75, 100, 125, 150)) {
            param <- list(objective = "binary", 
                          learning_rate = 0.0025,
                          num_leaves = 50, 
                          max_depth = 4,
                          min_data_in_leaf = 2000,
                          min_sum_hessian_in_leaf = min_hessian,
                          num_threads = 3)
            
            cv = lgb.cv(param,
                        dlgb_train,
                        nrounds = 10000,
                        nfold = 5,
                        eval = "auc",
                        verbose = 1,
                        early_stopping_rounds = 50)
            
            cv_tunning[nrow(cv_tunning)+1, ] = c(min_hessian,
                                                 cv$best_iter, 
                                                 cv$best_score)
            write.csv(cv_tunning, "tunning.csv", row.names = FALSE)
}
# best: 125, 0.6428838


# training
start = Sys.time()
lgb_model <- lgb.train(data = dlgb_train, 
                       objective = "binary", 
                       learning_rate = 0.0025,
                       nrounds = 7730,
                       num_leaves = 50, 
                       max_depth = 4,
                       min_data_in_leaf = 2000,
                       min_sum_hessian_in_leaf = 125,
                       num_threads = 3)
Sys.time() - start
# 7 min

pred <- predict(lgb_model, test_matrix)
prediction <- data.frame(cbind(test$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction.csv", row.names = FALSE)
# test gini = 0.2857
# public lb = 0.280

importance = lgb.importance(lgb_model)
write.csv(importance, "importance.csv", row.names = FALSE)

# save the predictions for the training set
pred <- predict(lgb_model, train_matrix)
prediction <- data.frame(cbind(train$id, pred, train$target))
colnames(prediction) = c("id", "pred", "target")
write.csv(prediction, "lightgbm6_train.csv", row.names = FALSE)

