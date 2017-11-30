setwd("/Users/hzdy1994/Desktop/Kaggle")

#library(parallel)
library(Matrix)
library(lightgbm)

####################################
# try to figure out the overfitting feature

load("data/new_feature_no_corr2.RData")

# suspects: avg_car13_on_car04

train_test$avg_car13_on_car04 = NULL

train_test[is.na(train_test)] = -1

train = train_test[train_test$data == "train", -2]
test = train_test[train_test$data != "train", -2]

train_matrix = sparse.model.matrix(target ~ .-1, data = train[, c(2:44)])
dlgb_train = lgb.Dataset(data = train_matrix, label = train$target)
test_matrix = as.matrix(test[,c(3:44)])

start = Sys.time()
param <- list(objective = "binary", 
              learning_rate = 0.0025,
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
            early_stopping_rounds = 20)
Sys.time() - start

# before getting rid of the feature: 0.642553
# after: 0.6419

#################################
# training with the normalized and missing value imputed data
load("data/new_feature_normalized.RData")

train = train_test[train_test$data == "train", -2]
test = train_test[train_test$data != "train", -2]

train_matrix = sparse.model.matrix(target ~ .-1, data = train[, c(2:51)])
dlgb_train = lgb.Dataset(data = train_matrix, label = train$target)
test_matrix = as.matrix(test[,c(3:51)])

cv_tunning = data.frame(num_leaves = numeric(0), 
                        min_hessian = numeric(0),
                        min_data_in_leaf = numeric(0),
                        best_itr = numeric(0), 
                        best_gini = numeric(0))

for (n_leaves in c(30, 50)) {
    for (min_hessian in c(50, 100, 150)) {
        for (min_leaf in c(1500, 2000, 2500)) {
            param <- list(objective = "binary", 
                          learning_rate = 0.0025,
                          num_leaves = n_leaves, 
                          max_depth = 4,
                          min_data_in_leaf = min_leaf,
                          min_sum_hessian_in_leaf = min_hessian,
                          num_threads = 3)
            
            cv = lgb.cv(param,
                        dlgb_train,
                        nrounds = 10000,
                        nfold = 5,
                        eval = "auc",
                        verbose = 1,
                        early_stopping_rounds = 50)
            
            cv_tunning[nrow(cv_tunning)+1, ] = c(n_leaves, 
                                                 min_hessian,
                                                 min_leaf,
                                                 cv$best_iter, 
                                                 cv$best_score)
            write.csv(cv_tunning, "tunning.csv", row.names = FALSE)
        }
    }
}

# not good.. best only 0.6423

# what if only normalization, no missing value imputation?
load("data/new_feature_no_corr3.RData")
train_test[is.na(train_test)] = -1
train_test[is.infinite(train_test$car13_car15),"car13_car15"] = 2.0
train_test[,c(4:52)] = scale(train_test[,c(4:52)])

train = train_test[train_test$data == "train", -2]
test = train_test[train_test$data != "train", -2]

train_matrix = sparse.model.matrix(target ~ .-1, data = train[, c(2:51)])
dlgb_train = lgb.Dataset(data = train_matrix, label = train$target)
test_matrix = as.matrix(test[,c(3:51)])

param <- list(objective = "binary", 
              learning_rate = 0.0025,
              num_leaves = 50, 
              max_depth = 4,
              min_data_in_leaf = 2000,
              min_sum_hessian_in_leaf = 125,
              num_threads = 3)

cv = lgb.cv(param,
            dlgb_train,
            nrounds = 10000,
            nfold = 5,
            eval = "auc",
            verbose = 1,
            early_stopping_rounds = 50)

# before normalization: 0.6428838
# after: 0.641625

# what if missing value imputation only, but not normalization?
load("data/new_feature_no_corr3.RData")

# input missing values with median
for (col in colnames(train_test)) {
    if (anyNA(train_test[,col])) {
        median_col = median(na.omit(train_test[,col]))
        train_test[is.na(train_test[,col]),col] = median_col
    }
}

train = train_test[train_test$data == "train", -2]
test = train_test[train_test$data != "train", -2]

train_matrix = sparse.model.matrix(target ~ .-1, data = train[, c(2:51)])
dlgb_train = lgb.Dataset(data = train_matrix, label = train$target)
test_matrix = as.matrix(test[,c(3:51)])

param <- list(objective = "binary", 
              learning_rate = 0.0025,
              num_leaves = 50, 
              max_depth = 4,
              min_data_in_leaf = 2000,
              min_sum_hessian_in_leaf = 125,
              num_threads = 3)

cv = lgb.cv(param,
            dlgb_train,
            nrounds = 10000,
            nfold = 5,
            eval = "auc",
            verbose = 1,
            early_stopping_rounds = 50)


# before missing value imputation: 0.6428838
# after: 0.641624

#################################
# new feature set 4 - added categorical regs
load("data/new_feature_4.RData")

train_test[is.na(train_test)] = -1

train = train_test[train_test$data == "train", -2]
test = train_test[train_test$data != "train", -2]

train_matrix = sparse.model.matrix(target ~ .-1, data = train[, c(2:51)])
dlgb_train = lgb.Dataset(data = train_matrix, label = train$target)
test_matrix = as.matrix(test[,c(3:51)])

cv_tunning = data.frame(num_leaves = numeric(0), 
                        min_hessian = numeric(0),
                        best_itr = numeric(0), 
                        best_gini = numeric(0))

for (n_leaves in c(25, 50)) {
    for (min_hessian in c(50, 100, 125)) {
        param <- list(objective = "binary", 
                      learning_rate = 0.0025,
                      num_leaves = n_leaves, 
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
        
        cv_tunning[nrow(cv_tunning)+1, ] = c(n_leaves, 
                                             min_hessian,
                                             cv$best_iter, 
                                             cv$best_score)
        write.csv(cv_tunning, "tunning.csv", row.names = FALSE)
    }
}

param <- list(objective = "binary", 
              learning_rate = 0.0025,
              num_leaves = 50, 
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

start = Sys.time()
lgb_model <- lgb.train(data = dlgb_train, 
                       objective = "binary", 
                       learning_rate = 0.0025,
                       nrounds = 6545,
                       num_leaves = 50, 
                       max_depth = 4,
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
# public lb = 0.280

importance = lgb.importance(lgb_model)
write.csv(importance, "importance.csv", row.names = FALSE)

# save the predictions for the training set
pred <- predict(lgb_model, train_matrix)
prediction <- data.frame(cbind(train$id, pred, train$target))
colnames(prediction) = c("id", "pred", "target")
write.csv(prediction, "lightgbm7_train.csv", row.names = FALSE)

#################################
# new feature set 5 - added ps_car_14 deviation features

train_test[is.na(train_test)] = -1

train = train_test[train_test$data == "train", -2]
test = train_test[train_test$data != "train", -2]

train_matrix = sparse.model.matrix(target ~ .-1, data = train[, c(2:54)])
dlgb_train = lgb.Dataset(data = train_matrix, label = train$target)
test_matrix = as.matrix(test[,c(3:54)])

cv_tunning = data.frame(num_leaves = numeric(0), 
                        min_hessian = numeric(0),
                        best_itr = numeric(0), 
                        best_gini = numeric(0))

for (n_leaves in c(25, 50)) {
    for (min_hessian in c(50, 100, 125)) {
        param <- list(objective = "binary", 
                      learning_rate = 0.0025,
                      num_leaves = n_leaves, 
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
        
        cv_tunning[nrow(cv_tunning)+1, ] = c(n_leaves, 
                                             min_hessian,
                                             cv$best_iter, 
                                             cv$best_score)
        write.csv(cv_tunning, "tunning3.csv", row.names = FALSE)
    }
}

# not good....
