setwd("/Users/hzdy1994/Desktop/Kaggle")
library(xgboost)

load("data/new_feature_no_corr.RData")
#train_test[is.na(train_test)] = -1

train = train_test[train_test$data == "train", -2]
test = train_test[train_test$data != "train", -2]

train_x = train[,c(3:42)]
train_x = as.matrix(train_x)
train_y = as.vector(train$target)
train_x = xgb.DMatrix(data = train_x, label = train_y)
test_x = as.matrix(test[,c(3:42)])

start = Sys.time()
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.01, gamma=0, max_depth=5, 
               min_child_weight=5, subsample=0.6, 
               colsample_bytree=0.7)

xgbcv <- xgb.cv(params = params, 
                data = train_x,
                nrounds = 1500, nfold = 5, showsd = T, 
                stratified = T, print_every_n = 5, 
                early_stop_round = 20, 
                metrics = 'auc',
                maximize = T)
Sys.time() - start

best = which.max(xgbcv$evaluation_log$test_auc_mean)
# 0.6407798, gini = 0.2815596, n = 1395
# 2.5hr


params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.01, gamma=0, max_depth=5, 
               min_child_weight=5, subsample=0.6, 
               colsample_bytree=0.7)

xg_model = xgb.train(params = params,
                     data = train_x,
                     nrounds = best,
                     verbose = 1,
                     save_name = "xgboost_default.model")

pred <- predict(xg_model, test_x)
prediction <- data.frame(cbind(test$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction.csv", row.names = FALSE)
# 0.274.....???

importance = xgb.importance(feature_names = colnames(train[,c(3:42)]), model = xg_model)
write.csv(importance, "importance3.csv", row.names = FALSE)