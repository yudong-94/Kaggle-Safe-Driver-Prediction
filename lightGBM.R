#############################################################
# one hot code of categorical variables
setwd("/Users/hzdy1994/Desktop/Kaggle")

load("data/train&test.RData")

library(ade4)
library(data.table)
ohe_feats = c("ps_ind_02_cat", "ps_ind_04_cat", "ps_ind_05_cat", 
              "ps_car_01_cat", "ps_car_02_cat", "ps_car_03_cat",
              "ps_car_04_cat", "ps_car_05_cat", "ps_car_07_cat",
              "ps_car_06_cat", "ps_car_08_cat", "ps_car_09_cat",
              "ps_car_10_cat")
train$ps_ind_02_cat = as.factor(train$ps_ind_02_cat)
train$ps_ind_04_cat = as.factor(train$ps_ind_04_cat)
train$ps_ind_05_cat = as.factor(train$ps_ind_05_cat)
train$ps_car_01_cat = as.factor(train$ps_car_01_cat)
train$ps_car_02_cat = as.factor(train$ps_car_02_cat)
train$ps_car_03_cat = as.factor(train$ps_car_03_cat)
train$ps_car_04_cat = as.factor(train$ps_car_04_cat)
train$ps_car_05_cat = as.factor(train$ps_car_05_cat)
train$ps_car_06_cat = as.factor(train$ps_car_06_cat)
train$ps_car_07_cat = as.factor(train$ps_car_07_cat)
train$ps_car_08_cat = as.factor(train$ps_car_08_cat)
train$ps_car_09_cat = as.factor(train$ps_car_09_cat)
train$ps_car_10_cat = as.factor(train$ps_car_10_cat)

for (f in ohe_feats){
    train_dummy = acm.disjonctif(train[f])
    train[f] = NULL
    train = cbind(train, train_dummy)
}

colnames(train)

test$ps_ind_02_cat = as.factor(test$ps_ind_02_cat)
test$ps_ind_04_cat = as.factor(test$ps_ind_04_cat)
test$ps_ind_05_cat = as.factor(test$ps_ind_05_cat)
test$ps_car_01_cat = as.factor(test$ps_car_01_cat)
test$ps_car_02_cat = as.factor(test$ps_car_02_cat)
test$ps_car_03_cat = as.factor(test$ps_car_03_cat)
test$ps_car_04_cat = as.factor(test$ps_car_04_cat)
test$ps_car_05_cat = as.factor(test$ps_car_05_cat)
test$ps_car_06_cat = as.factor(test$ps_car_06_cat)
test$ps_car_07_cat = as.factor(test$ps_car_07_cat)
test$ps_car_08_cat = as.factor(test$ps_car_08_cat)
test$ps_car_09_cat = as.factor(test$ps_car_09_cat)
test$ps_car_10_cat = as.factor(test$ps_car_10_cat)

for (f in ohe_feats){
    test_dummy = acm.disjonctif(test[f])
    test[f] = NULL
    test = cbind(test, test_dummy)
}

rm(test_dummy, train_dummy)
rm(f, ohe_feats)

colnames(train) = gsub("\\.", "", colnames(train))
colnames(train) = gsub("-", "_", colnames(train))
colnames(test) = gsub("\\.", "", colnames(test))
colnames(test) = gsub("-", "_", colnames(test))

#############################################################
# initial training
library(parallel)
library(Matrix)
library(lightgbm)

train_matrix = sparse.model.matrix(target ~ .-1, data = train[, c(2:126)])
dlgb_train = lgb.Dataset(data = train_matrix, label = train$target)
test_matrix = as.matrix(test[,c(2:125)])

start = Sys.time()
lgb_model <- lgb.train(data = dlgb_train, 
                       objective = "binary", 
                       learning_rate = 0.01,
                       nrounds = 1000, 
                       num_leaves = 1024, 
                       max_depth = 8,
                       nthread = 2)
Sys.time() - start
# 3.6min

pred <- predict(lgb_model, test_matrix)
prediction <- data.frame(cbind(test$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction.csv", row.names = FALSE)
# 0.275

#############################################################
# tune parameters
normalizedGini <- function(aa, pp) {
    Gini <- function(a, p) {
        if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
        temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
        temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
        population.delta <- 1 / length(a)
        total.losses <- sum(a)
        null.losses <- rep(population.delta, length(a)) # Hopefully is similar to accumulatedPopulationPercentageSum
        accum.losses <- temp.df$actual / total.losses # Hopefully is similar to accumulatedLossPercentageSum
        gini.sum <- cumsum(accum.losses - null.losses) # Not sure if this is having the same effect or not
        sum(gini.sum) / length(a)
    }
    Gini(aa,pp) / Gini(aa,aa)
}

GiniEvaluation <- function(preds, train) {
    label = getinfo(train, "label")
    gini = normalizedGini(label, preds)
    return(list(name = "Gini", value = gini, higher_better = TRUE))
}

param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 1024, 
              max_depth = 8,
              num_threads = 2)

lgb.cv(param,
       dlgb_train,
       nrounds = 1000,
       nfold = 5,
       eval = GiniEvaluation,
       verbose = 1)

# best around 600: 0.2758
# around 30min

#### tune max depth
param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 1024, 
              max_depth = 7,
              num_threads = 2)

cv_7 = lgb.cv(param,
       dlgb_train,
       nrounds = 800,
       nfold = 5,
       eval = GiniEvaluation,
       verbose = 1)

gini_cv = cv_7$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]

# best around 723 0.2775663

param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 1024, 
              max_depth = 6,
              num_threads = 2)

cv_6 = lgb.cv(param,
              dlgb_train,
              nrounds = 1500,
              nfold = 5,
              eval = GiniEvaluation,
              verbose = 1)

gini_cv = cv_6$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
# best: 896, 0.2799289 (could be higher with more rounds)
# another run: 941, 0.2785583

param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 1024, 
              max_depth = 5,
              num_threads = 2)

cv_5 = lgb.cv(param,
              dlgb_train,
              nrounds = 1100,
              nfold = 5,
              eval = GiniEvaluation,
              verbose = 1)

gini_cv = cv_5$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
# best: 1099,0.2794919 (could be higher....)

#### tune num_leaves
param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 512, 
              max_depth = 6,
              num_threads = 2)

cv_512 = lgb.cv(param,
              dlgb_train,
              nrounds = 1500,
              nfold = 5,
              eval = GiniEvaluation,
              verbose = 1)

gini_cv = cv_512$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
# 916, 0.2786266


param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 64, 
              max_depth = 6,
              num_threads = 2)

cv_64 = lgb.cv(param,
                dlgb_train,
                nrounds = 1500,
                nfold = 5,
                eval = GiniEvaluation,
                verbose = 1)

gini_cv = cv_64$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
# 929, 0.2804271

param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 50, 
              max_depth = 6,
              num_threads = 2)

cv_50 = lgb.cv(param,
               dlgb_train,
               nrounds = 1200,
               nfold = 5,
               eval = GiniEvaluation,
               verbose = 1,
               early_stopping_rounds = 20)

gini_cv = cv_50$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
# 828, 0.2801691

param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 80, 
              max_depth = 6,
              num_threads = 2)

cv_80 = lgb.cv(param,
               dlgb_train,
               nrounds = 1000,
               nfold = 5,
               eval = GiniEvaluation,
               verbose = 1,
               early_stopping_rounds = 20)

gini_cv = cv_80$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
#783, 0.2784863

param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 31, 
              max_depth = 5,
              num_threads = 2)

cv_31 = lgb.cv(param,
               dlgb_train,
               nrounds = 1500,
               nfold = 5,
               eval = GiniEvaluation,
               verbose = 1,
               early_stopping_rounds = 20)

gini_cv = cv_31$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
# 1000, 0.2801819

param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 50, 
              max_depth = 5,
              num_threads = 2)

cv_50 = lgb.cv(param,
               dlgb_train,
               nrounds = 1500,
               nfold = 5,
               eval = GiniEvaluation,
               verbose = 1,
               early_stopping_rounds = 20)

gini_cv = cv_50$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
# 1054, 0.2812939


param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 60, 
              max_depth = 5,
              num_threads = 2)

cv_60 = lgb.cv(param,
               dlgb_train,
               nrounds = 1500,
               nfold = 5,
               eval = GiniEvaluation,
               verbose = 1,
               early_stopping_rounds = 20)

gini_cv = cv_60$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
# 1014, 0.2795623

#### tune min_data_in_leaf

param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 50, 
              max_depth = 5,
              min_data_in_leaf = 100,
              num_threads = 2)

cv_100 = lgb.cv(param,
               dlgb_train,
               nrounds = 1500,
               nfold = 5,
               eval = GiniEvaluation,
               verbose = 1,
               early_stopping_rounds = 20)

gini_cv = cv_100$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
# 1027, 0.2809446


param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 50, 
              max_depth = 5,
              min_data_in_leaf = 500,
              num_threads = 2)

cv_500 = lgb.cv(param,
                dlgb_train,
                nrounds = 1500,
                nfold = 5,
                eval = GiniEvaluation,
                verbose = 1,
                early_stopping_rounds = 20)

gini_cv = cv_500$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
# 1216, 0.282941


param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 50, 
              max_depth = 5,
              min_data_in_leaf = 1000,
              num_threads = 2)

cv_1000 = lgb.cv(param,
                dlgb_train,
                nrounds = 1500,
                nfold = 5,
                eval = GiniEvaluation,
                verbose = 1,
                early_stopping_rounds = 20)

gini_cv = cv_1000$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
#1176, 0.2813552

param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 50, 
              max_depth = 5,
              min_data_in_leaf = 1500,
              num_threads = 2)

cv_1500 = lgb.cv(param,
                 dlgb_train,
                 nrounds = 1500,
                 nfold = 5,
                 eval = GiniEvaluation,
                 verbose = 1,
                 early_stopping_rounds = 20)

gini_cv = cv_1500$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
#1093, 0.2822626

param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 50, 
              max_depth = 5,
              min_data_in_leaf = 2000,
              num_threads = 2)

cv_2000 = lgb.cv(param,
                 dlgb_train,
                 nrounds = 1500,
                 nfold = 5,
                 eval = GiniEvaluation,
                 verbose = 1,
                 early_stopping_rounds = 20)

gini_cv = cv_2000$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
#1323, 0.2829504

param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 50, 
              max_depth = 5,
              min_data_in_leaf = 3000,
              num_threads = 2)

cv_3000 = lgb.cv(param,
                 dlgb_train,
                 nrounds = 1600,
                 nfold = 5,
                 eval = GiniEvaluation,
                 verbose = 1,
                 early_stopping_rounds = 20)

gini_cv = cv_3000$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
# 1367, 0.282511

#### try dart - not good

#### specify is_unbalance
param <- list(objective = "binary", 
              learning_rate = 0.01,
              num_leaves = 50, 
              max_depth = 5,
              min_data_in_leaf = 2000,
              is_unbalance = TRUE,
              num_threads = 2)

cv_ub = lgb.cv(param,
                 dlgb_train,
                 nrounds = 1500,
                 nfold = 5,
                 eval = GiniEvaluation,
                 verbose = 1,
                 early_stopping_rounds = 20)

gini_cv = cv_ub$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
#951 0.2815672

#### lower down learing rate
param <- list(objective = "binary", 
              learning_rate = 0.005,
              num_leaves = 50, 
              max_depth = 5,
              min_data_in_leaf = 2000,
              is_unbalance = TRUE,
              num_threads = 2)

cv_ub = lgb.cv(param,
               dlgb_train,
               nrounds = 2000,
               nfold = 5,
               eval = GiniEvaluation,
               verbose = 1,
               early_stopping_rounds = 30)

gini_cv = cv_ub$record_evals[-1]$valid$Gini$eval
which.max(gini_cv)
gini_cv[which.max(gini_cv)]
#1995, 0.2817891


cv_tunning = data.frame(num_leaves = numeric(0), 
                        max_depth = numeric(0),
                        min_data_in_leaf = numeric(0),
                        best_itr = numeric(0), 
                        best_gini = numeric(0))

# turn colsample_bylevel
for (num_leaves in seq(30, 60, 10)) {
    for (max_depth in c(5, 6, 7)) {
        for (min_data_in_leaf in seq(500, 2500, 500)) {
            param <- list(objective = "binary", 
                          learning_rate = 0.01,
                          num_leaves = num_leaves, 
                          max_depth = max_depth,
                          min_data_in_leaf = min_data_in_leaf,
                          num_threads = 3)
            
            cv = lgb.cv(param,
                        dlgb_train,
                        nrounds = 1500,
                        nfold = 5,
                        eval = GiniEvaluation,
                        verbose = 0,
                        early_stopping_rounds = 20)
            
            gini_cv = cv$record_evals[-1]$valid$Gini$eval
            cv_tunning[nrow(cv_tunning)+1, ] = c(num_leaves, 
                                                 max_depth,
                                                 min_data_in_leaf,
                                                 which.max(gini_cv), 
                                                 gini_cv[which.max(gini_cv)])
            write.csv(cv_tunning, "tunning.csv", row.names = FALSE)
        }
    }
}

# 2:30pm


# training the data with tunned parameters
start = Sys.time()
lgb_model <- lgb.train(data = dlgb_train, 
                       objective = "binary", 
                       learning_rate = 0.01,
                       nrounds = 1323,
                       num_leaves = 50, 
                       max_depth = 5,
                       min_data_in_leaf = 2000,
                       num_threads = 2)
Sys.time() - start
# 2.3min


pred <- predict(lgb_model, test_matrix)
prediction <- data.frame(cbind(test$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction.csv", row.names = FALSE)
# 0.278

start = Sys.time()
lgb_model <- lgb.train(data = dlgb_train, 
                       objective = "binary", 
                       learning_rate = 0.01,
                       nrounds = 1000,
                       num_leaves = 50, 
                       max_depth = 5,
                       min_data_in_leaf = 2000,
                       is_unbalance = TRUE,
                       num_threads = 2)
Sys.time() - start
# 2.3min


pred <- predict(lgb_model, test_matrix)
prediction <- data.frame(cbind(test$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction.csv", row.names = FALSE)
# 0.278 - gini remains the same, but the predicted probability boosted a lot


# how to ensemble xgboost and lightGBM to optimize AUC