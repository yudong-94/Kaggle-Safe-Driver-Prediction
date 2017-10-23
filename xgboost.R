setwd("/Users/hzdy1994/Desktop/Kaggle")
library(xgboost)

train = read.csv("train.csv")
test = read.csv("test.csv")

dim(train)
# 595k * 59
summary(train)
sum(train$target) / nrow(train) 
# only 3.6%

#######################################################

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
    return(list(metric = "Gini", value = gini))
}

#normalizedGini(c(0.8,0.9,0.1), c(1,1,0))
#GiniEvaluation(c(1,1,0), c(0.8,0.9,0.1))

# initialize data
train_x = train[,c(3:59)]
train_x = as.matrix(train_x)
train_y = as.vector(train$target)
train_x = xgb.DMatrix(data = train_x, label = train_y)
test_x = as.matrix(test[,c(2:58)])

#######################################################

# cross validation to find the best nround
Sys.time()
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.01, gamma=0, max_depth=6, 
               min_child_weight=1, subsample=1, 
               colsample_bytree=1)

# max_delta_step=50 or 100 just amplify the differences (50 is better)

xgbcv <- xgb.cv(params = params, 
                data = train_x,
                nrounds = 1000, nfold = 5, showsd = T, 
                stratified = T, print_every_n = 5, 
                early_stop_round = 20, 
                feval = GiniEvaluation,
                maximize = T)

plot(xgbcv$evaluation_log$test_Gini_mean)
which.max(xgbcv$evaluation_log$test_Gini_mean)
# best nround: 800-1000, around 0.279


# train a default model
Sys.time()
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.01, gamma=0, max_depth=6, 
               min_child_weight=1, subsample=1, 
               colsample_bytree=1)

xg_model = xgb.train(params = params,
                     data = train_x,
                     nrounds = 900,
                     verbose = 1,
                     save_name = "xgboost_default.model")

Sys.time()
pred <- predict(xg_model, test_x)  #30mins
Sys.time()

prediction <- data.frame(cbind(test$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction.csv", row.names = FALSE)

#### 0.275

#######################################################

# do a grid search on other parameters

#create learner
library(mlr)
lrn <- makeLearner("classif.xgboost", predict.type = "prob")
lrn$par.vals <- list(booster = "gbtree", 
                     objective="binary:logistic", 
                     eval_metric="auc", nrounds=1000L, eta=0.01)
#set parameter space
params <- makeParamSet(makeIntegerParam("max_depth",lower = 4L,upper = 8L),
                       makeNumericParam("min_child_weight",lower = 4L,upper = 10L),
                       makeNumericParam("subsample",lower = 0.6,upper = 1),
                       makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))
#set resampling strategy
rdesc <- makeResampleDesc("CV", stratify=T, iters=5L)
#search strategy
ctrl <- makeTuneControlRandom(maxit = 20L)
#create task
train_task_data = train[,c(2:59)]
traintask <- makeClassifTask(data = train_task_data, target = "target")

#set parallel backend
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores() - 1)

#parameter tuning
Sys.time()
mytune <- tuneParams(learner = lrn, 
                     task = traintask, 
                     resampling = rdesc, 
                     measures = auc, 
                     par.set = params, 
                     control = ctrl, 
                     show.info = T)

Sys.time()
## 24.5 hr


##Result: max_depth=7; min_child_weight=4.97; subsample=0.607; colsample_bytree=0.754 : auc.test.mean=0.642
#gini ~= 0.642 * 2 - 1 = 0.284

# train a tunned model

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.01, gamma=0, max_depth=7, 
               min_child_weight=4.97, subsample=0.607, 
               colsample_bytree=0.754)

xg_model = xgb.train(params = params,
                     data = train_x,
                     nrounds = 1000,
                     verbose = 1,
                     save_name = "xgboost_tunned.model")

pred <- predict(xg_model, test_x)
prediction <- data.frame(cbind(test$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction2.csv", row.names = FALSE)
# 0.279

####################################################### 
# feature selection

# view feature importance
importance_matrix <- xgb.importance(feature_names = colnames(train[,c(3:59)]), model = xg_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
### get rid of all those after 
train2 = train[,-c(12,13,15,45:59)]

###### One hot code categorical variables

library(ade4)
library(data.table)
ohe_feats = c("ps_ind_02_cat", "ps_ind_04_cat", "ps_ind_05_cat", 
              "ps_car_01_cat", "ps_car_02_cat", "ps_car_03_cat",
              "ps_car_04_cat", "ps_car_05_cat", "ps_car_07_cat",
              "ps_car_06_cat", "ps_car_08_cat", "ps_car_09_cat",
              "ps_car_10_cat")
train2$ps_ind_02_cat = as.factor(train2$ps_ind_02_cat)
train2$ps_ind_04_cat = as.factor(train2$ps_ind_04_cat)
train2$ps_ind_05_cat = as.factor(train2$ps_ind_05_cat)
train2$ps_car_01_cat = as.factor(train2$ps_car_01_cat)
train2$ps_car_02_cat = as.factor(train2$ps_car_02_cat)
train2$ps_car_03_cat = as.factor(train2$ps_car_03_cat)
train2$ps_car_04_cat = as.factor(train2$ps_car_04_cat)
train2$ps_car_05_cat = as.factor(train2$ps_car_05_cat)
train2$ps_car_06_cat = as.factor(train2$ps_car_06_cat)
train2$ps_car_07_cat = as.factor(train2$ps_car_07_cat)
train2$ps_car_08_cat = as.factor(train2$ps_car_08_cat)
train2$ps_car_09_cat = as.factor(train2$ps_car_09_cat)
train2$ps_car_10_cat = as.factor(train2$ps_car_10_cat)

for (f in ohe_feats){
    train_dummy = acm.disjonctif(train2[f])
    train2[f] = NULL
    train2 = cbind(train2, train_dummy)
}

colnames(train2)
correlation = cor(train2[,c(3:108)], train2$target)

test2 = test[,-c(11,12,14,44:58)]
ohe_feats = c("ps_ind_02_cat", "ps_ind_04_cat", "ps_ind_05_cat", 
              "ps_car_01_cat", "ps_car_02_cat", "ps_car_03_cat",
              "ps_car_04_cat", "ps_car_05_cat", "ps_car_07_cat",
              "ps_car_06_cat", "ps_car_08_cat", "ps_car_09_cat",
              "ps_car_10_cat")
test2$ps_ind_02_cat = as.factor(test2$ps_ind_02_cat)
test2$ps_ind_04_cat = as.factor(test2$ps_ind_04_cat)
test2$ps_ind_05_cat = as.factor(test2$ps_ind_05_cat)
test2$ps_car_01_cat = as.factor(test2$ps_car_01_cat)
test2$ps_car_02_cat = as.factor(test2$ps_car_02_cat)
test2$ps_car_03_cat = as.factor(test2$ps_car_03_cat)
test2$ps_car_04_cat = as.factor(test2$ps_car_04_cat)
test2$ps_car_05_cat = as.factor(test2$ps_car_05_cat)
test2$ps_car_06_cat = as.factor(test2$ps_car_06_cat)
test2$ps_car_07_cat = as.factor(test2$ps_car_07_cat)
test2$ps_car_08_cat = as.factor(test2$ps_car_08_cat)
test2$ps_car_09_cat = as.factor(test2$ps_car_09_cat)
test2$ps_car_10_cat = as.factor(test2$ps_car_10_cat)

for (f in ohe_feats){
    test_dummy = acm.disjonctif(test2[f])
    test2[f] = NULL
    test2 = cbind(test2, test_dummy)
}

test2_x = as.matrix(test2[,c(2:107)])


# try to run cv
train2_x = train2[,c(3:108)]
train2_x = as.matrix(train2_x)
train2_y = as.vector(train2$target)
train2_x = xgb.DMatrix(data = train2_x, label = train2_y)

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.01, gamma=0, max_depth=7, 
               min_child_weight=4.97, subsample=0.607, 
               colsample_bytree=0.754)

xgbcv <- xgb.cv(params = params, 
                data = train2_x,
                nrounds = 1000, nfold = 5, showsd = T, 
                stratified = T, print_every_n = 5, 
                early_stop_round = 20, 
                feval = GiniEvaluation,
                maximize = T)
# test Gini gains much faster than before

# try to train the model
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.01, gamma=0, max_depth=7, 
               min_child_weight=4.97, subsample=0.607, 
               colsample_bytree=0.754)

xg_model = xgb.train(params = params,
                     data = train2_x,
                     nrounds = 1000,
                     verbose = 1,
                     save_name = "xgboost_tunned2.model")
# 56min

pred <- predict(xg_model, test2_x)
prediction <- data.frame(cbind(test$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction3.csv", row.names = FALSE)
# 0.279

#######################################################
# further feature engineering

importance_matrix <- xgb.importance(feature_names = colnames(train2[,c(3:108)]), model = xg_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

# remove those features that do not appear in the 78 features
colnames(train2) %in% importance_matrix$Feature
# 58, 82:108
colnames(train2)[58]
colnames(train2)[82:108]
train2 = train2[, -c(58, 82:108)]
test2 = test2[, -c(57, 81:107)]

# create interaction features among the highly important ones
train2$car13_reg03 = train2$ps_car_13 * train2$ps_reg_03
train2$car13_ind03 = train2$ps_car_13 * train2$ps_ind_03
train2$car13_car14 = train2$ps_car_13 * train2$ps_car_14
train2$car13_ind15 = train2$ps_car_13 * train2$ps_ind_15
train2$reg03_ind03 = train2$ps_reg_03 * train2$ps_ind_03
train2$reg03_car14 = train2$ps_reg_03 * train2$ps_car_14
train2$reg03_ind15 = train2$ps_reg_03 * train2$ps_ind_15
train2$ind03_car14 = train2$ps_ind_03 * train2$ps_car_14
train2$ind03_ind15 = train2$ps_ind_03 * train2$ps_ind_15
train2$car14_ind15 = train2$ps_car_14 * train2$ps_ind_15

test2$car13_reg03 = test2$ps_car_13 * test2$ps_reg_03
test2$car13_ind03 = test2$ps_car_13 * test2$ps_ind_03
test2$car13_car14 = test2$ps_car_13 * test2$ps_car_14
test2$car13_ind15 = test2$ps_car_13 * test2$ps_ind_15
test2$reg03_ind03 = test2$ps_reg_03 * test2$ps_ind_03
test2$reg03_car14 = test2$ps_reg_03 * test2$ps_car_14
test2$reg03_ind15 = test2$ps_reg_03 * test2$ps_ind_15
test2$ind03_car14 = test2$ps_ind_03 * test2$ps_car_14
test2$ind03_ind15 = test2$ps_ind_03 * test2$ps_ind_15
test2$car14_ind15 = test2$ps_car_14 * test2$ps_ind_15

# grid search
#create learner
library(mlr)
lrn <- makeLearner("classif.xgboost", predict.type = "prob")
lrn$par.vals <- list(booster = "gbtree", 
                     objective="binary:logistic", 
                     eval_metric="auc", nrounds=1000L, eta=0.01)
#set parameter space
params <- makeParamSet(makeIntegerParam("max_depth",lower = 5L,upper = 8L),
                       makeIntegerParam("min_child_weight",lower = 4L,upper = 8L),
                       makeNumericParam("subsample",lower = 0.6,upper = 1),
                       makeNumericParam("colsample_bytree",lower = 0.4,upper = 0.8))
#set resampling strategy
rdesc <- makeResampleDesc("CV", stratify=T, iters=5L)
#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)
#create task
train_task_data = train2[,c(2:90)]
colnames(train_task_data) = gsub("\\.", "", colnames(train_task_data))
colnames(train_task_data) = gsub("-", "_", colnames(train_task_data))
traintask <- makeClassifTask(data = train_task_data, target = "target")

#set parallel backend
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores() - 1)

#parameter tuning
Sys.time()
mytune <- tuneParams(learner = lrn, 
                     task = traintask, 
                     resampling = rdesc, 
                     measures = auc, 
                     par.set = params, 
                     control = ctrl, 
                     show.info = T)

Sys.time()
# 12.5hr
# Result: max_depth=5; min_child_weight=5; subsample=0.618; colsample_bytree=0.686 : auc.test.mean=0.64

# train a tunned model
train2_x = train2[,c(3:90)]
train2_x = as.matrix(train2_x)
train2_y = as.vector(train2$target)
train2_x = xgb.DMatrix(data = train2_x, label = train2_y)
test2_x = as.matrix(test2[,c(2:89)])

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.01, gamma=0, max_depth=5, 
               min_child_weight=5, subsample=0.618, 
               colsample_bytree=0.686)

xg_model = xgb.train(params = params,
                     data = train2_x,
                     nrounds = 1000,
                     verbose = 1,
                     save_name = "xgboost_tunned.model")

pred <- predict(xg_model, test2_x)
prediction <- data.frame(cbind(test2$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction6.csv", row.names = FALSE)
# 0.274

#######################################################
# another round (tune max_delta_step)
importance_matrix <- xgb.importance(feature_names = colnames(train2[,c(3:90)]), model = xg_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
colnames(train2) %in% importance_matrix$Feature
train2 = train2[,-c(63:90)]
test2 = test2[-c(63:90)]

#### tune "max_delta_step" (0 to 10)
lrn <- makeLearner("classif.xgboost", predict.type = "prob")
lrn$par.vals <- list(booster = "gbtree", 
                     objective="binary:logistic", 
                     eval_metric="auc", nrounds=1000L, eta=0.01,
                     min_child_weight=5, subsample = 0.6)

#set parameter space
params <- makeParamSet(makeIntegerParam("max_depth",lower = 4L,upper = 8L),
                       makeNumericParam("colsample_bytree",lower = 0.5,upper = 0.8),
                       makeNumericParam("max_delta_step",lower = 0,upper = 10))
#set resampling strategy
rdesc <- makeResampleDesc("CV", stratify=T, iters=5L)
#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)
#create task
train_task_data = train2[,c(2:62)]
colnames(train_task_data) = gsub("\\.", "", colnames(train_task_data))
colnames(train_task_data) = gsub("-", "_", colnames(train_task_data))
traintask <- makeClassifTask(data = train_task_data, target = "target")
#set parallel backend
parallelStartSocket(cpus = detectCores() - 1)

#parameter tuning
Sys.time()
mytune <- tuneParams(learner = lrn, 
                     task = traintask, 
                     resampling = rdesc, 
                     measures = auc, 
                     par.set = params, 
                     control = ctrl, 
                     show.info = T)

Sys.time()

# train a tunned model
train2_x = train2[,c(3:62)]
train2_x = as.matrix(train2_x)
train2_y = as.vector(train2$target)
train2_x = xgb.DMatrix(data = train2_x, label = train2_y)
test2_x = as.matrix(test2[,c(2:61)])

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.01, gamma=0, max_depth=5, 
               min_child_weight=5, subsample=0.618, 
               colsample_bytree=0.686)

xg_model = xgb.train(params = params,
                     data = train2_x,
                     nrounds = 1000,
                     verbose = 1,
                     save_name = "xgboost_tunned.model")

pred <- predict(xg_model, test2_x)
prediction <- data.frame(cbind(test2$id, pred))
colnames(prediction) = c("id", "target")
write.csv(prediction, "prediction.csv", row.names = FALSE)

# 0.275

