#############################################################
# one hot code of categorical variables
setwd("/Users/hzdy1994/Desktop/Kaggle")

train = read.csv("data/train.csv")
test = read.csv("data/test.csv")
# or other pre-processed datasets

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
# gini functions

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

#############################################################
# grid search tuning

cv_tunning = data.frame(num_leaves = numeric(0), 
                        max_depth = numeric(0),
                        min_data_in_leaf = numeric(0),
                        best_itr = numeric(0), 
                        best_gini = numeric(0))

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