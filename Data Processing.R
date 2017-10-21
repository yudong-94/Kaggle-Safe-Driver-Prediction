setwd("/Users/hzdy1994/Desktop/Kaggle")

train = read.csv("data/train.csv")
test = read.csv("data/test.csv")

library(stringr)
library(dplyr)

test$target = 0
test$data = "test"
test = test[, c(1, 60, 59, 2:58)]
train$data = "train"
train = train[, c(1, 60, 2:59)]
    
train_test = rbind(train, test)

# transfer those -1 to NA
categoricals = c("ps_ind_02_cat", "ps_ind_04_cat", "ps_ind_05_cat", 
                 "ps_car_01_cat", "ps_car_02_cat", "ps_car_03_cat",
                 "ps_car_04_cat", "ps_car_05_cat", "ps_car_07_cat",
                 "ps_car_06_cat", "ps_car_08_cat", "ps_car_09_cat",
                 "ps_car_10_cat", "ps_car_11_cat")

for (f in categoricals){
    train_test[train_test[f] == -1, f] = NA
}

rm(f, categoricals)

# save.image("~/Desktop/Kaggle/data/train&test.RData")

###########################################
# One-hot-Coding
categoricals = c("ps_ind_02_cat", "ps_ind_04_cat", "ps_ind_05_cat", 
                 "ps_car_01_cat", "ps_car_02_cat", "ps_car_03_cat",
                 "ps_car_04_cat", "ps_car_05_cat", "ps_car_07_cat",
                 "ps_car_06_cat", "ps_car_08_cat", "ps_car_09_cat",
                 "ps_car_10_cat")

train_test$ps_ind_02_cat = as.factor(train_test$ps_ind_02_cat)
train_test$ps_ind_04_cat = as.factor(train_test$ps_ind_04_cat)
train_test$ps_ind_05_cat = as.factor(train_test$ps_ind_05_cat)
train_test$ps_car_01_cat = as.factor(train_test$ps_car_01_cat)
train_test$ps_car_02_cat = as.factor(train_test$ps_car_02_cat)
train_test$ps_car_03_cat = as.factor(train_test$ps_car_03_cat)
train_test$ps_car_04_cat = as.factor(train_test$ps_car_04_cat)
train_test$ps_car_05_cat = as.factor(train_test$ps_car_05_cat)
train_test$ps_car_06_cat = as.factor(train_test$ps_car_06_cat)
train_test$ps_car_07_cat = as.factor(train_test$ps_car_07_cat)
train_test$ps_car_08_cat = as.factor(train_test$ps_car_08_cat)
train_test$ps_car_09_cat = as.factor(train_test$ps_car_09_cat)
train_test$ps_car_10_cat = as.factor(train_test$ps_car_10_cat)

library(ade4)
library(data.table)

for (f in categoricals){
    dummy = acm.disjonctif(train_test[f])
    train_test[f] = NULL
    train_test = cbind(train_test, dummy)
}

rm(dummy, f, categoricals)

colnames(train_test) = gsub("\\.", "", colnames(train_test))
colnames(train_test)

# save.image("~/Desktop/Kaggle/data/one-hot-coding.RData")

###########################################
# Likelihood Coding

categoricals = c("ps_ind_02_cat", "ps_ind_04_cat", "ps_ind_05_cat", 
                 "ps_car_01_cat", "ps_car_02_cat", "ps_car_03_cat",
                 "ps_car_04_cat", "ps_car_05_cat", "ps_car_07_cat",
                 "ps_car_06_cat", "ps_car_08_cat", "ps_car_09_cat",
                 "ps_car_10_cat", "ps_car_11_cat")

for (f in categoricals){
    
    newname = paste0(f,"new")
    train_test[newname] = NA
    
    for (i in unique(train_test[f][,1])) {
        if (!is.na(i)) {
            perc = sum(train[train[f] == i, ]$target) / nrow(train[train[f] == i, ])
            train_test[train_test[f] == i & !is.na(train_test[f]), newname] = perc
        }
    }
    
    train_test[f] = NULL
}

rm(i, f, categoricals, newname, perc)

# save.image("~/Desktop/Kaggle/data/likelihood-coding.RData")

