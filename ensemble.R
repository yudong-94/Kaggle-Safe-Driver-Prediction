setwd("/Users/hzdy1994/Desktop/Kaggle")

lightgbm_1 = read.csv("data/lightgbm1.csv")
lightgbm_2 = read.csv("data/lightgbm2.csv")
lightgbm_3 = read.csv("data/lightgbm3.csv")
lightgbm_4 = read.csv("data/lightgbm4.csv")
xgboost_1 = read.csv("data/xgboost1.csv")
xgboost_2 = read.csv("data/xgboost2.csv")

ensembled = cbind(lightgbm_1, lightgbm_2, lightgbm_3, lightgbm_4, xgboost_1, xgboost_2)
ensembled = ensembled[,c(1,2,4,6,8,10, 12)]
colnames(ensembled) = c("id", "lightgbm_1", "lightgbm_2", "lightgbm_3", "lightgbm_4", "xgboost_1", "xgboost_2")
summary(ensembled)

# set target as average
ensembled$target = (ensembled$lightgbm_1 + ensembled$lightgbm_2 + ensembled$lightgbm_3 + ensembled$lightgbm_4 + ensembled$xgboost_1 + ensembled$xgboost_2)/6
write.csv(ensembled[,c(1,8)], "prediction.csv", row.names = FALSE)
# 0.282 (ensembling 6 models didn't improve the model anymore)

# set target as maximum
# ensembled[, "target"] = apply(ensembled[, 2:6], 1, max)
# write.csv(ensembled[,c(1,7)], "prediction.csv", row.names = FALSE)
# 0.282 -> 0.281

# set target as Harmonic mean
ensembled$target = 1/((1/ensembled$lightgbm_1 + 1/ensembled$lightgbm_2 + 1/ensembled$lightgbm_3 + 1/ensembled$lightgbm_4 + 1/ensembled$xgboost_1 + 1/ensembled$xgboost_2)/6)
write.csv(ensembled[,c(1,8)], "prediction.csv", row.names = FALSE)
# 0.282 -> 0.282 (a little bit lower)

# run a logistic regression to ensemble

