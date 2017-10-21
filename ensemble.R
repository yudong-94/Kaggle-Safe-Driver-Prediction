setwd("/Users/hzdy1994/Desktop/Kaggle")

lightgbm_1 = read.csv("data/lightgbm1.csv")
lightgbm_2 = read.csv("data/lightgbm2.csv")
xgboost_1 = read.csv("data/xgboost1.csv")
xgboost_2 = read.csv("data/xgboost2.csv")

ensembled = cbind(lightgbm_1, lightgbm_2, xgboost_1, xgboost_2)
ensembled = ensembled[,c(1,2,4,6,8)]
colnames(ensembled) = c("id", "lightgbm_1", "lightgbm_2", "xgboost_1", "xgboost_2")
summary(ensembled)
ensembled$target = (ensembled$lightgbm_1 + ensembled$lightgbm_2 + ensembled$xgboost_1 + ensembled$xgboost_2)/4

write.csv(ensembled[,c(1,6)], "prediction.csv", row.names = FALSE)

# 0.282