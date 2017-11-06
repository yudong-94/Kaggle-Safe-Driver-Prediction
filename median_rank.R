setwd("/Users/hzdy1994/Desktop/Kaggle")

lightgbm_1 = read.csv("data/lightgbm1.csv")
lightgbm_2 = read.csv("data/lightgbm2.csv")
lightgbm_3 = read.csv("data/lightgbm3.csv")
lightgbm_4 = read.csv("data/lightgbm4.csv")
lightgbm_5 = read.csv("data/lightgbm5.csv")
xgboost_1 = read.csv("data/xgboost1.csv")
xgboost_2 = read.csv("data/xgboost2.csv")
kernel = read.csv("data/kernel_0.285.csv")

# Bind all the target columns together
ensembled = cbind(lightgbm_1, lightgbm_2, lightgbm_3, lightgbm_4, lightgbm_5, xgboost_1, xgboost_2)
ensembled = ensembled[,c(1,2,4,6,8,10,12,14)]
colnames(ensembled) = c("id", "lightgbm_1", "lightgbm_2", "lightgbm_3", "lightgbm_4", "lightgbm_5", "xgboost_1", "xgboost_2")


##Calculate Median Rank and Create Submission File

# Create rank data
data.rank<-as.data.frame(sapply(ensembled,rank))
median.rank<-apply(data.rank,1,median)

#Write median rank CSV
id = ensembled$id
df.median.rank<-data.frame(id=id,target=median.rank/length(median.rank))
# 0.281

# blind with the 0.285 kernel 
# (https://www.kaggle.com/areeves87/aggregate-20-kernel-csvs-by-median-rank-lb-285)
final = cbind(df.median.rank, kernel$target)
final$target = ifelse(final$target > final$`kernel$target`, final$target, final$`kernel$target`)
final = final[,c(1,2)]
# 0.284

write.csv(final, 'median_rank_submission2.csv', row.names = FALSE)


###################################
# ensemble more kernels
kernel_1 = read.csv("kernel/dnn_0.268.csv")
kernel_2 = read.csv("kernel/gpx_0.283.csv")
kernel_3 = read.csv("kernel/Kinetics_0.282.csv")
kernel_4 = read.csv("kernel/xgb_0.283.csv")
kernel_5 = read.csv("kernel/xgb_0.284.csv")
kernel_6 = read.csv("kernel/xgb_0.284_2.csv")

ensembled = cbind(ensembled[,c(1,4:8)], kernel_1, kernel_2, kernel_3, kernel_4, kernel_5, kernel_6)
ensembled = ensembled[,c(1:6,8,10,12,14,16,18)]

data.rank<-as.data.frame(sapply(ensembled,rank))
median.rank<-apply(data.rank,1,median)
id = ensembled$id
df.median.rank<-data.frame(id=id,target=median.rank/length(median.rank))


write.csv(df.median.rank, 'median_rank_submission3.csv', row.names = FALSE)
