setwd("/Users/hzdy1994/Desktop/Kaggle")

kernel_1 = read.csv("kernel/dnn_0.268.csv")
kernel_2 = read.csv("kernel/gpx_0.283.csv")
kernel_3 = read.csv("kernel/Kinetics_0.282.csv")
kernel_4 = read.csv("kernel/xgb_0.283.csv")
kernel_5 = read.csv("kernel/xgb_0.284.csv")
kernel_6 = read.csv("kernel/xgb_0.284_2.csv")
kernel_7 = read.csv("kernel/cat_0.281.csv")
kernel_8 = read.csv("kernel/lgbm_0.283.csv")

ensembled = cbind(kernel_1, kernel_2, kernel_3, kernel_4, kernel_5, kernel_6, kernel_7, kernel_8)
ensembled = ensembled[,c(2,4,6,8,10,12,14,16)]

data.rank<-as.data.frame(sapply(ensembled,rank))
data.rank = data.rank / nrow(data.rank)
data.rank$pred = apply(data.rank,1,mean)

output = cbind(kernel_1$id, data.rank$pred)
colnames(output) = c("id", "target")

write.csv(output, 'mean_rank.csv', row.names = FALSE)

#####
ensembled = cbind(kernel_1, kernel_2, kernel_4, kernel_5, kernel_6, kernel_8)
ensembled = ensembled[,c(2,4,6,8,10,12)]

data.rank<-as.data.frame(sapply(ensembled,rank))
data.rank = data.rank / nrow(data.rank)
data.rank$pred = apply(data.rank,1,mean)

output = cbind(kernel_1$id, data.rank$pred)
colnames(output) = c("id", "target")

write.csv(output, 'mean_rank2.csv', row.names = FALSE)
