setwd("/Users/hzdy1994/Desktop/Kaggle")
load("data/likelihood-coding.RData")
# load("data/one-hot-coding.RData")

library(stringr)
library(dplyr)
library(ggplot2)
library(reshape2)

###########################################
# New variables

# exploration
train_test %>% group_by(ps_ind_01) %>% summarise(avg = sum(ps_car_13) / n())
train_test %>% group_by(ps_ind_05_catnew) %>% summarise(avg = sum(ps_car_13) / n())
train_test %>% group_by(ps_ind_13_bin) %>% summarise(avg = sum(ps_car_13) / n())
train_test %>% group_by(ps_reg_03) %>% summarise(avg = sum(ps_car_13) / n())

# ps_car_13 over the group average on all ps_ind_xx: 
# how much the mileage is deviated from average of the client group

inds = colnames(train_test)[str_detect(colnames(train_test), "ind")]

avg = train_test %>%
    group_by(ps_ind_01) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_01")
train_test$avg_car13_on_ind01 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_02_catnew)) %>%
    group_by(ps_ind_02_catnew) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_02_catnew",
                   all.x = TRUE)
train_test$avg_car13_on_ind02 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_03)) %>%
    group_by(ps_ind_03) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_03",
                   all.x = TRUE)
train_test$avg_car13_on_ind03 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_04_catnew)) %>%
    group_by(ps_ind_04_catnew) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_04_catnew",
                   all.x = TRUE)
train_test$avg_car13_on_ind04 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_05_catnew)) %>%
    group_by(ps_ind_05_catnew) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_05_catnew",
                   all.x = TRUE)
train_test$avg_car13_on_ind05 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_06_bin)) %>%
    group_by(ps_ind_06_bin) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_06_bin",
                   all.x = TRUE)
train_test$avg_car13_on_ind06 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_07_bin)) %>%
    group_by(ps_ind_07_bin) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_07_bin",
                   all.x = TRUE)
train_test$avg_car13_on_ind07 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_08_bin)) %>%
    group_by(ps_ind_08_bin) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_08_bin",
                   all.x = TRUE)
train_test$avg_car13_on_ind08 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_09_bin)) %>%
    group_by(ps_ind_09_bin) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_09_bin",
                   all.x = TRUE)
train_test$avg_car13_on_ind09 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_10_bin)) %>%
    group_by(ps_ind_10_bin) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_10_bin",
                   all.x = TRUE)
train_test$avg_car13_on_ind10 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_11_bin)) %>%
    group_by(ps_ind_11_bin) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_11_bin",
                   all.x = TRUE)
train_test$avg_car13_on_ind11 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_12_bin)) %>%
    group_by(ps_ind_12_bin) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_12_bin",
                   all.x = TRUE)
train_test$avg_car13_on_ind12 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_13_bin)) %>%
    group_by(ps_ind_13_bin) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_13_bin",
                   all.x = TRUE)
train_test$avg_car13_on_ind13 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_14)) %>%
    group_by(ps_ind_14) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_14",
                   all.x = TRUE)
train_test$avg_car13_on_ind14 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_15)) %>%
    group_by(ps_ind_15) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_15",
                   all.x = TRUE)
train_test$avg_car13_on_ind15 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_16_bin)) %>%
    group_by(ps_ind_16_bin) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_16_bin",
                   all.x = TRUE)
train_test$avg_car13_on_ind16 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_17_bin)) %>%
    group_by(ps_ind_17_bin) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_17_bin",
                   all.x = TRUE)
train_test$avg_car13_on_ind17 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_ind_18_bin)) %>%
    group_by(ps_ind_18_bin) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_ind_18_bin",
                   all.x = TRUE)
train_test$avg_car13_on_ind18 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

# ps_car_13 over the group average on all ps_reg_01 and ps_reg_02: 
# how much the mileage is deviated from average of the region group

avg = train_test %>%
    filter(!is.na(ps_reg_01)) %>%
    group_by(ps_reg_01) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_reg_01",
                   all.x = TRUE)
train_test$avg_car13_on_reg01 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_reg_02)) %>%
    group_by(ps_reg_02) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_reg_02",
                   all.x = TRUE)
train_test$avg_car13_on_reg02 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

# ps_car_13 over the group average on ps_car_11_cat: 
# how much the mileage is deviated from average of the region group
avg = train_test %>%
    filter(!is.na(ps_car_11_catnew)) %>%
    group_by(ps_car_11_catnew) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_car_11_catnew",
                   all.x = TRUE)
train_test$avg_car13_on_car11 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

# adjust the column orders
train_test = train_test[, c(22:24, 1:21, 25:81)]

# save.image("~/Desktop/Kaggle/data/new_feature_added.RData")

###########################################
# feature selection - avoid the great variance caused by multicollinearity

# remove all the calc features
train_test = train_test[,-(31:50)]

cormat = round(cor(train_test[,c(3:61)]),2)
head(cormat)

melted_cormat = melt(cormat)
head(melted_cormat)

# correlation heatmap
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()

melted_cormat = melted_cormat %>%
    filter(Var1 != Var2) %>%
    arrange(-value)

train_test$ps_ind_12_bin = NULL

train_test$avg_car13_on_ind08 = NULL
train_test$avg_car13_on_ind09 = NULL
train_test$avg_car13_on_ind10 = NULL
train_test$avg_car13_on_ind11 = NULL
train_test$avg_car13_on_ind12 = NULL
train_test$avg_car13_on_ind13 = NULL
train_test$avg_car13_on_ind14 = NULL
train_test$avg_car13_on_ind15 = NULL
train_test$avg_car13_on_ind16 = NULL
train_test$avg_car13_on_ind17 = NULL
train_test$avg_car13_on_ind18 = NULL
train_test$avg_car13_on_reg01 = NULL

train_test$avg_car13_on_ind01 = NULL
train_test$avg_car13_on_ind03 = NULL
train_test$avg_car13_on_reg02 = NULL
train_test$avg_car13_on_ind06 = NULL
train_test$avg_car13_on_ind07 = NULL


cormat = round(cor(train_test[,c(3:43)]),2)
melted_cormat = melt(cormat)
melted_cormat = melted_cormat %>%
    filter(Var1 != Var2) %>%
    arrange(-value)

save.image("~/Desktop/Kaggle/data/new_feature_no_corr.RData")

###########################################
# exclude those seems useless for lightgbm
# add new features grouped on car features

train_test$ps_ind_10_bin = NULL
train_test$ps_ind_11_bin = NULL
train_test$ps_ind_13_bin = NULL


# ps_car_13 / ps_car_15: mileage / age
train_test$car13_car15 = train_test$ps_car_13 / train_test$ps_car_15

# ps_car_14 / ps_car_13: previous claims / mileage
train_test$car14_car13 = train_test$ps_car_14 / train_test$ps_car_13

# ps_car_13 over the group average on all ps_car_xx_cat: 
# how much the mileage is deviated from average of the car

avg = train_test %>%
    filter(!is.na(ps_car_01_catnew)) %>%
    group_by(ps_car_01_catnew) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_car_01_catnew",
                   all.x = TRUE)
train_test$avg_car13_on_car01 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_car_02_catnew)) %>%
    group_by(ps_car_02_catnew) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_car_02_catnew",
                   all.x = TRUE)
train_test$avg_car13_on_car02 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_car_04_catnew)) %>%
    group_by(ps_car_04_catnew) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_car_04_catnew",
                   all.x = TRUE)
train_test$avg_car13_on_car04 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_car_06_catnew)) %>%
    group_by(ps_car_06_catnew) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_car_06_catnew",
                   all.x = TRUE)
train_test$avg_car13_on_car06 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_car_07_catnew)) %>%
    group_by(ps_car_07_catnew) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_car_07_catnew",
                   all.x = TRUE)
train_test$avg_car13_on_car07 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_car_08_catnew)) %>%
    group_by(ps_car_08_catnew) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_car_08_catnew",
                   all.x = TRUE)
train_test$avg_car13_on_car08 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_car_09_catnew)) %>%
    group_by(ps_car_09_catnew) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_car_09_catnew",
                   all.x = TRUE)
train_test$avg_car13_on_car09 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL

avg = train_test %>%
    filter(!is.na(ps_car_10_catnew)) %>%
    group_by(ps_car_10_catnew) %>%
    summarise(avg = sum(ps_car_13) / n())
train_test = merge(train_test, avg,
                   by = "ps_car_10_catnew",
                   all.x = TRUE)
train_test$avg_car13_on_car10 = train_test$ps_car_13 / train_test$avg
train_test$avg = NULL


# check correlations
train_test = train_test[,c(9:11, 1:8, 12:50)]
colnames(train_test)
summary(train_test[,41:50])

cormat = round(cor(train_test[,c(3:50)]),2)
melted_cormat = melt(cormat)
melted_cormat = melted_cormat %>%
    filter(Var1 != Var2) %>%
    arrange(-value)

train_test$avg_car13_on_car10 = NULL
train_test$car14_car13 = NULL
train_test$avg_car13_on_car08 = NULL
train_test$avg_car13_on_car06 = NULL

cormat = round(cor(train_test[,c(3:46)]),2)
melted_cormat = melt(cormat)
melted_cormat = melted_cormat %>%
    filter(Var1 != Var2) %>%
    arrange(-value)

save.image("~/Desktop/Kaggle/data/new_feature_no_corr2.RData")


###########################################
# All features have been created + impact encoding: "impact_encoding_ver.RData"
# All features have been created + one-hot-encoding: "one_hot_encoding_ver.RData"


