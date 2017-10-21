setwd("/Users/hzdy1994/Desktop/Kaggle")
load("data/likelihood-coding.RData")

library(stringr)
library(dplyr)

###########################################
# New variables

# exploration
train_test %>% group_by(ps_ind_01) %>% summarise(avg = sum(ps_car_13) / n())
train_test %>% group_by(ps_ind_05_catnew) %>% summarise(avg = sum(ps_car_13) / n())
train_test %>% group_by(ps_ind_13_bin) %>% summarise(avg = sum(ps_car_13) / n())
train_test %>% group_by(ps_reg_03) %>% summarise(avg = sum(ps_car_13) / n())


# ps_car_13 / ps_car_15: mileage / age
train_test$car13_car15 = train_test$ps_car_13 / train_test$ps_car_15

# ps_car_14 / ps_car_13: previous claims / mileage
train_test$car14_car13 = train_test$ps_car_14 / train_test$ps_car_13

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
library(fmsb)

vif_func<-function(in_frame,thresh=10,trace=T,...){

    
    if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
    
    #get initial vif value for all comparisons of variables
    vif_init<-NULL
    var_names <- names(in_frame)
    for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
    }
    vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
    
    if(vif_max < thresh){
        if(trace==T){ #print output of each iteration
            prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
            cat('\n')
            cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
        }
        return(var_names)
    }
    else{
        
        in_dat<-in_frame
        
        #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
        while(vif_max >= thresh){
            
            vif_vals<-NULL
            var_names <- names(in_dat)
            
            for(val in var_names){
                regressors <- var_names[-which(var_names == val)]
                form <- paste(regressors, collapse = '+')
                form_in <- formula(paste(val, '~', form))
                vif_add<-VIF(lm(form_in, data = in_dat, ...))
                vif_vals<-rbind(vif_vals,c(val,vif_add))
            }
            max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
            
            vif_max<-as.numeric(vif_vals[max_row,2])
            
            if(vif_max<thresh) break
            
            if(trace==T){ #print output of each iteration
                prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
                cat('\n')
                cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
                flush.console()
            }
            
            in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
            
        }
        
        return(names(in_dat))
        
    }
    
}

col_vif = vif_func(in_frame = train_test[,c(4:61)])
