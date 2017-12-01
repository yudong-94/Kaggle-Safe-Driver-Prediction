# Kaggle Safe Driver Prediction
This repo comtains my work for the Kaggle Competition "Porto Seguro’s Safe Driver Prediction"  
Link to the competition: https://www.kaggle.com/c/porto-seguro-safe-driver-prediction  

Final achievement (teamwork with Yuvi Tu @NYU): Top 15%, Normalized Gini Coefficient 0.28986 (1st place 0.29698)

## Summmary
Porto Seguro is one of Brazil’s largest auto and homeowner insurance companies. Inaccuracies in car insurance company’s claim predictions raise the cost of insurance for good drivers and reduce the price for bad ones. The objective of this competition is to predict the probability that a driver will initiate an auto insurance claim in the next year, given the features of individuals, cars, regions, etc.

## Documentation

### 1st Part: Data Processing
The data given by Porto Seguro is anonymous, with 595,212 training observations and 892,816 testing observations. There are 59 variables recorded in [features.xlsx](https://github.com/yudong-94/Kaggle-Safe-Driver-Prediction/tree/master/tracking).  
For Exploratory Data Analysis, please refer to the terrific public kernel: [Steering Wheel of Fortune - Porto Seguro EDA
](https://www.kaggle.com/headsortails/steering-wheel-of-fortune-porto-seguro-eda).  
I did basic data pre-processing and one-hot-encoding and target encoding in [Data Porcessing.R](https://github.com/yudong-94/Kaggle-Safe-Driver-Prediction/blob/master/feature%20engineering/Data%20Processing.R). For all the feature engineering work, you can find them in [feature engineering.R](https://github.com/yudong-94/Kaggle-Safe-Driver-Prediction/blob/master/feature%20engineering/feature%20engineering.R). I have also tried [SVD](https://github.com/yudong-94/Kaggle-Safe-Driver-Prediction/blob/master/feature%20engineering/SVD.R) to detect whethere ther eis obvious clusters exist. Unfortunately, this does not seem to work here.

### 2nd Part: Modeling & Prediction
This problem is essentially a binary classification problem. For the modeling part, I tried XGBoost model and LightGBM model. The performance of both models are similar, but since LightGBM is much faster, and I only run on my local machine, I selected features and tunned parameters mostly on the LightGBM model. The codes could be found in the [modeling](https://github.com/yudong-94/Kaggle-Safe-Driver-Prediction/tree/master/modeling) folder.

### 3rd Part: Ensembling
For this dataset, it seems that the model ensembling method that works best is actually take the average or median of the ranks, since we are competing against normalized Gini, which is only dependent on the ranks of the predictions. I made some naive trials in the [ensembling](https://github.com/yudong-94/Kaggle-Safe-Driver-Prediction/tree/master/ensembling) folder.

## Takeaways
1. A trustworthy local CV schema is really really important.
2. There isn't "one rule fit for all". No matter for data processing, feature engineering, parameter tunning, model stacking, etc. Just try around and see.
3. It's hard to judge whether you are overfitting sometimes... Many are expecting some kind of great overfitting on some public kernels, but actually they are not. I guess it could be due to the extremely imbalanced data we get, or this kind of ensembling just not easily leads to overfitting.
