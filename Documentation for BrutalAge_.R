#Step One → classification
# 1. read data
library(tidyverse)
library(Matrix)
library(data.table)
setwd("D:/Projects/BrutalAge/data/")
train <- read_csv("tap_fun_train.csv")
#creat some other features for later use
train$lifetime <- as.numeric(as.Date(max(train$register_time))-as.Date(train$register_time))
train$cls <- as.factor(ifelse(train$prediction_pay_price==0,"pre_free","pre_paid"))
train$cat <- as.factor(ifelse(train$pay_price==0,'free','paid'))

# 2. split train set using stratified sampling 

library(sampling) 
free_size <- round(length(train[train$cat=="free",]$user_id)*0.8)
paid_size <- round(length(train[train$cat=="paid",]$user_id)*0.8)
train_sample <- strata(train,stratanames="cat",size = c(free_size,paid_size), method="srswor") 
train <- train[,-c(1,2,109,112)]
#train<-subset(train,select=-cat)
train_set <- train[train_sample$ID_unit,]
test_set <- train[-train_sample$ID_unit,]
rm(train_sample)

# 3. correlation
#cor(train[,107:109])


# 4. feature engineering 
# 4.1. handling imbalance
library(ROSE)
train_bal <- ROSE(cls ~ . ,data = train_set,seed = 42)$data
# 4.2.  scaling
train_set <- cbind(scale(train_set[,1:107]),train_set[,108]) #scaling
# 4.3.  VI & feature selection
library(Boruta)


# 5. select model

# 5.1. Logistic (omitted)
# 5.2. GBM
library(caret)
# 5.3. KNN
library(caret)
# 5.4. xgboost
library(xgboost)

sparse_matrix <- sparse.model.matrix(cls ~ ., data = train_set)[,-1]
output_vector = train_set[,"cls"] == "pre_paid"
dtrain <- xgb.DMatrix(data = sparse_matrix, label = output_vector)

bst <- xgboost(data = dtrain, max.depth = 4, eta = 1, nthread = 2, max_delta_step=10,
               nround = 5, objective = "binary:logistic", verbose = 2,eval_metric="auc")

importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst)
importance[1:10,]


#cross validation
# nround <- 5
# param <- list(max_depth=4, eta=1, silent=1, nthread=2, objective='binary:logistic')
# xgb.cv(param, dtrain, nround, nfold=5, metrics={'auc'})


# error binary classification error rate
# rmse Rooted mean square error
# logloss negative log-likelihood function
# auc Area under curve
# aucpr Area under PR curve
# merror Exact matching error, used to evaluate multi-class classification



pred <- predict(bst, sparse_matrix_test)
prediction <- as.numeric(pred > 0.5)

table(output_vector_test,prediction)
mean(prediction == output_vector_test)
library(ROSE)
roc.curve(output_vector_test, prediction)

# 6. make prediction
sparse_matrix_test <- sparse.model.matrix(cls ~ ., data = test_set)[,-1]
output_vector_test = test_set[,"cls"] == "1"
dtest <- xgb.DMatrix(data = sparse_matrix_test, label = output_vector_test)



# 7. Evaluate


# 8. Fintune
# 8.1. Choosing important variable
#     1:          pay_price 0.949601971 0.253349934 0.16666667
#     2: avg_online_minutes 0.020115557 0.241627501 0.14285714
#     3:     wood_add_value 0.015137221 0.010058042 0.04761905
#     4:  wood_reduce_value 0.004325745 0.067092800 0.07142857
#     5:    ivory_add_value 0.003099065 0.181963277 0.09523810
#     6: stone_reduce_value 0.001505463 0.003612211 0.07142857



# 8.2.  using Grid Search find best param for xgboost
#https://www.rdocumentation.org/packages/NMOF/versions/1.4-1/topics/gridSearch
#https://www.r-bloggers.com/grid-search-in-the-tidyverse/

#******************************************************************************************#

#Step Two → regression

train$cls <- as.factor(ifelse(train$prediction_pay_price==0,"pre_free","pre_paid"))
train_set <- train[,c(3:34,106:107,109:110)]
label_linear <- train_set$prediction_pay_price
train_set <- train_set[,-c(35)]

library(xgboost)
sparse_matrix <- sparse.model.matrix(cls ~ ., data = train_set)[,-1]
output_vector = train_set[,"cls"] == "pre_paid"
dtrain <- xgb.DMatrix(data = sparse_matrix, label = output_vector)

bst_lgt <- xgboost(data = dtrain, max.depth = 5, eta = 1, nthread = 2, max_delta_step=10,
               nround = 5, objective = "binary:logistic", verbose = 2,eval_metric="auc")

importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst_lgt)
importance[1:10,]

pred <- predict(bst_lgt, sparse_matrix)
prediction <- as.numeric(pred > 0.5)
submit_cls <- train[pred<0.5,1]
submit_cls$prediction_pay_price <- 0

# regression
train_set_for_linear <- train[pred>=0.5,c(3:34,106:109)]
train_set_for_linear <- cbind(as.data.frame(scale(train_set_for_linear[,1:35])),train_set_for_linear$prediction_pay_price)
colnames(train_set_for_linear)[36] <- c("prediction_pay_price")
sparse_matrix_linear <- sparse.model.matrix(prediction_pay_price ~ ., data = train_set_for_linear)
output_vector_linear <- train_set_for_linear$prediction_pay_price
dtrain_linear <- xgb.DMatrix(data = sparse_matrix_linear, label = output_vector_linear)
bst_linear <- xgboost(data = dtrain_linear, max.depth = 2, eta = 1, nthread = 2, 
                   nround = 5, objective = "reg:linear", verbose = 2,eval_metric="rmse")


param <- list(max_depth=2, eta=1, silent=1, nthread=2, objective='reg:linear')
xgb.cv(param, dtrain_linear, nround=5, nfold=5, metrics={'rmse'})
#https://datascienceplus.com/extreme-gradient-boosting-with-r/
#https://www.kaggle.com/jiashenliu/updated-xgboost-with-parameter-tuning

