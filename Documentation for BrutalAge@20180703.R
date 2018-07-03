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

#https://datascienceplus.com/extreme-gradient-boosting-with-r/
#https://www.kaggle.com/jiashenliu/updated-xgboost-with-parameter-tuning

# new start

library(tidyverse)
library(Matrix)
library(data.table)
setwd("D:/Projects/BrutalAge/data/")
train <- read_csv("tap_fun_train.csv")
test <- read_csv("tap_fun_test.csv")

library(plyr) 
pt <- ddply(train, .(prediction_pay_price), summarize, count = length(user_id))
pt[order(pt$count,decreasing = T),]

train <- train[,-110]
train$cls <- as.factor(ifelse(train$prediction_pay_price==0,"pre_free","pre_paid"))
# train$pattern <- ifelse(train$prediction_pay_price==0,"user_free",
#                         ifelse(train$prediction_pay_price==0.99,"user_0.99",
#                                ifelse(train$prediction_pay_price==1.98,"user_1.98","paid")))
# prop.table(table(train$pattern))
table(train$cls)
prop.table(table(train$cls))

###combination
rm(df)
df <- train[,c(109:110)]

df$wood <- train$wood_add_value + train$wood_reduce_value
df$stone <- train$stone_add_value + train$stone_reduce_value
df$ivory <- train$ivory_add_value + train$ivory_reduce_value
df$meat <- train$meat_add_value + train$meat_reduce_value
df$magic <- train$magic_add_value + train$magic_reduce_value
df$infantry <- train$infantry_add_value + train$infantry_reduce_value + train$wound_infantry_add_value + train$wound_infantry_reduce_value
df$cavalry <- train$cavalry_add_value + train$cavalry_reduce_value + train$wound_cavalry_add_value + train$wound_cavalry_reduce_value
df$shaman <- train$shaman_add_value + train$shaman_reduce_value + train$wound_shaman_add_value + train$wound_shaman_reduce_value
#acc
df$gen_acc <- train$general_acceleration_add_value + train$general_acceleration_reduce_value
df$bld_acc <- train$building_acceleration_add_value + train$building_acceleration_reduce_value
df$rsh_acc <- train$reaserch_acceleration_add_value + train$reaserch_acceleration_reduce_value
df$train_acc <- train$training_acceleration_add_value + train$training_acceleration_reduce_value
df$treat_acc <- train$treatment_acceleraion_add_value + train$treatment_acceleration_reduce_value
#level
df$bdlevel <- apply(train[,35:50],1,sum)
df$sclevel <- apply(train[,51:99],1,sum)
#atk
df$pvp <- train$pvp_battle_count
df$pve <- train$pve_battle_count

#other
df$avg_online_minutes <- train$avg_online_minutes
df$pay_price <- train$pay_price
df$pay_count <- train$pay_count

# library(ROSE)
# train_set <- train[,c(3:108,110)]
# train_bal <- ROSE(cls ~ . ,data = train_set,seed = 42)$data

TRN <- as.data.frame(cbind(scale(df[,c(3:22)]),df$prediction_pay_price))
colnames(TRN)[21] <- c("prediction_pay_price")
fit <- lm(prediction_pay_price~.,data=TRN)
summary(fit)
library(Metrics)
rmse(actual = TRN$prediction_pay_price,predicted = fit$fitted.values)

library(caret)
train_control <- trainControl(method="cv", number=10)
model <- train(prediction_pay_price~., data=TRN, trControl=train_control, method="lm")





