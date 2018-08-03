rm(list=("id_paid"))

#▊1. read data
library(tidyverse)
library(Matrix)
library(data.table)
setwd("D:/Projects/BrutalAge/data/")
train <- read_csv("tap_fun_train.csv")
#train$cls <- as.factor(ifelse(train$pay_price==train$prediction_pay_price,'same','diff'))
train$cls <- as.factor(ifelse(train$pay_price==0,'free','paid'))

#▊2. feat. engineering
#▊2.1. combination
df<- train[,c(109,106:108)]
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
# df$bdlevel <- apply(train[,35:50],1,sum)
# df$sclevel <- apply(train[,51:99],1,sum)
#atk
df$pvp <- train$pvp_battle_count
df$pve <- train$pve_battle_count
#▊2.2. selection
#"prediction_pay_price","avg_online_minutes","pay_price","pay_count"
df <- train[,-c(1:2,110,35:99)]

#▊3. missing value
df <- data.frame(df)
any(is.na(df))

#▊4. skewness/distribution/nzv
library(e1071)
skew_info <- apply(df,2,skewness)
head(sort(skew_info,decreasing = T))

library(lattice)
id_paid <- train$pay_price!=0
id_diff <- train$pay_price!=train$prediction_pay_price
histogram(~df$prediction_pay_price)

library(caret)
nzv_info <- nearZeroVar(df,saveMetrics = T)

#YeoJohnson transformation(apply or not)
id_paid <- trn$prediction_pay_price!=0
histogram(~log10(trn$prediction_pay_price[id_paid]))

#▊5. correlation
library(corrplot)
corrplot(cor(df_flt),type = "lower",tl.cex = 0.5,order="hclust")
cor_info <- findCorrelation(cor(df),cutoff = 0.9)
df_flt <- df[,-cor_info]
library(ggcorrplot)
ggcorrplot(round(cor(df_flt),2),hc.order = TRUE,type = "lower",lab = TRUE,lab_size = 2,tl.cex = 8)

#▊6. split
df_sc <- cbind(scale(df_flt[,-20]),df_flt$prediction_pay_price)
colnames(df_sc)[20] <- c("prediction_pay_price")

trn_id <- createDataPartition(df_sc$prediction_pay_price,p=0.8,list=F)
trn <- df_sc[trn_id,]
vld <- df_sc[-trn_id,]

#▊7. try some "dirty" models
#7.1 olm
fit_lm <- lm(prediction_pay_price~.,data=trn)
#"pvp_win_count" is not statistically significant
RMSE(pred = fit_lm$fitted.values,obs = trn$prediction_pay_price)
#RMSE 57.56035 (negative in pred)

#7.2 rlm
library(MASS)
fit_rlm <- rlm(prediction_pay_price~.,data=trn)
RMSE(pred = fit_rlm$fitted.values,obs = trn$prediction_pay_price)
#RMSE 62.42144 (negative in pred)

#7.3 pls
library(pls)
fit_pls <- plsr(prediction_pay_price~.,data=trn)
RMSE(pred = fit_pls$fitted.values,obs = trn$prediction_pay_price)
#RMSE 62.62439 (negative in pred)

#7.4 ridge
library(elasticnet)
fit_ridge <- enet(x=as.matrix(trn[,-20]),y=trn[,20],lambda=0.001)
pred_ridge <- predict(fit_ridge,newx = as.matrix(trn[,-20]),s=1,mode="fraction",type="fit")
RMSE(pred = pred_ridge$fit,obs = trn$prediction_pay_price)
#RMSE 57.56047 (negative in pred)

#7.5 lasso
library(elasticnet)
fit_lasso <- enet(x=as.matrix(trn[,-20]),y=trn[,20],lambda=0.01,normalize = T)
pred_lasso <- predict(fit_lasso,newx = as.matrix(trn[,-20]),s=.1,mode="fraction",type="fit")
RMSE(pred = pred_lasso$fit,obs = trn$prediction_pay_price)
#RMSE 77.59891 rmse is larger but no negative in response values

#7.6 randomForest
library(randomForest)
fit_rf<- randomForest(prediction_pay_price~.,data= trn)
#cannot allocate vector of size 6.8 Gb

#7.7 xgboost
library(xgboost)
dtrain <- xgb.DMatrix(data = as.matrix(trn[,-20]), label = trn[,20])
bst_linear <- xgboost(data = dtrain, max.depth = 10, eta = 1, nthread = 2, max_delta_step=10,
                   nround = 10, objective = "reg:linear", verbose = 2,eval_metric="rmse")
#RMSE 85.272507






