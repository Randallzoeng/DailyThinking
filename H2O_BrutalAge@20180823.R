library(tidyverse)
library(Matrix)
library(data.table)
setwd("D:/Projects/BrutalAge/data/")
train <- read_csv("tap_fun_train.csv")

df<- train[,c(109,106:108)]
# df<- train[,c(109,107)]

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

df$cls <- as.factor(ifelse(train$prediction_pay_price==0,"free","paid"))


library(caret)
df_paid <- data.frame(df[df$cls=="paid",1:19])
set.seed(823)
trn_id <- createDataPartition(df_paid$prediction_pay_price,p=0.8,list=F)

trn <- df[trn_id,] 
vld <- df[-trn_id,]

# #removing high-correlation predictors
# "prediction_pay_price","pay_price","magic","infantry","shaman","pvp","pve","treat_acc"

# trnX <- df[trn_id,2:8]
# trnY <- df$prediction_pay_price[trn_id]
# trn_cls <- df$cls[trn_id]
# 
# vldX <- df[-trn_id,2:8]
# vldY <- df$prediction_pay_price[-trn_id]
# vld_cls <- df$cls[-trn_id]

#▉ identity free/paid user
library(xgboost)
# #caret wraper
# set.seed(823)
# ctrl <- trainControl(method = "cv",number=5,search = "random")
# fit_xgboost <- train(x = trnX,y=trn_cls,method = "xgbTree",trControl = ctrl)

trn_sub <- subset(trn,select = -prediction_pay_price)
sparse_matrix <- sparse.model.matrix(cls ~ ., data = trn_sub)[,-1]
output_vector = trn_sub[,"cls"] == "paid"
dtrain <- xgb.DMatrix(data = sparse_matrix, label = output_vector)

vld_sub <- subset(vld,select = -prediction_pay_price)
sparse_matrix <- sparse.model.matrix(cls ~ ., data = vld_sub)[,-1]
output_vector = vld_sub[,"cls"] == "paid"
dtest <- xgb.DMatrix(data = sparse_matrix, label=output_vector)

watchlist <- list(train=dtrain, eval=dtest)
set.seed(823)
fit_xgbst <- xgb.train(data=dtrain, max_depth=2, eta=1, nrounds=10, watchlist=watchlist,
                 nthread = 2, objective = "binary:logistic",eval_metric = "auc")
pred_xgbst <- as.numeric(predict(fit_xgbst, dtest)>0.5)


#▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉▉
trnX <- trn[,2:8]
trnY <- trn$prediction_pay_price

vldX <- vld[,2:8]
vldY <- vld$prediction_pay_price

#▉Linear Regression
fit_lm <- train(trnX,trnY,method = "lm")
min(fit_lm$results$RMSE)
# [1] 61.10227
pred_lm <- predict(fit_lm,newdata = vldX)
RMSE(pred = pred_lm,obs = vldY)
# [1] 59.06139


#▉PLS
library(pls)
set.seed(823)
ctrl <- trainControl(method = "cv",number=5)
fit_pls <- train(trnX,trnY,method="pls",tuneLength=10,trControl = ctrl)
min(fit_pls$results$RMSE)
# [1] 60.37774
pred_pls <- predict(fit_pls,newdata = vldX)
RMSE(pred = pred_pls,obs = vldY)
# [1] 59.06339


#▉MARS
library(earth)
set.seed(823)
ctrl <- trainControl(method = "cv",number=3)
fit_mars <- train(trnX,trnY,method="earth",tuneLength=10,trControl = ctrl)
min(fit_mars$results$RMSE)
# [1] 60.2836
pred_mars <- predict(fit_mars,newdata = vldX)
RMSE(pred = pred_mars,obs = vldY)
# [1] 60.30475

#▉SVM

#▉KNN
df <- as.data.frame(df)
library(caret)
set.seed(823)
trn_id <- createDataPartition(df$prediction_pay_price,p=0.8,list=F)
trn <- df[trn_id,]
vld <- df[-trn_id,]
set.seed(823)
fit_knn <- train(prediction_pay_price~.,data=trn,method="knn",
                 preProc=c("center","scale"),
                 tuneGrid=data.frame(.k=1:20),
                 trControl=trainControl(method="cv",number = 5))

#▉RF
library(h2o)
h2o.init(port = 54322)
data_path <- c("D:/Projects/BrutalAge/data/tap_fun_train.csv")
train <- h2o.importFile(data_path,header = T)
train <- train[2:2288008,]
trn <- df[trn_id,]
vld <- df[-trn_id,]

y <- "prediction_pay_price"
x <- setdiff(names(trn), y)

fit_rf <- h2o.randomForest(x = x,y = y,training_frame = trn,model_id = "fit_rf",seed = 823)
# RMSE:  66.76978
h2o.performance(model = fit_rf,newdata = vld)
# RMSE:  61.11859

#▉GBM
fit_gbm <- h2o.gbm(x = x,y = y,training_frame = trn,model_id = "fit_gbm",seed = 823)
# RMSE:  49.41872
h2o.performance(model = fit_gbm,newdata = vld)
# RMSE:  57.03195

#hyper tuning
gbm_fit <- h2o.gbm(x = x,y = y,training_frame = trn,validation_frame = vld,model_id = "gbm_fit",ntrees = 500,
                   score_tree_interval = 5,stopping_rounds = 3,stopping_metric = "RMSE",learn_rate = 0.05,seed = 823)

#predict
pred_gbm <- h2o.predict(fit_gbm,newdata = df)
pred_gbm <- as.data.frame(pred_gbm)
submit_sample <- cbind(train$user_id,pred_gbm)
write.csv(submit,file="submit_sample")

h2o.shutdown()

#▉xgboost
train <- as(as.matrix(trn), "sparseMatrix")
test <- as(as.matrix(vld), "sparseMatrix")

dtrain <- xgb.DMatrix(data = train[,1:7], label = train[,"prediction_pay_price"])
dtest <- xgb.DMatrix(data = test[,1:7], label = test[,"prediction_pay_price"])
watchlist <- list(train=dtrain, eval=dtest)

fit_xgbLinear <- xgb.train(data=dtrain, max_depth=2, eta=1, nrounds=10,nthread = 2,booster = "gbtree",alpha = 0.0001, lambda = 1,
                           objective = "reg:linear",watchlist=watchlist,eval_metric = "rmse")
#caret wraper
set.seed(823)
ctrl <- trainControl(method = "cv", number = 3)
fit_xgbLinear <-train(prediction_pay_price ~.,data=df,method="xgbTree",
                      metric = "RMSE",trControl=ctrl,tuneLength=20)
#▉cubist
library(Cubist)
df <- as.data.frame(df[df$prediction_pay_price!=0,])
set.seed(824)
library(caret)
ctrl <- trainControl(method = "cv",number=5)
fit_cubist <- train(df[,2:19],df$prediction_pay_price,method="cubist",tuneLength=10)

#-------------Classification before Regression-----------------
df_cls <- df[,2:20]
df_reg <- df[df$cls=="paid",1:19]
y_cls <- "cls"
x_cls <- setdiff(names(df_cls), y_cls)  

y_reg <- "prediction_pay_price"
x_reg <- setdiff(names(df_reg), y_reg)

cls_glm <- h2o.glm(x=x_cls,y=y_cls,training_frame = df_cls,family = "binomial",
                   lambda_search = T,nfolds = 5,seed=823)
h2o.auc(cls_glm, xval = TRUE)  
# [1] 0.9899017
reg_gbm <- h2o.gbm(x = x_reg,y = y_reg,training_frame = df_reg,model_id = "gbm_fit",
                   ntrees = 500,score_tree_interval = 5,stopping_rounds = 3,
                   stopping_metric = "RMSE",nfolds = 5,seed = 823)

h2o.rmse(reg_gbm, xval = TRUE)
# [1] 441.874