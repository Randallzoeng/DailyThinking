#Step One → classification
# 1. read data
library(tidyverse)
library(Matrix)
library(data.table)
setwd("D:/Projects/BrutalAge/data/")
train <- read_csv("tap_fun_train.csv")
#creat some other features for later use
test <- read_csv("tap_fun_test.csv")

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
df <- test[,3:109]
df <- as.matrix(df)
df <- as(df,'sparseMatrix')

pred <- predict(bst, df)
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
train <- train[train$prediction_pay_price!=0,]
df <- train

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


#@20180704
#Handling Imbalance data using SMOTE(ROSE)
#http://www.ituring.com.cn/article/215742

##cannot allocate vector of size 1.8 Gb
#https://stackoverflow.com/questions/5171593/r-memory-management-cannot-allocate-vector-of-size-n-mb



# library(DMwR)
# set.seed(9560)
# df_scale <- as.data.frame(df_scale)
# smote_train <- SMOTE(cls ~ ., data  = df_scale)                         
# table(smote_train$Class)


# Feature         Gain        Cover Frequency
# 1:                         pay_price 8.954971e-01 0.3522030252     0.125
# 2:                avg_online_minutes 7.646727e-02 0.1650050405     0.075
# 3:                   ivory_add_value 7.850232e-03 0.1362506871     0.075
# 4:                    wood_add_value 4.502457e-03 0.0282040830     0.025
# 5:           sr_training_speed_level 4.351962e-03 0.0903250795     0.025
# 6:    general_acceleration_add_value 1.415684e-03 0.0201083800     0.025
# 7:                stone_reduce_value 1.149080e-03 0.0073406103     0.025
# 8:                bd_warehouse_level 1.043384e-03 0.0025438602     0.025
# 9:                   magic_add_value 8.518638e-04 0.0462656239     0.100
# 10:                  pve_battle_count 8.457561e-04 0.0234323401     0.050
# 11:                 wood_reduce_value 8.451635e-04 0.0028940124     0.025
# 12:                          lifetime 7.949440e-04 0.0186964539     0.075
# 13:   reaserch_acceleration_add_value 7.450027e-04 0.0160820057     0.050
# 14:   building_acceleration_add_value 7.218366e-04 0.0014676757     0.025
# 15:                magic_reduce_value 5.606009e-04 0.0114756825     0.025
# 16:                infantry_add_value 4.231479e-04 0.0095826543     0.025
# 17:           bd_healing_spring_level 3.388621e-04 0.0018930288     0.025
# 18: general_acceleration_reduce_value 3.171399e-04 0.0175391828     0.025
# 19:              cavalry_reduce_value 3.046991e-04 0.0166787733     0.025
# 20:                   pve_lanch_count 2.725660e-04 0.0015679073     0.025
# 21:               bd_hero_gacha_level 2.060896e-04 0.0138764894     0.025
# 22:                   stone_add_value 1.461971e-04 0.0007833621     0.025
# 23:       wound_infantry_reduce_value 1.445956e-04 0.0135862521     0.025
# 24:           sr_outpost_tier_2_level 1.191887e-04 0.0013381087     0.025
# 25:                    meat_add_value 8.519216e-05 0.0008596813     0.025
# Feature         Gain        Cover Frequency



#@20180705
library(tidyverse)
library(Matrix)
library(data.table)
setwd("D:/Projects/BrutalAge/data/")
train <- read_csv("tap_fun_train.csv")

train$lifetime <- as.numeric(as.Date(max(train$register_time))-as.Date(train$register_time))
train$cls <- as.factor(ifelse(train$prediction_pay_price==0,"pre_free","pre_paid"))
train[,35:99] <- data.frame(apply(train[35:99],2,as.factor))
df <- train[,-c(1,2,109)]



library(ROSE)
#train_bal <- ROSE(cls ~ . ,data = train,seed = 42)$data
df_bal <- ovun.sample(cls ~ ., data = df, method = "both", p=0.5, N=2288007, seed = 1)$data
table(df_bal$sr_healing_speed_level)

#featuring selection
library(xgboost)
sparse_matrix <- sparse.model.matrix(cls ~ ., data = df_bal)[,-1]
output_vector = df_bal[,"cls"] == "pre_paid"
dtrain <- xgb.DMatrix(data = sparse_matrix, label = output_vector)
bst_lgt <- xgboost(data = dtrain, max.depth = 5, eta = 0.1, nthread = 2, max_delta_step=5,
                nround = 5, objective = "binary:logistic", verbose = 2,eval_metric="auc")

importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst_lgt)
feat_sel <- importance$Feature

df_bal <- df[,c(feat_sel,"cls")]

#grid search using caret but very time-consuming
library(caret)
# xgb_ctrl <- trainControl(method="cv",number = 5,allowParallel = TRUE,
# verboseIter = FALSE,returnData = FALSE)
#
# xgb_grid <- expand.grid(nrounds = c(100,200,500), max_depth = seq(1,10),
# colsample_bytree = seq(0.5, 0.9, length.out = 5), eta = c(0.01,0.3,1), gamma=
# c(0.0,0.2,1), min_child_weight = c(1:5), subsample = 1)
#
# X <- xgb.DMatrix(as.matrix(df_bal %>% select(-cls))) Y <- df_bal$cls
#
# set.seed(42) fit_bst <- train(X,Y,trControl = xgb_ctrl, tuneGrid = xgb_grid,
# method = "xgbTree",metric = "auc")


## cross validation 
# sparse_matrix <- sparse.model.matrix(cls ~ ., data = df_bal)[,-1]
# output_vector = df_bal[,"cls"] == "pre_paid"
# dtrain <- xgb.DMatrix(data = sparse_matrix, label = output_vector)
nround <- 5
param <- list(max_depth=5, eta=0.1, silent=1, nthread=2, objective='binary:logistic')
xgb.cv(param, dtrain, nround, nfold=3, metrics={'auc'})
#cross validation looks good

#next step we might want build a regression to predict those paid customer
feat_sel <- c("pay_price","avg_online_minutes","ivory_add_value","sr_training_speed_level","stone_reduce_value","wood_reduce_value","wood_add_value",
         "general_acceleration_add_value","stone_add_value","magic_add_value","bd_healing_spring_level","bd_warehouse_level","meat_reduce_value",
         "bd_guest_cavern_level","reaserch_acceleration_add_value","training_acceleration_add_value","bd_market_level","wound_shaman_add_value",
         "building_acceleration_add_value","sr_outpost_durability_level","training_acceleration_reduce_value","bd_hero_strengthen_level",
         "treatment_acceleraion_add_value","pvp_battle_count","prediction_pay_price")

df_sel <- df[,feat_sel]
correlations = cor(df)
library(corrplot)
corrplot(correlations, method="color")
cor(df$wood_reduce_value,df$prediction_pay_price)
#0.6264764

cor(df$wood_reduce_value + df$wood_add_value,df$prediction_pay_price)
#0.642695
#shows more cor than single variable, so I would like to do some feature combination
pairs(df)

feat_com <- c("pay_price","avg_online_minutes","ivory","sr_training_speed_level","stone","wood",
              "gen_acc","magic","bd_healing_spring_level","bd_warehouse_level","meat",
              "bd_guest_cavern_level","rsh_acc","train_acc","bd_market_level","shaman",
              "bld_acc","sr_outpost_durability_level","bd_hero_strengthen_level",
              "treat_acc","pvp_battle_count","prediction_pay_price")

df_com <- df[,feat_com]
correlations = cor(df_sel)
library(corrplot)
corrplot(correlations,method="circle",type="lower",sig.level = 0.01,insig = "blank")

#linear reg
library(caret)

fit_linear <- train(prediction_pay_price ~ ., data = train[train$prediction_pay_price!=0,], 
                    method = "lm", 
                    preProc = c("center", "scale"))


# ctrl <- trainControl(method = "repeatedcv", repeats = 3)
# fit_cv <- train(
#   prediction_pay_price ~ .,
#   data = df_com,
#   method = "lm",
#   preProc = c("center", "scale"),
#   trControl = ctrl
# )

#xgoost
library(xgboost)
sparse_matrix_sel <- sparse.model.matrix(prediction_pay_price~.-1,data=df_sel)
output_vector = df_sel$prediction_pay_price
dtrain_sel <- xgb.DMatrix(data = sparse_matrix_sel, label = output_vector)

sparse_matrix_com <- sparse.model.matrix(prediction_pay_price~.-1,data=df_com)
dtrain_com <- xgb.DMatrix(data = sparse_matrix_com, label = output_vector)


#ctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 2, search = "random")
#model <- train(prediction_pay_price~., data = df_sel,preProc = c("center", "scale"), method = "xgbTree",  trControl = ctrl)
train[,35:99] <- data.frame(apply(train[35:99],2,as.factor))
imp_feat <- c("pay_price","avg_online_minutes","ivory_add_value","stone_reduce_value","general_acceleration_add_value",
"wood_add_value","magic_add_value","meat_reduce_value","stone_add_value","building_acceleration_add_value",
"pve_battle_count","meat_add_value","reaserch_acceleration_add_value","pve_win_count","training_acceleration_add_value",
"infantry_add_value","wood_reduce_value","training_acceleration_reduce_value","lifetime","wound_infantry_reduce_value",
"treatment_acceleraion_add_value","bd_barrack_level","shaman_reduce_value")
df <- train[train$prediction_pay_price!=0,c(imp_feat,"prediction_pay_price")]




