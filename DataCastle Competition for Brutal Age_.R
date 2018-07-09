#Step One → classification
#▊1. read data
library(tidyverse)
library(Matrix)
library(data.table)
setwd("D:/Projects/BrutalAge/data/")
train <- read_csv("tap_fun_train.csv")
test <- read_csv("tap_fun_test.csv")
train$cat <- as.factor(ifelse(train$pay_price==train$prediction_pay_price,'same','diff'))


#▊2. split train set using stratified sampling 
library(sampling)
train$cat <- as.factor(ifelse(train$pay_price==0,'free','paid'))
free_size <- round(length(train[train$cat=="free",]$user_id)*0.8)
paid_size <- round(length(train[train$cat=="paid",]$user_id)*0.8)
train_sample <- strata(train,stratanames="cat",size = c(free_size,paid_size), method="srswor") 
#train<-subset(train,select=-cat)
train_set <- train[train_sample$ID_unit,]
test_set <- train[-train_sample$ID_unit,]



#▊3. correlation
#cor(train)
# library(corrplot)
# corrplot(correlations,method="circle",type="lower",sig.level = 0.01,insig = "blank")


#▊4. feature engineering 
# 4.1. handling imbalance
library(ROSE)
#train_bal <- ROSE(cls ~ . ,data = train_set,seed = 42)$data
df_bal <- ovun.sample(cls ~ ., data = df, method = "both", p=0.5, N=2288007, seed = 1)$data
# 4.2.  scaling or not

# 4.3.  VI & feature selection
library(Boruta) #very memmory consuming
library(xgboost)

#▊5.  modeling
# 5.1. classification (free or paid user in next 45d)
library(xgboost)
sparse_matrix <- sparse.model.matrix(cls ~ ., data = train_set)[,-1]
output_vector = train_set[,"cls"] == "pre_paid"
dtrain <- xgb.DMatrix(data = sparse_matrix, label = output_vector)

bst_lgt <- xgboost(data = dtrain, max.depth = 4, eta = 1, nthread = 2, max_delta_step=10,
               nround = 5, objective = "binary:logistic", verbose = 2,eval_metric="auc")
importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst_lgt)
importance[1:10,]
# 5.2. Prediction (for those paid users, how much will they pay?)
df <- data.frame(train[,-c(1,2)])
library(caret)
fit_linear <- train(prediction_pay_price ~ ., data = df, 
                    method = "lm", 
                    preProc = c("center", "scale"))
# 5.3. cross validation
# param <- list(max_depth=4, eta=1, silent=1, nthread=2, objective='binary:logistic')
# xgb.cv(param, dtrain, nround=5, nfold=5, metrics={'auc'})
# 5.4. Evaluation
library(Metrics)
rmse()
sse()
library(ROSE)
roc.curve(output_vector_test, prediction)
df <- test[,3:109]
df <- as.matrix(df)
df <- as(df,'sparseMatrix')
# 5.5. Tune
#using Grid Search find best param for xgboost
#https://www.rdocumentation.org/packages/NMOF/versions/1.4-1/topics/gridSearch
#https://www.r-bloggers.com/grid-search-in-the-tidyverse/


# ▊6. make prediction



#******************************************************************************************#
#★some references
#https://datascienceplus.com/extreme-gradient-boosting-with-r/
#https://www.kaggle.com/jiashenliu/updated-xgboost-with-parameter-tuning



#▊ EDA
library(plyr) 
pt <- ddply(train, .(prediction_pay_price), summarize, count = length(user_id))
pt[order(pt$count,decreasing = T),]
### feature combination
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

#change level as factor
train[,35:99] <- data.frame(apply(train[35:99],2,as.factor))
imp_feat <- c("pay_price","avg_online_minutes","ivory_add_value","stone_reduce_value","general_acceleration_add_value",
              "wood_add_value","magic_add_value","meat_reduce_value","stone_add_value","building_acceleration_add_value",
              "pve_battle_count","meat_add_value","reaserch_acceleration_add_value","pve_win_count","training_acceleration_add_value",
              "infantry_add_value","wood_reduce_value","training_acceleration_reduce_value","lifetime","wound_infantry_reduce_value",
              "treatment_acceleraion_add_value","bd_barrack_level","shaman_reduce_value")

#if i dont't changes those levels to factor, the IV will look like this
feat_sel <- c("pay_price","avg_online_minutes","ivory_add_value","sr_training_speed_level","stone_reduce_value","wood_reduce_value","wood_add_value",
              "general_acceleration_add_value","stone_add_value","magic_add_value","bd_healing_spring_level","bd_warehouse_level","meat_reduce_value",
              "bd_guest_cavern_level","reaserch_acceleration_add_value","training_acceleration_add_value","bd_market_level","wound_shaman_add_value",
              "building_acceleration_add_value","sr_outpost_durability_level","training_acceleration_reduce_value","bd_hero_strengthen_level",
              "treatment_acceleraion_add_value","pvp_battle_count","prediction_pay_price")

#I want do some combination based on my knows
feat_com <- c("pay_price","avg_online_minutes","ivory","sr_training_speed_level","stone","wood",
              "gen_acc","magic","bd_healing_spring_level","bd_warehouse_level","meat",
              "bd_guest_cavern_level","rsh_acc","train_acc","bd_market_level","shaman",
              "bld_acc","sr_outpost_durability_level","bd_hero_strengthen_level",
              "treat_acc","pvp_battle_count","prediction_pay_price")


#considering clsutering since the distribution is highly skewed
library(plyr) 
pt <- ddply(train, .(pay_count), summarize, count = length(user_id))
pt[order(pt$count,decreasing = T),]

library(ykmeans)
df <- data.frame(train[,c(1,109)])
user_bins <- ykmeans(df, "pay_price", "pay_price", 3)
table(user_bins$cluster)

library(ggplot2)
library(scales)
ggplot(arrange(user_bins, desc(prediction_pay_price)),
       aes(x = 1:length(user_id), y = prediction_pay_price,
           col = as.factor(cluster), shape = as.factor(cluster))) +
  geom_line() +
  xlab("user") +
  ylab("point") +
  scale_y_continuous(label = comma) +
  ggtitle("Point") +
  theme(legend.position = "none")

boxplot(prediction_pay_price ~ cluster, data = user_bins)

# clustering using mclust
install.packages("mclust")
library(mclust)
df_clust <- Mclust(as.matrix(df), G=1:20)
m.best <- dim(df_clust$z)[2]

cor(train$pay_price,train$prediction_pay_price)
df <- train[,c(1,107)]
#**********************RMSE**********************#
df <- data.frame(train[,-c(1,2)])
df_scal <- data.frame(cbind(df[,c(1:106)],df$prediction_pay_price))
colnames(df_scal)[107] <- c("prediction_pay_price")
fit_linear <- lm(prediction_pay_price~., data = df_scal)
library(Metrics)
rmse(actual = df_scal$prediction_pay_price,predicted = fit_linear$fitted.values)

require(caret)
require(xgboost)
require(data.table)
#fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 2, search = "random")
#model <- train(prediction_pay_price~., data = df, method = "xgbTree", trControl = fitControl)
df$labels <- df$prediction_pay_price
df <- df[,c(2:22)]
sparse_matrix <- sparse.model.matrix(labels ~ ., data = df)[,-1]labels()
output_vector = df$labels
dtrain <- xgb.DMatrix(data = sparse_matrix, label = output_vector)

bst_linear <- xgboost(data = dtrain, max.depth = 10, eta = 1, nthread = 5, max_delta_step=5,
                   nround = 10, objective = "reg:linear", verbose = 2,eval_metric="rmse")

param <- list(objective = "reg:linear", booster = "gblinear",
              nthread = 2, alpha = 0.0001, lambda = 1)
watchlist <- list(eval = dtrain, train = dtrain)
num_round <- 2
bst <- xgb.train(param, dtrain, num_round,watchlist)
pred <- predict(bst,dtest)
#64.670815
submit <- data.frame(cbind(test[,1],round(pred,2)))
write.csv(submit,file="submit.csv")

#**********************20180709**********************#
df<- train[,-c(1,2)]
df$cls <- ifelse(df$pay_price==df$prediction_pay_price,"same","diff")
df <- df[,-107]
df <- data.frame(df)
for(i in c(33:97)){
  df[,i] <- factor(df[,i],order=TRUE)
}

library(ROSE)
df_bal <- ovun.sample(cls ~ ., data = df, method = "both", p=0.5, N=2288007, seed = 1)$data

feat <- c("avg_online_minutes","ivory_add_value","pay_price","stone_add_value","magic_add_value",
"reaserch_acceleration_add_value","meat_add_value","general_acceleration_add_value",
"wood_reduce_value","pve_battle_count","cls")
# training_acceleration_add_value
# bd_warehouse_level1
# building_acceleration_add_value
# general_acceleration_reduce_value
# magic_reduce_value

df_bal_sel <- df_bal[,feat]

library(xgboost)
sparse_matrix <- sparse.model.matrix(cls ~ ., data = df_bal_sel)[,-1]
output_vector = df_bal[,"cls"] == "same"
dtrain <- xgb.DMatrix(data = sparse_matrix, label = output_vector)
bst_lgt <- xgboost(data = dtrain, max.depth = 10, eta = 1, nthread = 2, max_delta_step=10,
                   nround = 5, objective = "binary:logistic", verbose = 2,eval_metric="auc")
importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst_lgt)
importance[1:10,]
param <- list(max_depth=10, eta=1, silent=1, nthread=2, objective='binary:logistic')
xgb.cv(param, dtrain, nround=5, nfold=3, metrics={'auc'})


#Pre-Processing using caret
train <- train[train$pay_price!=train$prediction_pay_price,-c(1,2)]



