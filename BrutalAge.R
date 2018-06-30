


setwd("/Users/randall/codes/r_language/BrutalAge/tap4fun_data/")
train <- read.csv("tap_fun_train.csv",sep=",",
         header=TRUE, fileEncoding="UTF-8-BOM",
         stringsAsFactors=FALSE,fill=TRUE)
#sub_sample.csv
#tap_fun_test.csv
#tap_fun_train.csv
row.names(train) <- train$user_id
train$lifetime <- as.numeric(as.Date(max(train$register_time))-as.Date(train$register_time))

#是否需要将游戏级别转换为因子？
#train[,35:99] <- data.frame(apply(train[35:99],2,as.factor))
train$cls <- ifelse(train$prediction_pay_price==0,"0","1")
#0 stand for free users
#1 stand for paid users
df <- train[,c(3:108,110:111)]
df$cls <- as.factor(df$cls)


#Imbalanced
#http://www.ituring.com.cn/article/215742
library(ROSE)

train_bal <- ROSE(cls ~ . ,data = df,seed = 123)$data
prop.table(table(df$cls))
prop.table(table(train_bal$cls))



#featuring selection
library(Boruta)
set.seed(123)
btr <- Boruta(cls~.,data = df)

#using xgboost
library(xgboost)
require(Matrix)
require(data.table)

#sparse_matrix <- sparse.model.matrix(cls ~ ., data = train_bal)[,-1]
#output_vector = train_bal[,cls] == "paid"
train_set <- as.matrix(train_bal)

bst <- xgboost(data = train_set[,1:107], label = train_bal[108], max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")








