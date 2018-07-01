
library(tidyverse)
setwd("/Users/randall/codes/r_language/BrutalAge/tap4fun_data/")
# train <- read.csv("tap_fun_train.csv",sep=",",
#          header=TRUE, fileEncoding="UTF-8-BOM",
#          stringsAsFactors=FALSE,fill=TRUE)
train <- read_csv("tap_fun_train.csv")
train$lifetime <- as.numeric(as.Date(max(train$register_time))-as.Date(train$register_time))
train <- as.data.frame(train)
sort(summary(as.factor(train$pay_count)),decreasing = T)
hist(train$pay_count)

library(plyr) 
pt <- ddply(train, .(pay_count), summarize, count = length(user_id))
pt$pro <- round(pt$count / length(train$user_id) *100,4)
rm(pt)

cor(train$prediction_pay_price,train$pay_price)
cor(train$prediction_pay_price,train$pay_count)
cor(train$pay_price,train$pay_count)
train$avg_pay_price <- ifelse(train$pay_count==0,0,train$pay_price / train$pay_count)
cor(train$prediction_pay_price,train$avg_pay_price)

#分层抽样
train$cat <- ifelse(train$pay_price==0,'free','paid')
train$cat <- as.factor(train$cat)
summary(train$cat)

library(sampling) 
free_size <- round(2246568*0.8)
paid_size <- round(41439*0.8)
train_sample <- strata(train,stratanames="cat",size = c(free_size,paid_size), method="srswor") 
train <- train[,-c(110)]
#train<-subset(train,select=-cat)
train_set <- train[train_sample$ID_unit,]
test_set <- train[-train_sample$ID_unit,]
rm(train_sample)

#是否需要将游戏级别转换为因子？
#train[,35:99] <- data.frame(apply(train[35:99],2,as.factor))
train_set$cls <- ifelse(train_set$prediction_pay_price==0,"0","1")
#0 stand for free users
#1 stand for paid users
df <- train_set[,c(3:108,110:111)]
df$cls <- as.factor(df$cls)


#Imbalanced
#http://www.ituring.com.cn/article/215742
library(ROSE)

train_bal <- ROSE(cls ~ . ,data = df,seed = 42)$data
prop.table(table(df$cls))
prop.table(table(train_bal$cls))

rm(df)

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
train_bal <- as.tibble(train_bal)
train_set <- as.matrix(train_bal)
mt <- NULL
bst <- xgboost(data = train_set[,1:107], label = train_bal[108], max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")




