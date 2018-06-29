#1. Get the data
setwd("D:/Projects/BrutalAge/data/")

train<- read.table("tap_fun_train.csv",sep=",",
                   header=TRUE, fileEncoding="UTF-8-BOM",
                   stringsAsFactors=FALSE,fill=TRUE)
row.names(train) <- train$user_id
train$lifetime <- as.numeric(as.Date(max(train$register_time))-as.Date(train$register_time))

test<- read.table("tap_fun_test.csv",sep=",",
                   header=TRUE, fileEncoding="UTF-8-BOM",
                   stringsAsFactors=FALSE,fill=TRUE)

#2. prediction using simple liner regression (frame problems)
train_simp <- train[,3:110]
fit <- lm(prediction_pay_price~.,data=train_simp)
summary(fit)

# Residual standard error: 57.03 on 2287899 degrees of freedom
# Multiple R-squared:  0.5844,	Adjusted R-squared:  0.5844 
# F-statistic: 3.007e+04 on 107 and 2287899 DF,  p-value: < 2.2e-16
library(Metrics)
rmse(train_simp$prediction_pay_price,fit$fitted.values)

rmse(pred = fit$fitted.values,obs = train_simp$prediction_pay_price)
#[1] 57.02891
#As the measure Metrics of this competition is RMSE, 
#so I must figure out a model whose RMSE is less than this simple regression

#3. Explore the data to gain insights
#3.1 Inbalance of the data
# pay nothing for the first 7 days
nopay <- train[train$pay_price==0,]
colist_nopay <- c()
for (i in 3:110){
  if (sum(nopay[,i])==0)
    colist_nopay <- append(colist_nopay,colnames(nopay)[i])
}

# no payment in the next 45 days
nopayment <- train[train$prediction_pay_price==0,]
colist_45nopay <-c()
for (i in 3:110){
  if (sum(nopayment[,i])==0)
    colist_45nopay <- append(colist_45nopay,colnames(nopayment)[i])
}

#find different features
setdiff(colist_45nopay,colist_nopay)

#find some paterns
library(VennDiagram)
list_allusers <- train$user_id
list_nopayweek <- train[train$pay_price==0,1]
list_nopay45d <- train[train$prediction_pay_price==0,1]

length(list_nopayweek)/length(train$user_id)
#98.19% users(2246568 of 2288007) won't pay in the 1st week
length(setdiff(list_nopay45d,list_nopayweek))
#all the users who pay no nothing in 45d are from those 2246568
length(setdiff(list_nopayweek,list_nopay45d))
#4549 made purchse in the 45d even they pay nothing in 1st week, what dirves it?

#Vis it using waffle or we could use venn from gplots
library(waffle)
users<-c('Made purchases in 1st week'=41439/3000,'No purchases in next 45d'=2242019/3000,'Made purchases after 1st week'=4549/3000)
waffle(users,xlab='',legend_pos = "top")

#now let's see how price distribut

summary(train[train$pay_price!=0,'pay_price'])
summary(train[train$prediction_pay_price!=0,'pay_price'])


pay_price <- train$pay_price
prediction_pay_price <- train$prediction_pay_price

hist(prediction_pay_price,             
     freq=FALSE,                                
     col="pink",                                     
     xlab="prediction_pay_price",                       
     main="distribution of prediction_pay_price")     
rug(jitter(prediction_pay_price))                             
lines(density(prediction_pay_price), col="navy", lwd=2)   

prediction_pay_price <- NULL

#xgboost
library(xgboost)

dataset <- as.matrix(train[,3:109])

bst <- xgboost(data = dataset, label = dataset[,107], max_depth = 2, eta = 1,
               nrounds = 2, objective = "reg:linear")

xgb.save(bst, 'model.save')
bst = xgb.load('model.save')
pred <- predict(bst, test$data)

library(Matrix)
Matrix(as.matrix(x), sparse = TRUE)


####features combine

df <- data.frame()
df <- train['lifetime']
df$wood <- train$wood_add_value + train$wood_reduce_value
df$stone <- train$stone_add_value - train$stone_reduce_value
df$ivory <- train$ivory_add_value - train$ivory_reduce_value
df$meat <- train$meat_add_value - train$meat_reduce_value
df$magic <- train$magic_add_value - train$magic_reduce_value
df$infantry <- train$infantry_add_value - train$infantry_reduce_value - train$wound_infantry_add_value + train$wound_infantry_reduce_value
df$cavalry <- train$cavalry_add_value - train$cavalry_reduce_value - train$wound_cavalry_add_value + train$wound_cavalry_reduce_value
df$shaman <- train$shaman_add_value - train$shaman_reduce_value - train$wound_shaman_add_value + train$wound_shaman_reduce_value
#acc
df$gen_acc <- train$general_acceleration_add_value - train$general_acceleration_reduce_value
df$bld_acc <- train$building_acceleration_add_value - train$building_acceleration_reduce_value
df$rsh_acc <- train$reaserch_acceleration_add_value - train$reaserch_acceleration_reduce_value
df$train_acc <- train$training_acceleration_add_value - train$training_acceleration_reduce_value
df$treat_acc <- train$treatment_acceleraion_add_value - train$treatment_acceleration_reduce_value

#level
df$bdlevel <- apply(train[,35:50],1,sum)
df$sclevel <- apply(train[,51:99],1,sum)
#other info
df <- cbind(df,train[,100:109])


#what about now?
fit <- lm(prediction_pay_price~.,data=df)
library(Metrics)
rmse(actual = df$prediction_pay_price,predicted = fit$fitted.values)
#unfortunately it rised to 58.8306
df_pay <- df[df$pay_price!=0,]
df_nopay <- df[df$pay_price==0,]

fit_pay <- lm(prediction_pay_price~.,data=df_pay)
fit_nopay <- lm(prediction_pay_price~.,data=df_nopay)

rmse(actual = df_pay$prediction_pay_price,predicted = fit_pay$fitted.values)
rmse(actual = df_nopay$prediction_pay_price,predicted = fit_nopay$fitted.values)

##let's using xgboost

library(xgboost)

dataset <- as.matrix(train[,3:109])
dataset_pay <- as.matrix(train[train$pay_price!=0,3:109])

bst <- xgboost(data = dataset_pay, label = dataset_pay[,107], max_depth = 2, eta = 1,
               nrounds = 5, objective = "reg:linear")

# [1]	train-rmse:224.188126 
# [2]	train-rmse:188.529678 
# [3]	train-rmse:153.518906 
# [4]	train-rmse:140.240799 
# [5]	train-rmse:112.550278 

xgb.save(bst, 'model.save')
bst = xgb.load('model.save')

pred <- predict(bst, as.matrix(test[,3:108]))
test$prediction_pay_price <- pred
row.names(test)<- test$user_id
write.csv(x=test[,c(1,109)],file = "upload.csv")


#library(Matrix)
#Matrix(as.matrix(x), sparse = TRUE)








