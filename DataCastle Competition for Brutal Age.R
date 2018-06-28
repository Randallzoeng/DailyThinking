#1. Get the data
setwd("D:/Projects/BrutalAge/data/")

train<- read.table("tap_fun_train.csv",sep=",",
                   header=TRUE, fileEncoding="UTF-8-BOM",
                   stringsAsFactors=FALSE,fill=TRUE)
row.names(train) <- train$user_id
train$lifetime <- as.numeric(as.Date(max(train$register_time))-as.Date(train$register_time))


#2. prediction using simple liner regression (frame problems)
train_simp <- train[,3:110]
fit <- lm(prediction_pay_price~.,data=train_simp)
summary(fit)

# Residual standard error: 57.03 on 2287899 degrees of freedom
# Multiple R-squared:  0.5844,	Adjusted R-squared:  0.5844 
# F-statistic: 3.007e+04 on 107 and 2287899 DF,  p-value: < 2.2e-16
library(Metrics)
rmse(train_simp$prediction_pay_price,fit$fitted.values)

RMSE(pred = fit$fitted.values,obs = train_simp$prediction_pay_price)
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

#Vis it using waffle
library(waffle)
users<-c('Made purchases in 1st week'=41439/3000,'No purchases in next 45d'=2242019/3000,'Made purchases after 1st week'=4549/3000)
waffle(users,xlab='',legend_pos = "top")
#Vis it using venn
# library(gplots)
# venn(list(list_allusers,list_nopayweek,list_nopay45d))




