#Get the data.
getwd()
filedir<- "C:/Users/randa/Desktop/JupyterNote/tab4fun/data/tap_fun_train.csv"
train<- read.table(filedir,sep=",",
                   header=TRUE, fileEncoding="UTF-8-BOM",
                   stringsAsFactors=FALSE,fill=TRUE)
train$lifetime <- as.numeric(as.Date(max(train$register_time))-as.Date(train$register_time))
backup <- train
ref <- read.table('C:/Users/randa/Desktop/JupyterNote/tab4fun/data/ref.csv',sep=",",
                  header=TRUE, fileEncoding="UTF-8-BOM",
                  stringsAsFactors=FALSE,fill=TRUE)
# STP one: pay nothing for the first 7 days
##  no payment in the next 45 days
nopay <- train[train$pay_price==0,]
list_1 <- c()
for (i in 3:109){
  if (sum(nopay[,i])==0)
    list_1 <- append(list_1,colnames(nopay)[i])
    #print(colnames(nopay)[i])
}
#*************************************************#
nopayment <- train[train$prediction_pay_price==0,]
list_2<-c()
for (i in 3:109){
  if (sum(nopayment[,i])==0)
    list_2 <- append(list_2,colnames(nopayment)[i])
    #print(colnames(nopayment)[i])
}
#what happened to the 2246568-2242019=4549

##  STP two: using PCA to find clusters
library(lattice)
library(caret)

rest_user <- train[train$pay_price!=0,]
df <- rest_user[,-c(1,2,109)]
row.names(df) <- rest_user$user_id
nzv <- nearZeroVar(df)
df_filter_info <- df[,-nzv]

df_cor <- cor(df_filter_info)
highly_cor <- findCorrelation(df_cor,cutoff=.7)
df_filter_col <- df_filter_info[,-highly_cor]

df_pca <- prcomp(df_filter_col,scale. = T)
df_pca$rotation #cluster or not?

#another way of pca
df.pca <- princomp(df, cor = T)
summary(df.pca,loadings = T)
df_trans_pca <- predict(df.pca)

#prediction using simple liner regression 
train_simp <- train[,3:109]
fit <- lm(train_simp$prediction_pay_price~.,data=train_simp)
summary(fit)

library(Metrics)
RMSE(pred = fit$fitted.values,obs = train_simp$prediction_pay_price)
#57.02907
#
library(h2o)
h2o.init(port = 54322)
#h2o.shutdown()
#demo(h2o.glm)
Y=colnames(train)[109]
X=colnames(train)[3:108]
payment45.glm <- h2o.glm(training_frame = train,x=X,Y=y)
# View model information: training statistics,

summary(airlines.glm)

# Predict using GLM model
pred = h2o.predict(object = airlines.glm, newdata =
                        airlines.test)
# Look at summary of predictions: probability of TRUE
class (p1)
summary(pred$p1)

#library(devtools)
#install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package")
