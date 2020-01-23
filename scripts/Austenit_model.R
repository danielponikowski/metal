getwd()
load('../data/dane-zeliwo-uzupelnienie_tw.rda')
# install.packages('LightGBM')
# 
library(gbm)
library(dplyr)
library(randomForest)

dane_<-dane[!is.na(dane$`Udział austenitu %`),]
dane_$`Wielkość sferoidów`==""
dane_[dane_==""]<-NA
dane_$`Nr źródła`

braki_danych<-apply(dane_, 2, function(x) sum(!is.na(x))/length(x))
dane_<-dane_[,braki_danych>0.9]

s<-sample(nrow(dane_), round(nrow(dane_)*0.2))
dane_train<-dane_[-s,]
dane_test<-dane_[s,]



srednie<-sapply(1:ncol(dane_), function(i) mean(na.omit(dane_train[,i])))

for(i in 1:ncol(dane_)){
  x<-is.na(dane_[,i])
  dane_[x,i]<-srednie[i]
  print(i)
}



dane_train<-dane_[-s,]
dane_test<-dane_[s,]




m<-gbm(`Udział austenitu %`~., data = dane_train, 
       n.trees = 10000)


y_pred<-predict(m, dane_test, n.trees = 10000)
y<-dane_test$`Udział austenitu %`



res<-(y-y_pred)
res%>%density()%>%plot()
(res/y)%>%density()%>%plot()
mean(res^2)

mean(abs((res/y))<0.1)

o<-order(-abs(res))
y[o]
y_pred[o]










# Poniżej jakiś śmieciowy kod. Chcialem zrobic cross walidacje, ale nic madrego mi nie 
# wyszlo. XGBoost nie chcial działać :(

m<-gbm(`Udział austenitu %`~., data = dane_, 
       n.trees = 10000, cv.folds = 10, bag.fraction = 0.3)

m<-gbm(`Udział austenitu %`~., data = dane_,
         family = "gaussian",
         bag.fraction = 0.5)

m$train.error







library(tidyverse)
library(caret)
library(xgboost)

sum(is.na(as.matrix(dane__)))


set.seed(123)
dane__<-dane_
dane__$`Nr źródła`<-NULL
names(dane__)<-  make.names(names(dane__))

model <- train(
  Udział.austenitu.. ~., data = dane__, method = "xgbTree"
  # ,
  # trControl = trainControl("cv", number = 10)
)
# Best tuning parameter
model$bestTune

?train

dane_test
library("xgboost")
??xgb.Dmatrix
y_pred<-predict(model, dane_test)
y<-dane_test$`Udział austenitu %`



res<-(y-y_pred)
res%>%density()%>%plot()
(res/y)%>%density()%>%plot()
mean(res^2)

mean(abs((res/y))<0.1)

o<-order(-abs(res))
y[o]
y_pred[o]


