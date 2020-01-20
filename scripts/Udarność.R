#`Udział austenitu %
#`Udarność Charpy [J]`

load('./Github/metal/data/dane-zeliwo-uzupelnienie_tw.rda')
install.packages('LightGBM')

library(gbm)
library(dplyr)
library(randomForest)

dane_<-dane[!is.na(dane$`Udarność Charpy [J]`),]
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


m<-gbm(`Udarność Charpy [J]`~., data = dane_train, 
       n.trees = 10000)


y_pred<-predict(m, dane_test, n.trees = 10000)
y<-dane_test$`Udarność Charpy [J]`


mean(res^2)

res<-(y-y_pred)
res%>%density()%>%plot()
(res/y)%>%density()%>%plot()

mean(abs((res/y))<0.2)

o<-order(-abs(res))
y[o]
y_pred[o]




########## Lasy

dane__<-dane_
dane__$`Nr źródła`<-NULL # czepial sie tej zmiennej. Pewnie daltego, ze to factor
names(dane__)<-make.names(names(dane__))
dane_test<-dane__[s,]
dane_train<-dane__[-s,]

m<-randomForest(`Udarność.Charpy..J.`~., data = dane_train, 
       n.trees = 10000)


y_pred<-predict(m, dane_test, n.trees = 10000)
y<-dane_test$`Udarność.Charpy..J.`

res<-(y-y_pred)
mean(res^2)


res%>%density()%>%plot()
(res/y)%>%density()%>%plot()

mean(abs((res/y))<0.2)

o<-order(-abs(res))
y[o]
y_pred[o]







# Poniżej jakiś śmieciowy kod. Chcialem zrobic cross walidacje, ale nic madrego mi nie 
# wyszlo. XGBoost nie chcial działać :(

m<-gbm(`Udarność Charpy [J]`~., data = dane_, 
       n.trees = 10000, cv.folds = 10, bag.fraction = 0.3)

m<-gbm(`Udarność Charpy [J]`~., data = dane_,
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




