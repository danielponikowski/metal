library(gbm)
library(dplyr)
library(randomForest)

load('./Github/metal/data/dane-zeliwo-uzupelnienie_tw.rda')
dane$`Twardość Rockwella [HRC]`<-NULL
dane$`Twardość Rockwella [HRA]`<-NULL
dane$`Twardość Rockwella [HRB]`<-NULL
dane$`Twardość Vickersa [HV]`<-NULL
# Wywalam wszystkie twardosci poza Brinellem

dane_<-dane[!is.na(dane$`Udarność Charpy [J]`),]
dane_[dane_==""]<-NA


braki_danych<-apply(dane_, 2, function(x) sum(!is.na(x))/length(x))
sort(braki_danych)

# Jeżeli jakość zmiennej będzie powyżej progu q, to imputuje zmienną średnią.
# Jeżeli poniżej, to dzielę zmienna według kwantyli i wprowadzam jako factor
# + dodatkowy poziom na braki
q<-0.9


z<-names(dane_[,braki_danych<=q])

for(i in seq_along(z)){
  x<-dane_[[z[i]]]
  if(!is.numeric(x)) next
  
  # k<-1 + 3.322*log(length(na.omit(x))) # liczba klas według zasady kciuka dla histogramów
  k<-3
  klasy<-seq(0, 1, by = 1/k)
  new<-cut(x, unique(quantile(x, klasy, na.rm = TRUE)))
  levels(new)<-c(levels(new), "Inne")
  new[is.na(new)]<-"Inne"
  dane_[[z[i]]]<-new
}

# Produkuje zmienne 0-1 mówiące czy zmienna była uzupełniona brakiem
z<-names(braki_danych)[braki_danych<1]
for(i in seq_along(z)){
  zz<-paste0(z[i], "_is_na")
  dane_[[zz]]<-is.na(dane_[[z[i]]])
}


braki_danych<-apply(dane_, 2, function(x) sum(!is.na(x))/length(x))
dane_<-dane_[,braki_danych>q]
s<-sample(nrow(dane_), round(nrow(dane_)*0.2))
dane_train<-dane_[-s,]
dane_test<-dane_[s,]


srednie<-sapply(1:ncol(dane_), function(i) mean(na.omit(dane_train[,i])))

for(i in 1:ncol(dane_)){
  x<-is.na(dane_[,i])
  dane_[x,i]<-srednie[i]
}

braki_danych<-apply(dane_, 2, function(x) sum(!is.na(x))/length(x))
dane_<-dane_[,braki_danych>q]

dane_train<-dane_[-s,]
dane_test<-dane_[s,]




################## GBM

dane_train<-dane_[-s,]
dane_test<-dane_[s,]

m<-gbm(`Udarność Charpy [J]`~., data = dane_train, 
       n.trees = 50000)


y<-dane_test$`Udarność Charpy [J]`

l<-seq(0, 5000, by = 100)
mse<-vector()
for(i in seq_along(l)){
  mse[i]<-mean((y-predict(m, dane_test, n.trees = l[i]))^2)
}
plot(l, mse, type='l')


y<-dane_test$`Udarność Charpy [J]`
y_pred<-predict(m, dane_test, n.trees = 4000)

res<-(y-y_pred)
mean(res^2)

res%>%density()%>%plot()
(res/y)%>%density()%>%plot()

mean(abs((res/y))<0.1)
mean(abs((res/y))<0.2)

o<-order(-abs(res))
y[o]
y_pred[o]



y<-dane_train$`Udarność Charpy [J]`
y_pred<-predict(m, dane_train, n.trees = 4000)

res<-(y-y_pred)
mean(res^2)

res%>%density()%>%plot()
(res/y)%>%density()%>%plot()
o<-order(-abs(res/y))
res[o]/y[o]
y[o]
y_pred[o]

########## Lasy

dane__<-dane_
dane__$`Nr źródła`<-NULL # czepial sie tej zmiennej. Pewnie daltego, ze to factor
names(dane__)<-make.names(names(dane__))
dane_test<-dane__[s,]
dane_train<-dane__[-s,]

m<-randomForest(`Udarność.Charpy..J.`~., data = dane_train, 
       n.trees = 100000)


y_pred<-predict(m, dane_test)
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




