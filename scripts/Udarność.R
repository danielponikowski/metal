library(gbm)
library(dplyr)
library(randomForest)

load('./Github/metal/data/dane-zeliwo-uzupelnienie_tw.rda')
read.csv('./Github/metal/data/dane-zeliwo-uzupelnienie_tw.csv')
dane$`Twardość Rockwella [HRC]`<-NULL
dane$`Twardość Rockwella [HRA]`<-NULL
dane$`Twardość Rockwella [HRB]`<-NULL
dane$`Twardość Vickersa [HV]`<-NULL
# Wywalam wszystkie twardosci poza Brinellem

dane_<-dane[!is.na(dane$`Udarność Charpy [J]`),]
dane_[dane_==""]<-NA


set.seed(123)
s<-sample(nrow(dane_), round(nrow(dane_)*0.2))
dane_train<-dane_[-s,]
dane_test<-dane_[s,]
dane_test_final<-dane_[s,]

braki_danych<-apply(dane_train, 2, function(x) sum(!is.na(x))/length(x))
sort(braki_danych)

# Jeżeli jakość zmiennej będzie powyżej progu q, to imputuje zmienną średnią.
# Jeżeli poniżej, to dzielę zmienna według kwantyli i wprowadzam jako factor
# + dodatkowy poziom na braki
q<-0.9


z<-names(dane_train[,braki_danych<=q])

for(i in seq_along(z)){
  x<-dane_train[[z[i]]]
  x_test<-dane_test[[z[i]]]
  x_test_final<-dane_test_final[[z[i]]]
  
  if(!is.numeric(x)) next
  
  # k<-floor(1 + 3.322*log(length(na.omit(x)))) # liczba klas według zasady kciuka dla histogramów
  k<-3
  klasy<-seq(0, 1, by = 1/k)
  podzial<-unique(quantile(x, klasy, na.rm = TRUE))
  podzial[1]<- -Inf
  podzial[length(podzial)]<- Inf
  
  new<-cut(x, podzial)
  new_test<-cut(x_test, podzial)
  new_test_final<-cut(x_test_final, podzial)
  
  levels(new)<-c(levels(new), "Inne")
  levels(new_test)<-levels(new)
  levels(new_test_final)<-levels(new)
  
  new[is.na(new)]<-"Inne"
  new_test[is.na(new_test)]<-"Inne"
  new_test_final[is.na(new_test_final)]<-"Inne"
  
  dane_train[[z[i]]]<-new
  dane_test[[z[i]]]<-new_test
  dane_test_final[[z[i]]]<-new_test_final
}

# Produkuje zmienne 0-1 mówiące czy zmienna była uzupełniona brakiem
z<-names(braki_danych)[braki_danych<1]
for(i in seq_along(z)){
  zz<-paste0(z[i], "_is_na")
  dane_train[[zz]]<-as.integer(is.na(dane_train[[z[i]]]))
  dane_test[[zz]]<-as.integer(is.na(dane_test[[z[i]]]))
  dane_test_final[[zz]]<-as.integer(is.na(dane_test_final[[z[i]]]))
}


braki_danych<-apply(dane_train, 2, function(x) sum(!is.na(x))/length(x))
dane_train<-dane_train[,braki_danych>q]
dane_test<-dane_test[,braki_danych>q]
dane_test_final<-dane_test_final[,braki_danych>q]

srednie<-sapply(1:ncol(dane_), function(i) mean(na.omit(dane_train[,i])))

for(i in 1:ncol(dane_)){
  dane_train[is.na(dane_train[,i]),i]<-srednie[i]
  dane_test[is.na(dane_test[,i]),i]<-srednie[i]
  dane_test_final[is.na(dane_test_final[,i]),i]<-srednie[i]
}

braki_danych<-apply(dane_train, 2, function(x) sum(!is.na(x))/length(x))
dane_train<-dane_train[,braki_danych>q]
dane_test<-dane_test[,braki_danych>q]
dane_test_final<-dane_test_final[,braki_danych>q]





################## GBM


m<-gbm(`Udarność Charpy [J]`~., data = dane_train, 
       n.trees =8000)
y_min<-min(dane_train$`Udarność Charpy [J]`)
y_max<-max(dane_train$`Udarność Charpy [J]`)


y<-dane_test$`Udarność Charpy [J]`

l<-seq(0, 8000, by = 100)
mse<-vector()
for(i in seq_along(l)){
  y_pred<-pmin(y_max, pmax(y_min, predict(m, dane_test, n.trees = l[i])))
  mse[i]<-mean((y-y_pred)^2)
}
plot(l, mse, type='l')
(n_trees<-l[which.min(mse)])

# Wychodziły czasami ujemne predykcje
y<-dane_test$`Udarność Charpy [J]`
y_pred<-pmin(y_max, pmax(y_min, predict(m, dane_test, n.trees = n_trees)))

res<-(y-y_pred)
mean(res^2)

res%>%density()%>%plot()
(res/y)%>%density()%>%plot()

mean(abs((res/y))<0.1)
mean(abs((res/y))<0.2)

o<-order(-abs(res))
y[o]
y_pred[o]
res[o]


plot(sort(y), ylab="Udarność")
lines(sort(y)*1.2, lty=2, col = 'red')
lines(sort(y)*0.8, lty=2, col = 'red')
points(y_pred[order(y)], col = 'red')




# Bład na zbiorze treningowym - może wywalić jakiś obserwacje?

y_train<-dane_train$`Udarność Charpy [J]`
y_train_pred<-predict(m, dane_train, n.trees = 4000)

res_train<-(y_train-y_train_pred)
mean(res_train^2)

res_train%>%density()%>%plot()
(res_train/y_train)%>%density()%>%plot()





################### Usunięcie obserwacji odstających

dane_train_2<-dane_train[(res_train/y_train)<3.5,]

m2<-gbm(`Udarność Charpy [J]`~., data = dane_train_2, 
       n.trees =8000)


y<-dane_test$`Udarność Charpy [J]`

l<-seq(0, 8000, by = 100)
mse<-vector()

for(i in seq_along(l)){
  y_pred2<-pmin(y_max, pmax(y_min, predict(m2, dane_test, n.trees = l[i])))
  mse[i]<-mean((y-y_pred2)^2)
}
plot(l, mse, type='l')
(n_trees<-l[which.min(mse)])


y<-dane_test$`Udarność Charpy [J]`
y_pred2<-pmin(y_max, pmax(y_min, predict(m2, dane_test, n.trees = n_trees)))

res2<-(y-y_pred2)
mean(res2^2)

res2%>%density()%>%plot()
(res2/y)%>%density()%>%plot()

mean(abs((res2/y))<0.1)
mean(abs((res2/y))<0.2)

o2<-order(-abs(res2))
y[o2]
y_pred2[o2]
res2[o2]


plot(sort(y), ylab="Udarność")
points(y_pred2[order(y)], col = 'red')


mean(res^2)
mean(res2^2)


# Wychodzi gorzej :(
# Raczej zostańmy bez wyrzucania obserwacji, które słabo się zamodelowały


plot(sort(y), ylab="Udarność")
lines(sort(y)*1.2, lty=2, col = 'red')
lines(sort(y)*0.8, lty=2, col = 'red')
points(y_pred2[order(y)], col = 'red')

max(y_pred2)
max(y)

########## Poniżej nieodświeżony, prawdopodobnie niedziałający kod. Nie uruchamiać.
# 
# ########## Lasy
# 
# dane__<-dane_
# dane__$`Nr źródła`<-NULL # czepial sie tej zmiennej. Pewnie daltego, ze to factor
# names(dane__)<-make.names(names(dane__))
# dane_test<-dane__[s,]
# dane_train<-dane__[-s,]
# 
# m<-randomForest(`Udarność.Charpy..J.`~., data = dane_train, 
#        n.trees = 100000)
# 
# 
# y_pred<-predict(m, dane_test)
# y<-dane_test$`Udarność.Charpy..J.`
# 
# res<-(y-y_pred)
# mean(res^2)
# 
# 
# res%>%density()%>%plot()
# (res/y)%>%density()%>%plot()
# 
# mean(abs((res/y))<0.2)
# 
# o<-order(-abs(res))
# y[o]
# y_pred[o]
# 
# 
# plot(sort(y), ylab = 'Udarność')
# points(y_pred[order(y)], col = 'red')
# 
# 
# 
# 
# 
# 
# # Poniżej jakiś śmieciowy kod. Chcialem zrobic cross walidacje, ale nic madrego mi nie 
# # wyszlo. XGBoost nie chcial działać :(
# 
# m<-gbm(`Udarność Charpy [J]`~., data = dane_, 
#        n.trees = 10000, cv.folds = 10, bag.fraction = 0.3)
# 
# m<-gbm(`Udarność Charpy [J]`~., data = dane_,
#          family = "gaussian",
#          bag.fraction = 0.5)
# 
# 
# 
# 
# 
# 
# 
# library(tidyverse)
# library(caret)
# library(xgboost)
# 
# sum(is.na(as.matrix(dane__)))
# 
# 
# set.seed(123)
# dane__<-dane_
# dane__$`Nr źródła`<-NULL
# names(dane__)<-  make.names(names(dane__))
# 
# model <- train(
#   Udział.austenitu.. ~., data = dane__, method = "xgbTree"
#   # ,
#   # trControl = trainControl("cv", number = 10)
# )
# # Best tuning parameter
# model$bestTune
# 
# 
# 
# 
