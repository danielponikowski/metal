library(dplyr)
library(randomForest)
library(gbm)

dane_wola<-read.csv('./Github/metal/data/dane-wola-910obser.csv', 
               check.names=FALSE)
load('./Github/metal/data/dane-zeliwo-uzupelnienie_tw.rda')

dane_wola<-dane_wola[,-c(1,2)]
pairs(dane_wola)
plot(dane_wola$nikiel, dane_wola$chrom)
plot(dane$`Ni [%]`, dane$`Cr [%]`)

dane<-read.csv('./Github/metal/data/dane-zeliwo-uzupelnienie_tw.csv')

s<-sample(nrow(dane_wola), nrow(dane_wola)*0.33)
dane_wola_train<-dane_wola[-s,]
dane_wola_test<-dane_wola[s,]

tward<-randomForest(tward~., data=dane_wola_train)
bledy<-(predict(tward, dane_wola_test)-dane_wola_test$tward)/dane_wola_test$tward
density(bledy)%>%plot()

importance(tward)


tward<-gbm(tward~., data=dane_wola_train, n.trees=1000)
bledy<-(predict(tward, dane_wola_test, n.trees = 1000)-dane_wola_test$tward)/dane_wola_test$tward

density(bledy)%>%plot()




colnames(dane_wola)
colnames(dane)

dane_2<-data_frame(
dane$C....,
dane$Mn....,
dane$Si....,
dane$P....,
dane$S....,
dane$Cr....,
dane$Ni....,
dane$Cu....,
dane$Mg....,
dane$R0.2..MPa.,
dane$A5....,
dane$Twardość.Brinella..HB.
)
colnames(dane_2)<-colnames(dane_wola)



bledy_2<-(predict(tward, dane_2, n.trees = 100)-dane_2$tward)/dane_2$tward
bledy_2 %>% na.omit() %>%density()%>%plot(xlim=c(-2,2))


i<-1

names(dane_2)[i]
dane_2[,i][[1]]%>%na.omit()%>%density()%>%plot()
dane_wola[,i]%>%na.omit()%>%density()%>%plot()
i<-i+1

i<-10
