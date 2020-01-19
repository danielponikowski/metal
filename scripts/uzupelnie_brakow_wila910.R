library(dplyr)

dane_wola<-read.csv('./Github/metal/data/dane-wola-910obser.csv', 
               check.names=FALSE)
dane_wola<-dane_wola[,-c(1,2)]
dane_wola

dane<-read.csv('./Github/metal/data/dane-zeliwo-uzupelnienie_tw.csv')

library(randomForest)


tward<-randomForest(tward~., data=dane_wola)

bledy<-abs(predict(tward)-dane_wola$tward)/dane_wola$tward
density(bledy)%>%plot()

importance(tward)




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
dane$Rm..MPa.,
dane$A5....,
dane$Twardość.Vickersa..HV.
)
colnames(dane_2)<-colnames(dane_wola)


bledy_2<-abs(predict(tward, dane_2)-dane_2$tward)/dane_2$tward
bledy_2 %>% na.omit() %>%density()%>%plot()


dane_wola$tward%>% na.omit() %>%density()%>%plot()
dane$Twardość.Vickersa..HV.%>% na.omit() %>%density()%>%plot()
