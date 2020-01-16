library(dplyr)


getwd()
dane<-read.csv('/home/marcin/Github/metal/data/dane-zeliwo.csv', 
               check.names=FALSE, row.names = 1)
dane<-read.csv('/home/marcin/Github/metal/data/dane-zeliwo-uzupelnienie.csv', 
               check.names=FALSE, row.names = 1)
tabela_twardosci<-read.csv('/home/marcin/Github/metal/data/Tabela twardosci.csv')
tabela_twardosci_HRA<-read.csv('/home/marcin/Github/metal/data/Tabela twardosci HRA.csv')
# tabela_twardosci_HRA$HRA<-as.numeric(as.character(tabela_twardosci_HRA$HRA))
tabela_twardosci_HRA2<-read.csv('/home/marcin/Github/metal/data/Tabela twardosci HRA2.csv')



names(dane)
twardosc<-dane[,c(33,34,35,36,37)]


twardosc$`Twardość Brinella [HB]`%>%na.omit()%>%range()
tabela_twardosci$HB%>%na.omit()%>%range()

twardosc$`Twardość Rockwella [HRC]`%>%na.omit()%>%range()
tabela_twardosci$HRC%>%na.omit()%>%range()

# Z HRA nie wiem co zrobić 
twardosc$`Twardość Rockwella [HRA]`%>%na.omit()%>%range()
tabela_twardosci_HRA$HRA%>%na.omit()%>%range()


twardosc$`Twardość Rockwella [HRA]`%>%na.omit()%>%sort()


# twardosc$`Twardość Rockwella [HRB]`%>%na.omit()%>%range() same braki danych

twardosc$`Twardość Vickersa [HV]`%>%na.omit()%>%range()
tabela_twardosci$HV%>%na.omit()%>%range()




HB<-tabela_twardosci$HB
HRB<-tabela_twardosci$HRB
HRC<-tabela_twardosci$HRC
HV<-tabela_twardosci$HV


plot(HRC, HB, type = 'l')
plot(HRB, HB, type = 'l')
plot(HV, HB, type = 'l')

# W HRA dane z tabeli wykraczały poza zakres. 
# Użyłem danych z tabeli i dodatkowo wkleiłem do kalkulatora kilka punktów z danych.
HRA<-c(tabela_twardosci_HRA2$HRA, tabela_twardosci_HRA$HRA)
HB_HRA<-c(tabela_twardosci_HRA2$HB, tabela_twardosci_HRA$HB)
HB_HRA<-(HB_HRA[!(is.na(HRA))])
HRA<-(HRA[!(is.na(HRA))])
HB_HRA<-HB_HRA[order(HRA)]
HRA<-sort(HRA)

plot(HRA, HB_HRA, type = 'l')


HRA_f<-splinefun(HRA, HB_HRA)
plot(HRA, HB_HRA, type = 'l')
lines(HRA, HRA_f(HRA), type='l', col = 'red')

HRC_f<-splinefun(HRC, HB)
plot(HRC, HB, type = 'l')
lines(HRC, HRC_f(HRC), type='l', col = 'red')

HRB_f<-splinefun(HRB, HB)
plot(HRB, HB, type = 'l')
lines(HRB, HRB_f(HRB), type='l', col = 'red')

HV_f<-splinefun(HV, HB)
plot(HV, HB, type = 'l')
lines(HV, HV_f(HV), type='l', col = 'red')





HRA_f(dane$`Twardość Rockwella [HRA]`)
HRB_f(dane$`Twardość Rockwella [HRB]`)
HRC_f(dane$`Twardość Rockwella [HRC]`)
HV_f(dane$`Twardość Vickersa [HV]`)


dane$`Twardość Brinella [HB]`<-round(coalesce(
  dane$`Twardość Brinella [HB]`,
  HRA_f(dane$`Twardość Rockwella [HRA]`),
  HRB_f(dane$`Twardość Rockwella [HRB]`),
  HRC_f(dane$`Twardość Rockwella [HRC]`),
  HV_f(dane$`Twardość Vickersa [HV]`)
), 2)


x<-dane$`Twardość Brinella [HB]`
is.na(x)%>%sum()/length(x)

# Pozostałe braki danych wynikają z braku jakiejkolwiek twardości
all(is.na(twardosc[is.na(x),]))

save(dane, file='/home/marcin/Github/metal/data/dane-zeliwo-uzupelnienie_tw.rda')
load('/home/marcin/Github/metal/data/dane-zeliwo-uzupelnienie_tw.rda')
