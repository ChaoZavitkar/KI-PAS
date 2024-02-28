rm(list=ls())
# vycisti pracovni prostor

# Nacteni databaze Policie.RData

# aktivace knihovny s popisnymi statistikami
library(DescTools)

##########################
## Popisne statistiky tvaru rozdeleni
# vyzkousejme na reakcni dobe
prom1<-Policie$react

# charakteristiky tvaru rozdeleni se pocitaji z standardizovanych velicin
z.prom1<-scale(prom1)

# Chaharakteristiky tvaru rozdeleni - sikmost a spicatost
#   se pocitaji ze standardizovanych velicin
# sikmost - prumer ze tretich mocnin z-skoru
# spicatost - prumer ze ctvrtych mocnin z-skoru minus 3
Skew(prom1)
Kurt(prom1)
# Kolik vysly a co rikaji o tvaru rozdeleni?
# Porovnat s histogramem?
hist(prom1,col="skyblue",border="darkblue",main="Histogram",
     xlab="Reakcni doba policistu",ylab="Absolutni cetnosti")

# Jak popisne statistiky polohy, variability a tvaru rozdeleni reaguji 
#   na posunuti a zmenu meritka
prom1.p<-prom1+10
prom1.m<-prom1*10
  # nove promenne

vyst<-matrix(NA,3,4)
vyst[1,1]<-mean(prom1);vyst[1,2]<-sd(prom1);vyst[1,3]<-Skew(prom1);vyst[1,4]<-Kurt(prom1)
vyst[2,1]<-mean(prom1.p);vyst[2,2]<-sd(prom1.p);vyst[2,3]<-Skew(prom1.p);vyst[2,4]<-Kurt(prom1.p)
vyst[3,1]<-mean(prom1.m);vyst[3,2]<-sd(prom1.m);vyst[3,3]<-Skew(prom1.m);vyst[3,4]<-Kurt(prom1.m)
rownames(vyst)<-c("react","react+10","react*10")
colnames(vyst)<-c("Prumer","Sm.odchylka","Sikmost","Spicatost")
vyst

# Porovnani grafu
par(mfrow=c(1,3))
hist(prom1,col="skyblue",border="darkblue",main="Histogram",
     xlab="Reakcni doba policistu",ylab="Absolutni cetnosti")
hist(prom1.p,col="skyblue",border="darkblue",main="Histogram",
     xlab="Reakcni doba policistu - posunuti",ylab="Absolutni cetnosti")
hist(prom1.m,col="skyblue",border="darkblue",main="Histogram",
     xlab="Reakcni doba policistu - meritko",ylab="Absolutni cetnosti")
par(mfrow=c(1,1))

# Spoctete popisne statistiky vysky policistu a komentujte je
prom2<-Policie$height
# popisne statistiky polohy
summary(prom2)
# popisne statistiky variability
variabilita<-c(var(prom2),sd(prom2),IQR(prom2),MAD(prom2),CoefVar(prom2))
names(variabilita)<-c("Var","SD","IQR","MAD","CV")
variabilita
# popisne statistiky tvaru rozdeleni
tvar<-c(Skew(prom2),Kurt(prom2))
names(tvar)<-c("Sikmost","Spicatost")
tvar
  # Co z popisnych statistik vidite? Jak by mohly vypadat grafy?

# Kontrola
hist(prom2,col="skyblue",border="darkblue",main="Histogram",
     xlab="Vyska policistu",ylab="Absolutni cetnosti")
boxplot(prom2,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Vyska policistu")

# Spoctete popisne statistiky diastolickeho tlaku policistu a komentujte je

# Nakreslete histogram a krabicovy graf pro procento tuku policistu
#   Jak byste z grafu odhadli popisne statistiky?
prom3<-Policie$fat
hist(prom3,col="skyblue",border="darkblue",main="Histogram",
     xlab="Procento tuku policistu",ylab="Absolutni cetnosti")
boxplot(prom3,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Procento tuku policistu")

# Kontrola
# popisne statistiky polohy
summary(prom3)
# popisne statistiky variability
variabilita<-c(var(prom3),sd(prom3),IQR(prom3),MAD(prom3),CoefVar(prom3))
names(variabilita)<-c("Var","SD","IQR","MAD","CV")
variabilita
# popisne statistiky tvaru rozdeleni
tvar<-c(Skew(prom3),Kurt(prom3))
names(tvar)<-c("Sikmost","Spicatost")
tvar

# Nakreslete histogram a krabicovy graf pro puls policistu a komentujte

#######################
## Vypocet modusu pro spojita data
# Nejprve mmusime urcit, kolik vrcholu (modu) promenna ma

# Vypocet pro puls
  # podle vzorce Modus = A + h * d0/(d0+d1)
prom4<-Policie$pulse
hist(prom4,col="skyblue",border="darkblue",main="Histogram",
     xlab="Puls policistu",ylab="Absolutni cetnosti")
# Modus je v intervalu 60-70, tedy
A <- 60
h <- 10
hist(prom4,plot=F)
cetnosti<-hist(prom4,plot=F)$counts
d0<-cetnosti[3]-cetnosti[2]
d1<-cetnosti[3]-cetnosti[4]
(modus <- A + h*d0/(d0+d1))

# Vypoctete modus pro diastolicky tlak
prom4<-Policie$diast
#Modus pro diast
A <- 70
h <- 5
hist(prom4,plot=F)
cetnosti<-hist(prom4,plot=F)$counts
m <- which.max(cetnosti)
d0<-cetnosti[m]-cetnosti[m-1]
d1<-cetnosti[m]-cetnosti[m+1]
(modus <- A + h*d0/(d0+d1))

# Vypoctete modus pro vahu
prom4<-Policie$weight
hist(prom4,col="skyblue",border="darkblue",main="Histogram",
     xlab="Vaha policistu",ylab="Absolutni cetnosti")
  # kolikavrcholove rozdeleni mame? Jak najit modus?
  # zmena delicich bodu
hist(prom4,col="skyblue",border="darkblue",main="Histogram",
     xlab="Vaha policistu",ylab="Absolutni cetnosti",right=F)
  # uprava zahrnuti krajnich bodu nepomohla
hist(prom4,col="skyblue",border="darkblue",main="Histogram",
     xlab="Vaha policistu",ylab="Absolutni cetnosti",breaks=5)
  # zmena poctu slupcu zabrala

A <- 70
h <- 10
hist(prom4,plot=F)
cetnosti<-hist(prom4,plot=F,breaks = 5)$counts
m <- which.max(cetnosti)
d0<-cetnosti[m]-cetnosti[m-1]
d1<-cetnosti[m]-cetnosti[m+1]
(modus <- A + h*d0/(d0+d1))

###########################
## Odlehla pozorovani
# Jak je to s odlehlym pozorovanim u procenta tuku?
prom5<-Policie$fat
boxplot(prom5,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Procento tuku policistu")
  # Je to odlehle pozorovani?
  # klasicka definice odlehleho pozorovani funguje jen pro symetricka data
Skew(prom5)
hist(prom5,col="skyblue",border="darkblue",main="Histogram",
     xlab="Procento tuku policistu",ylab="Absolutni cetnosti")
  # sesikmeni je zrejme, ale neni kriticke
  # pozorovani muzeme, nebo nemusime povazovat za odlehle
# Zvysime-li hranici pro odlehle pozorovani na 3IQR
boxplot(prom5,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Procento tuku policistu",range=3)
  # uz se jako odlehle nezobrazi

# Jak je to s odlehlym pozorovanim u diastolickeho tlaku?
#   kde jsou hranice odlehlosti?
