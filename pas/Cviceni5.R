
# aktivace knihovny s popisnymi statistikami
library(DescTools)

# Nacteni vestavene databaze pizza
pizza<-d.pizza[!is.na(d.pizza$price),]
  # vynechali jsme pritom hodnoty s chybejici cenou

##########################
# Jak byste popsali rozlozeni ceny 
prom1<-pizza$price

# popisne statistiky polohy
summary(prom1)
# popisne statistiky variability
variabilita<-c(var(prom1),sd(prom1),IQR(prom1),MAD(prom1),CoefVar(prom1))
names(variabilita)<-c("Var","SD","IQR","MAD","CV")
variabilita
# popisne statistiky tvaru rozdeleni
tvar<-c(Skew(prom1),Kurt(prom1))
names(tvar)<-c("Sikmost","Spicatost")
tvar
  # Co z popisnych statistik vidite? Jak by mohly vypadat grafy?

hist(prom1,col="skyblue",border="darkblue")
  # co grafu chybi?
# kolik a kde byste hledali modusu?
# modus pomoci jadroveho odhadu
hist(prom1,col="skyblue",border="darkblue",freq=F,main="Histogram",xlab="Cena pizzy",ylab="Hustota")
(jadro<-density(prom1))
lines(jadro,col=2,lwd=2)
lines(density(prom1,bw=6),col="orange",lwd=2)
  # obrazek pro jeden vrchol
  hist(prom1,col="skyblue",border="darkblue",freq=F,main="Histogram",xlab="Cena pizzy",ylab="Hustota",breaks=5)
  lines(density(prom1,bw=8),col="orange",lwd=2)

# Cim muze byt zpusobeno vice vrcholu? Podivat se na data a hledat duvod.
#   Co treba misto, kvalita, rabat?
tapply(prom1,pizza$area,summary)  
  # vypise zadane charakteristiky ciselne promenne zvlast v jednotlivych skupinach
tapply(prom1,pizza$quality,summary)  
tapply(prom1,pizza$rabate,summary)  
  # nejvetsi rozdily jsou zapricineny rabatem
# podivame se na data zvlast podle teto promenne
prom1.T<-pizza$price[pizza$rabate==T]
prom1.F<-pizza$price[pizza$rabate==F]

hist(prom1.T,col="skyblue",border="darkblue",freq=F,main="Histogram",xlab="Cena pizzy s rabatem",ylab="Hustota")
hist(prom1.F,col="skyblue",border="darkblue",freq=F,main="Histogram",xlab="Cena pizzy bez rabatu",ylab="Hustota")
  # je videt, ze ceny se podle rabatu lisi
  
# jak byste rozdily popsali
#   rozdil v poloze, variabilite, tvaru rozdeleni, v modusu
#   spoctete, popiste, nakreslete

# a co odlehla pozorovani?
boxplot(prom1,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="cena pizzy")
  # mame jen kandidaty, nebo primo odlehle hodnoty?
  # a co dilci skupiny podle rabatu?

# jake jsou meze pro odlehle hodnoty (nize jsou jen horni, nebot dolni nemam)
(HM<-quantile(prom1,0.75)+1.5*IQR(prom1))
prom1[prom1>HM]
quantile(prom1,0.75)+3*IQR(prom1)
mean(prom1)+3*sd(prom1)
mean(prom1)+4*sd(prom1)

# predpokladejme, ze chceme urcite "stredni hodnotu dat" 
#   s prihlednutim k faktu, ze hodnoty nad 100 uz jsou prilis vysoke
mean(prom1)
median(prom1)
# useknuty prumer - kolik procent hodnot je vetsich nez 100?
sum(prom1>100)/length(prom1)
mean(prom1,trim=0.015)
  # vynechano 1.5% nejvyssich a nejnizsich hodnot

# obdobne vyzkousejte pro teplotu dovezene pizzy (promenna temperature)
#   pro useknuty prumer vynechte hodnoty mensi nez 30

# kvartilovy koeficient sikmosti
Q1<-quantile(prom1,0.25)
Q3<-quantile(prom1,0.75)
(Q3+Q1-2*median(prom1))/(Q3-Q1)

# oktilovy koeficient spicatosti
Q18<-quantile(prom1,1/8)
Q28<-quantile(prom1,2/8)
Q38<-quantile(prom1,3/8)
Q48<-quantile(prom1,4/8)
Q58<-quantile(prom1,5/8)
Q68<-quantile(prom1,6/8)
Q78<-quantile(prom1,7/8)
((Q78-Q58)+(Q38-Q18))/(Q68-Q28)

#############################
## Normalita dat - QQ plot

# Ktery z vyzkousenych histogramu Vam prisel, ze nejvice odpovida Gaussove krivce?
# Graficky normalitu hodnotime pomoci histogramu a pomoci pravdepodobnostniho grafu (Q-Q plot)
PlotQQ(prom1.T,pch=19,cex=0.8)
  # ukazka grafu pro promennou s tezsimi chvosty - kladna spicatost
PlotQQ(prom1.F,pch=19,cex=0.8)
  # horni cast dat je prilis vzdalena od "vetsiny" 
prom1<-pizza$price
PlotQQ(prom1,pch=19,cex=0.8)
  # v nizkych hodnotach mam malo hodnot, schody odpovidaji "vice vrcholum"
prom1<-pizza$temperature
PlotQQ(prom1,pch=19,cex=0.8)
  # sesikmena data - levostranne sesikmeni
prom1<-pizza$delivery_min
PlotQQ(prom1,pch=19,cex=0.8)
  # sesikmena data - pravostranne sesikmeni

#############################
### Vztah dvou promennych 

#############################
## Vztah dvou kategorickych promennych popisujeme kontingencni tabulkou
# Existuje zavislost mezi mistem a kvalitou pizzy?
prom2<-pizza$area
prom3<-pizza$quality
(tab<-table(prom2,prom3))
# mozna lepe videt z relativnich cetnosti
addmargins(prop.table(tab))
addmargins(prop.table(tab,1))
addmargins(prop.table(tab,2))
  # ktery typ relativnich cetnosti se pro popis zavislosti hodi nejvic?
  # Jak byste vztah popsali?

# Jezdi vsichni ridici vsude, nebo ma kazdy svou oblibenou destinaci
# A kdo z nich ma nejvyhranenejsi preference

#############################
## Vztah dvou ciselnych promennych popisujeme pomoci
#   bodoveho (rozptyloveho) grafu, korelacniho koeficientu, korelacni tabulky

# Jaky je vztah mezi teplotou pizzy a dobou dovazky
prom4<-pizza$temperature
prom5<-pizza$delivery_min
plot(prom4~prom5,pch=19,main="Rozptylovy graf",xlab="Dodaci doba",ylab="Teplota pizzy")
  # Co z grafu vidite?

cov(prom4,prom5,use="complete.obs")
  # co Vam rika kovariance
cor(prom4,prom5,use="complete.obs")
  # a co korelace

# A co je korelacni tabulka?
prom4.c <- factor(ifelse(prom4<=20, '<20', ifelse(prom4<=30, '20-30', 
          ifelse(prom4<=40, '30-40', ifelse(prom4<=50, '40-50', 
          ifelse(prom4<=60, '50-60', '>60'))))),
          levels=c("<20","20-30","30-40","40-50","50-60",">60")) 
prom5.c <- factor(ifelse(prom5<=10, '<10', ifelse(prom5<=20, '10-20', 
          ifelse(prom5<=30, '20-30', ifelse(prom5<=40, '30-40',
          ifelse(prom5<=50, '40-50', ifelse(prom5<=60, '50-50', '>60')))))),
          levels=c("<10","10-20","20-30","30-40","40-50","50-60",">60"))
table(prom4.c,prom5.c)

# Popiste souvislost ceny a casu dodani (bez korelacni tabulky)
