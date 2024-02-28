rm(list=ls())
# vycisti pracovni prostor

# Nacteni databaze prij.RData

##########################
### Opakovani z minula

# popisne statistiky pro nominalni (kategorickou neusporadanou) promennou
# popiste do jakeho oboru se studenti hlasili
prom1<-prij$Obor
ac<-table(prom1)
rc<-prop.table(table(prom1))
names(ac)<-names(rc)<-sort(unique(prom1))
data.frame(cbind("Absolutni"=ac,"Procenta"=round(rc*100,2)))
  # tabulka cetnosti - co z ni vidite?

barplot(ac,col=2:5,main="Cetnosti studijnich oboru",ylab="Absolutni cetnosti")
popis<-paste(sort(unique(prom1)),"(",round(rc*100,2),"%)")
pie(rc,lab=popis,col=2:5,main="Relativni cetnosti studijnich oboru")
  # A co je videt z grafu?

# popisne statistiky pro ordinalni promennou
# popiste, jake meli znamky z meteorologie
prom2<-prij$meteo
(ac<-table(prom2))
(kac<-cumsum(ac))
(rc<-round(prop.table(table(prom2)),2))
(krc<-cumsum(rc))
names(ac)<-names(rc)<-sort(unique(prom2))
data.frame(cbind("n(i)"=ac,"N(i)"=kac,"f(i)"=rc,"F(i)"=krc))
  # frekvencni rozdeleni - co z nej vidite?

barplot(ac,col=2:4,main="Cetnosti znamek z meteorologie",ylab="Absolutni cetnosti")
popis<-paste(sort(unique(prom2)),"(",round(rc*100,2),"%)")
pie(rc,lab=popis,col=2:4,main="Relativni cetnosti znamek z meteorologie")
  # A co je videt z grafu?

# popisne statistiky pro ciselnou diskretni promennou
# popiste pocty ziskanych bodu z matematiky
prom3<-prij$matprij
ac<-table(prom3)
x.val<-sort(unique(prom3))
plot(x.val,ac,type="h",lwd=3,col="darkgreen",main="Frekvencni polygon",
     xlab="Pocty bodu u prijimacek z matematiky",ylab="Absolutni cetnosti",ylim=c(0,5))
x.val2<-min(prom3):max(prom3)
ac2<-rep(0,length(x.val2))
for(i in 1:length(x.val2)){
  for(j in 1:length(x.val)){if(x.val2[i]==x.val[j]){ac2[i]<-ac[j]}}
}
lines(x.val2,ac2,col="red")
  # Co graf rika? Je vypovidajici?
# Nabyva-li ciselna diskretni promenna mnoha hodnot, je mozne ji zobrazit
#   pomoci histogramu
hist(prom3,col="skyblue",border="darkblue",main="Histogram",
     xlab="Pocty bodu u prijimacek z matematiky",ylab="Absolutni cetnosti")

hist(prom3[prij$TypM=="B"],col="skyblue",border="darkblue",main="Histogram",
     xlab="Pocty bodu u prijimacek z matematiky",ylab="Absolutni cetnosti")
  # Jak je mozne komentovat vznikly graf? Co vime o tvaru jeho rozdeleni?

# Frekvencni rozdeleni
hist(prom3,plot=F)
deleni<-hist(prom3,plot=F)$breaks
popis<-as.character(deleni[-1])
for(i in 1:length(popis)){
  popis[i]<-paste("(",deleni[i],",",deleni[i+1],"]")
}
ac<-hist(prom3,plot=F)$counts
kac<-cumsum(ac)
rc<-round(ac/sum(ac),3)
krc<-cumsum(rc)
data.frame(cbind(" "=popis,"n(i)"=ac,"N(i)"=kac,"f(i)"=rc,"F(i)"=krc))

# Krabicovy graf
boxplot(prom3,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Pocty bodu u priijimacek z matematiky")

boxplot(promss2,col="yellow",border="orange3",main="Krabicovy graf",
                 ylab="Průměr známek z druhého ročníku")
boxplot(promss2,col="yellow",border="orange3",main="Krabicovy graf",
                 ylab="Průměr známek z druhého ročníku", range=3)
# Co je v grafu videt
min(prom3)
max(prom3)
  # extremy
quantile(prom3,0.25)
quantile(prom3,0.75)
  # kvartily
median(prom3)
  # median
fivenum(prom3)
  # Jak byste promennou popsali na zaklade tohoto grafu?

mean(prom3)
  # prumer
summary(prom3)
  # zakladni popisne statistiky polohy
  # Jak lze komentovat tyto popisne statistiky?

# Vyzkousejte pro ciselnou spojitou promennou SS2 (prumerne znamky 
#   z druheho rocniku SS)
# Nakreslete boxplot, histogram, vypoctete popisne statistiky polohy a 
#   diskutujte, co Vam vyslo
# Jak je to s tim odlehlym pozorovanim?

# Popisne statistiky variability
library(DescTools)
prom4<-prij$ss2
var(prom4)
sd(prom4)
  # Zakladni charakteristiky variability odvozene od prumeru 
  # Vyberovy rozptyl a smerodatna odchylka
  # Jake maji jednotky, jak je interpretovat?
IQR(prom4)
MAD(prom4)
  # Robustni charakteristiky variability necitlive na odlehle pozorovani
  # Mezikvartilove rozpeti a medianova absolutni odchylka okolo medianu
  # Jake maji jednotky, jak je interpretovat?
CoefVar(prom4)
  # Variacni koeficient
  # Jake ma jednotky a jak ho interpretovat?

# Standardizovane veliciny - z.skory
z.val<-scale(prom4)
  # Jak jsou velke? Co popisuji? K cemu se pouzivaji?

# Caharakteristiky tvaru rozdeleni
Skew(prom4)
Kurt(prom4)
  # Kolik vysly a co rikaji o tvaru rozdeleni?
  # Porovnat s histogramem?

# Popiste promennou celprij (celkovy pocet bodu u prijimacek)
# Nakreslete grafy, vypoctete popisne statistiky polohy, variability 
#   a tvaru rozdeleni. Co Vam o datech rikaji?

# Jak popisne statistiky polohy, variability a tvaru rozdeleni reaguji 
#   na posunuti a zmenu meritka?
prom4.p<-prom4+10
prom4.m<-prom4*10
  # nove promenne
vyst<-matrix(NA,3,4)
vyst[1,1]<-mean(prom4);vyst[1,2]<-sd(prom4);vyst[1,3]<-Skew(prom4);vyst[1,4]<-Kurt(prom4)
vyst[2,1]<-mean(prom4.p);vyst[2,2]<-sd(prom4.p);vyst[2,3]<-Skew(prom4.p);vyst[2,4]<-Kurt(prom4.p)
vyst[3,1]<-mean(prom4.m);vyst[3,2]<-sd(prom4.m);vyst[3,3]<-Skew(prom4.m);vyst[3,4]<-Kurt(prom4.m)
rownames(vyst)<-c("ss2","ss2+10","ss2*10")
colnames(vyst)<-c("Prumer","Sm.odchylka","Sikmost","Spicatost")
vyst
