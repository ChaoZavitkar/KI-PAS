# Nacteni databaze Stulong.RData
# oprava nazvu promennych v souboru
names(Stulong)<-c("ID","vyska","vaha","syst1","syst2","chlst","vino","cukr","bmi",
                  "vek","KOURrisk","Skupina","VekK")

# aktivace knihovny s popisnymi statistikami
library(DescTools)

##########################
### Hledani modusu
## Diskretni promenna s mnoha kategoriemi
prom1<-Stulong$vek

# Frekvencni polygon
x.val<-sort(unique(prom1))
ac<-table(prom1)
plot(x.val,ac,type="h",lwd=3,col="darkgreen",main="Frekvencni polygon",
     xlab="Vek v rocich",ylab="Absolutni cetnosti")
lines(x.val,ac,col="red")
# zde je asi mozne urcit modus jako vek s nejvyssi cetnosti 
Mode(prom1)
  # pomoci funkce z knihovny DescTools
  x.val[which.max(ac)]
  
# Pres histogram
hist(prom1,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="Vek v rocich")
  # Kolik bude modu? Ve kterem intervalu je modus?
  # podle vzorce modus = A + h*d0/(d0+d1)
meze<-hist(prom1,plot=F)$breaks
pocty<-hist(prom1,plot=F)$counts
m<-which.max(pocty)
(A<-meze[m])
(h<-meze[2]-meze[1])
(d0<-pocty[m]-pocty[m-1])
(d1<-pocty[m]-pocty[m+1])
(modus<- A + h*d0/(d0+d1))

# Urcete modus pro prvni hodnotu systolickeho tlaku (syst1)
# Urcete modus pro cukr
  # pokud sousedni sloupec k tomu nejvyssimu chybi, bere se jeho cetnost jako 0

##########################
### Hledani modusu
## Spojita promenna

# pracujte s vyskou jako se spojitou promennou a hledejte modus
prom2<-Stulong$vyska
# histogram - budeme prokladat hustotu, je treba ji tedy uvazovat i na y-ove ose
hist(prom2,col="skyblue",border="darkblue",main="Histogram",ylab="Hustota",
     xlab="Vyska v cm",freq=F)
  # mozno pouzit i vice sloupcu
  hist(prom2,col="skyblue",border="darkblue",main="Histogram",ylab="Hustota",
     xlab="Vyska v cm",breaks=15,freq=F) #není absolutní četnost, ale hustota to freq=F

# prolozime jadrovy odhad hustoty
(jadro<-density(prom2))
lines(jadro,col=2)
# chceme hladsi funkci? musime zmenit bandwidth
(jadro<-density(prom2,bw="sj"))
lines(jadro,col=3)
(jadro<-density(prom2,bw=3))
lines(jadro,col=4)
(jadro<-density(prom2,bw=5))
lines(jadro,col=6)
  # jaky bandwidth se Vam jevi jako optimalni?

(jadro<-density(prom2,bw=2))
lines(jadro,col=2)
# kde ma odhad hustoty maximum?
jadro$x[which.max(jadro$y)]

# najdete modus pro spojitou promennou bmi
# pracujte s vahou jako se spojitou promennou a najdete modus

##############################
## Odlehle hodnoty

# Ma promenna vyska odlehle hodnoty
prom3<-Stulong$vyska
# Je promenna symetricka nebo sesikmena? 
#   A je jeji rozdeleni jednovrcholove nebo vicevrcholove?
hist(prom3,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="Vyska v cm",breaks=15)
# A co na odlehle hodnoty rika boxplot?
boxplot(prom3,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Vyska v cm")
  # Vidime adepty na odlehle hodnoty
# Zprisnime pravidlo
boxplot(prom3,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Vyska v cm", range=3)
  # Vidime odlehle hodnoty

# Jake mame hranice pro odlehle hodnoty?
# 1.5 nasobek mezikvartiloveho rozpeti - adept na odlehle pozorovani
(DM<-quantile(prom3,0.25) - 1.5*IQR(prom3))
prom3[prom3<DM]
  # dolni adepti na odlehle pozorovani
(HM<-quantile(prom3,0.75) + 1.5*IQR(prom3))
prom3[prom3>HM]
  # horni adepti na odlehle pozorovani

# 3 nasobek mezikvartiloveho rozpeti - odlehla pozorovani
(DM<-quantile(prom3,0.25) - 3*IQR(prom3))
prom3[prom3<DM]
  # dolni odlehla pozorovani
(HM<-quantile(prom3,0.75) + 3*IQR(prom3))
prom3[prom3>HM]
  # horni odlehla pozorovani

# 3 nasobek smerodatne odchylky - adepti na odlehle pozorovani
(DM<-mean(prom3) - 3*sd(prom3))
prom3[prom3<DM]
  # dolni adepti na odlehle pozorovani
(HM<-mean(prom3) + 3*sd(prom3))
prom3[prom3>HM]
  # horni adepti na odlehle pozorovani

# 4 nasobek smerodatne odchylky - odlehle pozorovani
(DM<-mean(prom3) - 4*sd(prom3))
prom3[prom3<DM]
  # dolni odlehla pozorovani
(HM<-mean(prom3) + 4*sd(prom3))
prom3[prom3>HM]
  # horni odlehla pozorovani

# A jak jsou na tom odlehla pozorovani pro vahu?
# A co prvni hodnota systolickeho tlaku?
prom4<-Stulong$syst1
hist(prom4,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="Systolicky tlak")
  # zde uz je sesikmeni vyraznejsi, zkusime symetrizujici transformaci
ln.prom4<-log(prom4)
hist(ln.prom4,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="Logaritmus systolickeho tlak")
  # pomohlo to? Odlehle hodnoty se hledaji u symetrizovane promenne

boxplot(prom4,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Systolicky tlak")
  # krabicovy graf pro puvodni promennou
  # pokud chci dosahnout tikadlem na vsechny samostatne body,
  #   patricne navysim parametr range
  boxplot(prom4,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Systolicky tlak",range=10)

# A co pro cukr?
boxplot(Stulong$cukr,col="yellow",border="orange3",main="Krabicovy graf",
          ylab="Vyska v cm", range=10)
#########################
## Jsou data normalne rozlozena?
# vyska
prom5<-Stulong$vyska
  
# nejprve histogram
hist(prom5,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="Vyska v cm")
# Sikmost, spicatost
Skew(prom5)
Kurt(prom5)

# Pravdepodobnostni graf
qqnorm(prom5);qqline(prom5,distribution=qnorm,col=2,lwd=2)
PlotQQ(prom5,pch=19)
  # jak cist pravdepodobnostni graf
  # vyska ma priblizne normalni rozdeleni

# Ma bmi normalni rozdeleni?
# Ma prvni hodnota systolickeho tlaku normalni rozdeleni?