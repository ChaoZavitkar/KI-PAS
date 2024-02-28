### Cviceni k Pravdepodobnost a Statistika
## cvicici: Alena Cernikova, e-mail: alena.cernikova@ujep.cz

### Populace, reprezentativni vyber
# Vzhledem k jake populaci byste mohli tvorit reprezentativni vyber?
# Proc jste/ nejste repreyentativni vyber vzhledem ke vsem studentum v CR? 
#   Ke studentum UJEP? Ke studentum informatiky UJEP? Ke studentum tretiho rocniku UJEP?

# Nactete si datovy soubor Kojeni2.RData
#   Data pouzita na zpracovani DP na PrF UK nekdy v roce 2001.
#   Data byla sbirana na jedne Prazske a jedne Kladenske porodnici
#   (v promenne Porodnice znaceno jako "Praha", "venkov")
attach(Kojeni2)
  # zpristupneni databaze

# Pro jakou populaci by byl tento vyber reprezentativni?
#   Muze/nemuze byt reprezentativni ke vsem novorozencum v CR?
#     Ke vsem novorozencum v CR v roce 2001? Ke vsem stredoceskym novorozencum
#     v roce 2001? Je mozne zobecnit vysledek porovnani Praha vs. venkov v roce 2001?
#     A co Praha vs. Kladno v roce 2001?

### Typy promennych
# Jakeho typu jsou promenne v databazi? Ktere jsou ciselne spojite?
#   Ktere ciselne diskretni? Ktere jsou kategoricke ordinalni?
#   Ktere jsou kategoricke nominalni? Jakymi zpusoby je v datech popsano pohlavi deti?

### Popis kategoricke promenne
# absolutni a relativni cetnosti, kumulativni absolutni a relativni cetnosti

## nominalni promenna
# vypoctete absolutni a relativni cetnosti promenne Hoch
table(Hoch)
  # absolutni cetnosti
prop.table(table(Hoch))*100
  # relativni cetnosti v procentech
  round(prop.table(table(Hoch))*100,2)
cbind("absolutni"=table(Hoch),"relativni"=round(prop.table(table(Hoch))*100,2))
  # v jedne tabulce

# vypoctete absolutni a relativni cetnosti pro misto porodu   
# vytvorte kombinovanou promennou pro pohlavi a misto porodu a 
#    vypoctete pro ni absolutni a relativni cetnosti.
Por.poh<-ifelse(hoch==1&Porodnice=="Praha","Hoch z Prahy","Hoch z venkova")
Por.poh<-ifelse(hoch==0&Porodnice=="Praha","Divka z Prahy",Por.poh)
Por.poh<-ifelse(hoch==0&Porodnice=="venkov","Divka z venkova",Por.poh)

# Jaky graf byste pouzili pro nominalni promennou?
## POZOR: Kazdy graf musi mit nazev a oznaceni os, jinak se krati body!!!
barplot(table(Por.poh),col=2:5,main="Sloupcovy graf", ylab = "Absolutní četnosti", xlab = "Pohlaví a místa porodnic")
  # sloupcovy graf
pie(table(Por.poh),col=2:5,main="Kolacovy graf")
  # kolacovy graf
# popisky jednotlivych bodu
leg<-sort(unique(Por.poh))
rc<-round(prop.table(table(Por.poh))*100,2)
for(i in 1:length(leg)){
  leg[i]<-paste(leg[i],"(",rc[i],"% )")
}
pie(table(Por.poh),col=2:5,main="Kolacovy graf",labels=leg)

## ordinalni promenna
# vypoctete absolutni a relativni cetnosti a kumulativni absolutni a relativni cetnosti
#   pro promennou Vzdelani
table(Vzdelani)
  # neni usporadano - usporadat
Vzdel<-ordered(Vzdelani,levels=c("základní","maturita","VŠ"))
(ac<-table(Vzdel))
  # absolutni cetnosti
(kac<-cumsum(ac))
  #  kumulativni absolutni cetnosti
(rc<-round(prop.table(table(Vzdel)),2))
  # relativni cetnosti
(krc<-cumsum(rc))
  # kumulativni relativni cetnosti
cbind("n(i)"=ac,"N(i)"=kac,"p(i)"=rc,"P(i)"=krc)
  # v jedne tabulce

# Jake grafy byste pouzili pro ordinalni promennou?

## ciselna diskretni promenna
# Pro dobu kojeni (promanne trvani) vypoctete frekvencni rozdeleni
#   Muzete si zvolit vlastni intervaly, se kterymi budete pracovat,
#   nebo mute pouzit deleni, ktere R-ko nabizi v histogramu
hist(trvani,plot=F)
  # popis histogramu, ktery se nenakresli
deleni<-hist(trvani,plot=F)$breaks
  # delici body
popis<-as.character(deleni[-1])
for(i in 1:length(popis)){
  popis[i]<-paste("(",deleni[i],",",deleni[i+1],"]")
}
  # popis intervalu
ac<-hist(trvani,plot=F)$counts
  # absolutni cetnosti v intervalech
kac<-cumsum(ac)
  # kumulativni absolutni cetnosti
rc<-round(ac/sum(ac),3)
  # relativni cetnosti
krc<-cumsum(rc)
  # kumulativni relativni cetnosti
cbind(" "=popis,"n(i)"=ac,"N(i)"=kac,"f(i)"=rc,"F(i)"=krc)
  # v jedne tabulce

# Jakym grafem byste kreslili ciselnou diskretni promennou?
library(ggplot2)
ggplot(Kojeni2, aes(trvani)) +  geom_freqpoly(breaks = deleni)
  # frekvencni polygon
x<-hist(trvani,plot=F)$mids
plot(x,ac,col=3,lwd=2,type="h",xlim=c(-10,30),main="Frekvencni polygon",xlab="Trvani",ylab="Pocty")
lines(c(-2.5,x,27.5),c(0,ac,0))
  # nebo taky jinak

## ciselna spojita promenna
# Pro porodni hmotnost deti (promanne por.hmotnost) vypoctete frekvencni rozdeleni
# dale vypoctete prumer
mean(por.hmotnost)
# a vybrane percentily: minumim, dolni kvartil, median, horni kvartil a maximum
quantile(por.hmotnost,c(0,1/4,1/2,3/4,1))
  fivenum(por.hmotnost)
  # vybrane percentily  
summary(por.hmotnost)
  # zakladni popisne statistiky polohy

# Jaky graf byste pouzili pro ciselnou spojitou promennou?
hist(por.hmotnost,labels=T,col="lightblue",border="royalblue4",ylim=c(0,45))
  # histogram
  # jakeho tvaru je rozdeleni?
  #   priblizne symetricke, mirne asymetricke, extremne asymetricke, tvaru U?
boxplot(por.hmotnost)
  # krabicovy graf
  # co grafu chybi?
  # je spravne vykreslovat odlehle hodnoty?
  # jak se odlehle hodnoty urci, a kdy ma smysl je do grafu kreslit?
  boxplot(por.hmotnost,main="Krabicovy graf",ylab="Porodni hmotnost",col="yellow",border="orange")
  points(c(1,1),lowx[c(1,2)],col="white")
  abline(h=lowx[3],col="white",lwd=2)
  lines(c(1,1),lowx[c(1,3)],col="orange",lty=2)
  lines(c(0.9,1.1),lowx[c(1,1)],col="orange")
    # krabicovy graf s tykadlem az k poslednimu odlehlemu pozorovani
    #   umite-li lepe, sem s tim :)
  
# Ciselne i graficke charakteristiky pro promenne vyska a vek matky.
# Ciselne i graficke charakteristiky pro pocet deti.
  