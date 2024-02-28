
# nactete databazi Stulong
names(Stulong)<-c("ID","vyska","vaha","syst1","syst2","chlst","vino","cukr",
	"bmi","vek","KOURrisk","Skupina","VekK")

library(DescTools)
   # aktivuje knihovnu

#######################
### Hodnoceni normality
# Jak zjistit, zda data maji ci nemaji normalni rozdeleni

# Ohodnotte normalitu promenne vaha
prom1<-Stulong$vaha

# prvni napovi histogram
hist(prom1,main="Histogram",xlab="Vaha v kg",ylab="Absolutni cetnosti",
    col="skyblue",border="darkblue")

# je mozne prokreslit i hustotu normalniho rozdeleni 
hist(prom1,main="Histogram",xlab="Vaha v kg",ylab="Hustota",
    col="skyblue",border="darkblue",freq=F)
    # je treba vykreslit histogram prepocteny na hustotu
lines(x<-seq(50,140,by=0.2),dnorm(x,mean(prom1),sd(prom1)),col=2)

# k hodnoceni pomohou i sikmost a spicatost
Skew(prom1)
Kurt(prom1)
    # oboji se vyrazne lisi od 0

# Q-Q plot - Quantile Comparison plot
PlotQQ(prom1)
    # je videt mirne sesikmeni a prilis tezky chvost ve vysokych hodnotach

# diky tezkemu chvostu data nemaji mormalni rozdeleni

# jsou v datech odlehle hodnoty?
Q1<-quantile(prom1,1/4)
Q3<-quantile(prom1,3/4)
    # kvantily do promennych

# kandidati na odlehle hodnoty
(mez<-Q3+1.5*IQR(prom1))
prom1[prom1>mez]
# odlehle hodnoty
(mez<-Q3+3*IQR(prom1))
prom1[prom1>mez]

# podle histogramu normalite vadi hodnoty vyssi nez 110
# jak vypada Q-Q Plot bez techto hodnot? 
PlotQQ(prom1[prom1<=110]) 
   # z grafu je videt mirne sesikmeni

# jak omezit vliv odlehlych hodnot na prumer?
# spoctete useknuty prumer tak, ze vynechate hodnoty vetsi nez 110
sum(prom1>110)/length(prom1)
   # procento hodnot vetsich nez 110
mean(prom1,trim=0.0116)
   # jak se lisi od obycejneho prumeru
   mean(prom1)
   
prom1.ln <- log(prom1)

## Jak je to s normalitou promenne syst1?
#  Jak urcit odlehle hodnoty? A ma smysl uvazovat useknuty prumer?
#  Jaky urcit bod useknuti?

## Obdobne pro promennou cukr.
#  Jakou uvazovat normalizujici transformaci? Mohu puzit logaritmus?
prom1.tr<-sqrt((prom1-min(prom1))/(max(prom1)-min(prom1)))

#############################
### Vztah dvou promennych 

#############################
## Vztah dvou kategorickych promennych popisujeme kontingencni tabulkou
# Souvisi spolu skupina a koureni?
prom2<-Stulong$Skupina
prom3<-Stulong$KOURrisk
(tab<-table(prom2,prom3))
# mozna lepe videt z relativnich cetnosti
addmargins(prop.table(tab))
addmargins(prop.table(tab,1))
addmargins(prop.table(tab,2))
  # ktery typ relativnich cetnosti se pro popis zavislosti hodi nejvic?
  # Jak byste vztah popsali?

# Souvisi spolu skupina a konzumace vina?
# A co skupina a vek?

#############################
## Vztah dvou ciselnych promennych popisujeme pomoci
#   bodoveho (rozptyloveho) grafu, korelacniho koeficientu, korelacni tabulky

# Jaky je vztah mezi vahou a vyskou?
prom4<-Stulong$vaha
prom5<-Stulong$vyska
plot(prom4~prom5,pch=19,main="Rozptylovy graf",xlab="výška v cm",ylab="váha v kg")
  # Co z grafu vidite?

cov(prom4,prom5,use="complete.obs")
  # co Vam rika kovariance
cor(prom4,prom5,use="complete.obs")
  # a co korelace (měří lineární vztah -1, přímka klesá - 1, přímka jede hore)

# A co je korelacni tabulka?
prom4.c <- factor(ifelse(prom4<=60, '<60', ifelse(prom4<=70, '60-70', 
                  ifelse(prom4<=80, '70-80', ifelse(prom4<=90, '80-90', 
                  ifelse(prom4<=100, '90-100',ifelse(prom4<=110, '100-110',
                  ifelse(prom4<=120, '110-120',ifelse(prom4<=130, '120-130','>130')))))))),
                  levels=c("<60","60-70","70-80","80-90","90-100","100-110","110-120","120-130",">130")) 
prom5.c <- factor(ifelse(prom5<=160, '<160', ifelse(prom5<=165, '160-165', 
                  ifelse(prom5<=170, '165-170', ifelse(prom5<=175, '170-175',
                  ifelse(prom5<=180, '175-180', ifelse(prom5<=185, '180-185',
                  ifelse(prom5<=190,'185-190',ifelse(prom5<=195,'190-195','>195')))))))),
                  levels=c("<160","160-165","165-170","170-175","175-180","180-185","185-190","190-195",">195"))
table(prom4.c,prom5.c)

# A co je korelacni matice?
cor(Stulong[,c(2:6,8:10)])

# Souvisi spolu vyska a Body mass index? Pripadne jak?
# A co vaha a BMI?
# Ktera promenna s ostatnimi temer nesouvisi? A meyi kterymi dvema promennymi je zavislost nejsilnejsi?


