######################
## Interval spolehlivosti

library(DescTools)
  # aktivace knihovny

# Nactete data Kojeni2.RData
## Spoctete 95% interval spolehlivosti pro prumer vysky muzu (otcu) kdyz vite, 
#  ze rozptyl vysky dospelych muzu je 49.
prom1<-Kojeni2$vyskaO

# Rucni vypocet
mn<-mean(prom1)
sd<-sqrt(49) #směrodatná oddchylka je odmocnina z rozptylu
n<-length(prom1)
alpha<-0.05
q.n<-qnorm(1-alpha/2)
# známý rozptyl => qnorm; neznámý rozptyl => qt

# Dolni mez
mn-q.n*sd/sqrt(n)
# Horni mez
mn+q.n*sd/sqrt(n)

# vypocet pomoci funkce v knihovne DescTools
MeanCI(prom1,sd=sd)

# a kdyby mi skutecny rozptyl nikdo nerekl?
# Rucni vypocet
mn<-mean(prom1)
sd<-sd(prom1)
n<-length(prom1)
alpha<-0.05
q.t<-qt(1-alpha/2,n-1)

# Dolni mez
mn-q.t*sd/sqrt(n)
# Horni mez
mn+q.t*sd/sqrt(n)

# vypocet pomoci funkce v knihovne DescTools
MeanCI(prom1)
# dostavam sirsi interval nez predtim, proc?

## Spoctete 99%-ni interval spolehlivosti pro stredni hodnotu porodni hmotnosti deti,
#  kdyz vim, ze smerodatna odchylka porodni hmotnosti je 440 g a kdyz nevim nic.
prom.hmot<-Kojeni2$por.hmotnost

mn<-mean(prom.hmot)
sd<-440
n<-length(prom.hmot)
alpha<-0.01
q.n<-qnorm(1-alpha/2)
mn-q.n*sd/sqrt(n)
mn+q.n*sd/sqrt(n)

MeanCI(prom.hmot, sd=440, conf.level=0.99)

MeanCI(prom.hmot, conf.level=0.99)

## Na jedno-procentni hladine vyznamnosti rozhodnete, zda se muze 
#  stredni hodnota porodni hmotnosti deti rovnat 3 kg?
# A co treba 3.1 kg, 3.2 kg, 3.3 kg, 3.4 kg, 3.5 kg, 3.6 kg?

## Je 95%-ni interval spolehlivosti pro porodni hmotnost stejny u divek a u hochu?
prom2<-Kojeni2$Hoch
tapply(prom.hmot,prom2,MeanCI)
library(gplots)
plotmeans(prom.hmot~prom2)

## A je mezi divkami a hochy vyznamny rozdil (na petiprocentni hladine vyznamnosti)?
MeanDiffCI(prom.hmot~prom2)

# A co rozdil na jednoprocentni hladine vyznamnosti?
MeanDiffCI(prom.hmot~prom2,conf.level=0.99)

## Vypoctete a interpretujte 95%-ni interval spolehlivosti 
#  pro hmotnost deti v pul roce (promenna hmotnost).
#  Lisi se prumerna hmotnost pulrocnich deti, ktere byly jeste
#  jeste v pul roce kojeny a tech, co uz kojeny nebyly (promenna Koj24)?
prom.hmotpornot<-Kojeni2$hmotnost
prom.koj24<-Kojeni2$Koj24
tapply(prom.hmotpornot,prom.koj24,MeanCI)
plotmeans(prom.hmotpornot~prom.koj24)

MeanDiffCI(prom.hmotpornot~prom.koj24)
MeanDiffCI(prom.hmotpornot~prom.koj24, conf.level=0.99)

###################################
## A jak spocitat interval spolehlivosti pro stredni hodnotu hmotnosti v pul roce 
#  pomoci bootstrapu?
prom3<-Kojeni2$hmotnost

boots<-list()
B<-10000
for(i in 1:B) boots[[i]]<-sample(prom3,replace=TRUE)
  # bootstrapove vybery
means<-unlist(lapply(boots,mean))
  # bootstrapove prumery

hist(means,col="honeydew2",xlab="hmotnost",
     main="Histogram bootstrapovych prumeru")
abline(v=mean(prom3),lwd=3,col="navy")
abline(v=quantile(means,probs=c(0.025,0.975)),lwd=3,col="red")
c(mean=mean(prom3),quantile(means,probs=c(0.025,0.975)))
  # bootstrapovy interval spolehlivosti

# Pomoci funkce
MeanCI(prom3,method="boot")
BootCI(prom3,FUN = mean)
# pomoci parametru FUN mohu pocitat interval spolehlivosti pro libovolnou funkci
decil <- function(x) quantile(x,0.1)  
BootCI(prom3,FUN = decil)
  # bootstrapovy interval spolehlivosti pro dolni decil

######################
### Interval spolehlivosti pro podil

## Spoctete a interpretujte 95%-ni interval spolehlivosti pro 
#  podil otcu pritomnych u porodu

prom4<-Kojeni2$Otec
table(prom4)
  # u porodu bylo pritomno 36 otcu
round(prop.table(table(prom4)),4)*100
  # coz je 36.36%
BinomCI(table(prom4)[2],sum(table(prom4)))
BinomCI(36,99)
  # 95%-ni interval spolehlivosti pro podil/pravdepodobnost

## Spoctete 90%-ni interval spolehlivosti pro podil deti, 
#  ktere byly jeste v pul roce kojeny (promenna Koj24)
table(prom.koj24)
round(prop.table(table(prom.koj24)),4)*100

BinomCI(table(prom.koj24)[2],sum(table(prom.koj24)), conf.level = 0.9)

#####################
### Linearni regrese

## Souvisi spolu porodni hmotnost a delka deti?
prom5<-Kojeni2$por.hmotnost
prom6<-Kojeni2$por.delka

# nejprve graf
plot(prom5~prom6, pch=19,main="Graf zavislosti dvou ciselnych promennych",
     xlab="Porodni delka",ylab="Porodni hmotnost")
  # zavislost je zrejma

# korelacni koeficient
cor(prom5,prom6)
  # silna kladna/rostouci/prima zavislost

# linearni regrese
abline(lm(prom5~prom6),col=2,lwd=2)
  # grafem prolozim primku
(Model1<-lm(prom5~prom6))
  # odhad regresnich koeficientu - popis primky
  # porodni hmotnost = -7905.8 + 224.8*porodni delka
  #   je dulezite, ktera promenna je na x-ove a ktera na y-ove ose
  # s narustem porodni delky o 1cm naroste porodni hmotnost v promeru o 224.8 g
summary(Model1)
  # souhrn vystupu
  # v casti Coefficients najdeme odhady regresnich koeficientu (Estimate)
  #   a dale test o jejich nulovosti (kdyz je posledni hodnota v radku
  #   Pr(>|t|) mensi nez 0.05, pak se koeficient vyznamne lisi od nuly,
  #   a za posledni hodnotou se objevi alespon jedna hvezdicka)
  # v souhrnu Multiple R-squared je procento variability zavisle promenne 
  #   vysvetlene modelem> z variability porodni hmotnosti se vysvetlilo 62.5%
confint(Model1)
  # 95% intervaly spolehlivosti pro regresni koeficienty
  # ani jeden z nich neobsahuje nulu

## Jak zavisi hmotnost v pul roce na porodni hmotnosti?
prom5<-Kojeni2$hmotnost
prom6<-Kojeni2$por.hmotnost

plot(prom5~prom6, pch=19,main="Graf zavislosti dvou ciselnych promennych",
     xlab="Porodní hmotnost",ylab="Hmotnost")
abline(lm(prom5~prom6),col=2,lwd=2)
cor(prom5,prom6)

(Model2<-lm(prom5~prom6))
  # hmotnost = 4839.3 + 0.8215*porodni hmotnost
  #   na jeden gram porodni hmotnosti pripada v prumeru 0.8215 gramu hmotnosti v pul roce
summary(Model2)
  # modelem se vysvetlilo 18.4% variability hmotnosti v pul roce
confint(Model2)
  # linerarni koeficient se vyznamne lisi od nuly

## A co kdyz do modelu pridame jeste delku?
#  Zavisi hmotnost deti v pul roce na jejich delce a na porodni hmotnosti?
prom7<-Kojeni2$delka

(Model3<-lm(prom5~prom6+prom7))
  # hmotnost = -413 + 0.5837*porodni hmotnost + 88.66*delka
  #   na jeden gram porodni hmotnosti pripada v prumeru 0.5837 gramu hmotnosti v pul roce
  #      pri stejne delce v pul roce
  #   na jeden centimetr delky v pul roce pripada v prumeru 88.66 gramu hmotnosti
  #      pri stejne porodni hmotnosti
summary(Model3)
  # modelem se vysvetlilo 28.7% variability hmotnosti v pul roce
  # pridani dalsi promenne vyrazne pomohlo
confint(Model3)
  # zadny z intervalu spolehlivosti pro linearni cleny neobsahuje nulu
  #   obe promenne jsou v modelu vyznamne

## A kdyz pridame do modelu jeste porodni delku?
prom8<-Kojeni2$por.delka

(Model4<-lm(prom5~prom6+prom7+prom8))
summary(Model4)
confint(Model4)
  # koeficienty se po pridani kazde dalsi promenne meni
  # pridanim porodni delky do modelu jsme si uskodili
  #  a vysvetlili jsme jen 29.3% variability hmotnosti
  
## Zavisi BMI pulrocnich deti na jejich porodni hmotnosti, delce v pul roce a veku matky?
