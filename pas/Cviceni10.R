# Potrebne knihovny
library(fitdistrplus)
library(DescTools)

# Nactete databazi Stulong.RData
names(Stulong)<-c("ID","vyska","vaha","syst1","syst2","chlst","vino","cukr","bmi",
                  "vek","KOURrisk","Skupina","VekK")

###########################
### Odhad rozdeleni

## Jake rozdeleni ma cholesterol? 
prom1<-Stulong$chlst
hist(prom1,col="azure",main="Histogram pro hladinu cholesterolu",xlab="Cholesterol")
PlotQQ(prom1)
Skew(prom1)
Kurt(prom1)

# Pomer sikmosti a spicatosti, k jakemu rozdeleni mam nejblize
descdist(prom1,discrete=FALSE,boot=1000)

# zkusim normalni, logisticke a lognormalni
(fit1<-fitdist(prom1,"norm"))
(fit2<-fitdist (prom1,"logis"))
(fit3<-fitdist (prom1+0.000000000000000000000000000001,"lnorm"))
(fit4<-fitdist (prom1+0.000000000000000000000000000001,"gamma"))
(fit5<-fitdist (prom1,"exp"))

# porovnani pomoci AIC a BIC kriterii
data.frame(distr=c("Norm","Logis","Lognorm","Gamma","Exp"),
           AIC=c(fit1$aic,fit2$aic,fit3$aic,fit4$aic,fit5$aic),
           BIC=c(fit1$bic,fit2$bic,fit3$bic,fit4$bic,fit5$bic)
)
  # nejnizsi hodnoty AIC i BIC ma lognormalni rozdeleni

# 95%-ni intervaly spolehlivosti pro odhady parametru lognormalniho rozdeleni
data.frame(estimate=coef(fit3),
           ci0.95lo=coef(fit3)-1.96*fit3$sd,
           ci0.95up=coef(fit3)+1.96*fit3$sd
)

data.frame(estimate=coef(fit5),
           ci0.95lo=coef(fit5)-1.96*fit5$sd,
           ci0.95up=coef(fit5)+1.96*fit5$sd
)

# kontrola, jak jednotlivym rozdelenim sedi Q-Q plot
PlotQQ(prom1)
PlotQQ(prom1,qdist=function(p) qlogis(p,location=coef(fit2)[1],scale=coef(fit2)[2]))
PlotQQ(prom1,qdist=function(p) qlnorm(p,meanlog=coef(fit3)[1],sdlog=coef(fit3)[2]))
PlotQQ(prom1,qdist=function(p) qexp(p,rate=coef(fit5)[1]))
  # lognormalni rozdeleni sedi nejlepe

## Jakym rozdelenim se ridi systolicky tlak 1 (syst1)?
## Jakym rozdelenim se ridi cukr?

######################
## Interval spolehlivosti

# Nactete data Policie.RData
## Spoctete 95% interval spolehlivosti pro prumer vysky kdyz vite, 
#  ze rozptyl vysky dospelych muzu je 49.
prom2<-Policie$height

# Rucni vypocet
mn<-mean(prom2)
sd<-sqrt(49)
n<-length(prom2)
alpha<-0.05
q.n<-qnorm(1-alpha/2)

# Dolni mez
mn-q.n*sd/sqrt(n)
# Horni mez
mn+q.n*sd/sqrt(n)

# vypocet pomoci funkce v knihovne DescTools
MeanCI(prom2,sd=sd)

# a kdyby mi skutecny rozptyl nikdo nerekl?
# Rucni vypocet
mn<-mean(prom2)
sd<-sd(prom2)
n<-length(prom2)
alpha<-0.05
q.t<-qt(1-alpha/2,n-1)

# Dolni mez
mn-q.t*sd/sqrt(n)
# Horni mez
mn+q.t*sd/sqrt(n)

# vypocet pomoci funkce v knihovne DescTools
MeanCI(prom2)
  # dostavam sirsi interval nez predtim, proc?

## Spoctete 99%-ni interval spolehlivosti pro stredni hodnotu reakcni doby,
#  kdyz vim, ze rozptyl je 0.015 a kdyz nevim nic.
MeanCI( , conf.level=0.99)

###################################
## A jak spocitat interval spolehlivosti pro stredni hodnotu reakcni doby 
#  pomoci bootstrapu?
prom3<-Policie$react

boots<-list()
B<-10000
for(i in 1:B) boots[[i]]<-sample(prom3,replace=TRUE)
  # bootstrapove vybery
means<-unlist(lapply(boots,mean))
  # bootstrapove prumery

hist(means,col="honeydew2",xlab="reakcni doba",
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