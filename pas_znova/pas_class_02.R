
Sys.setlocale("LC_ALL","Slovak")

################################################################################
################################################################################

## reading the data into R
require(XLConnect)
d0<-readWorksheet(loadWorkbook("classes02+_2024-10-09.xlsx"),sheet="data",header=TRUE)
str(d0)


## setting the numeric format manually
## the first two variables --> character
## all others --> numeric
rep(c("character","numeric"),c(2,12))
## or:
c("character","character","numeric","numeric","numeric",
  "numeric","numeric","numeric","numeric","numeric",
  "numeric","numeric","numeric","numeric")
## or:
c(rep("character",2),rep("numeric",12))


## reading the data into R correctly
d0<-readWorksheet(loadWorkbook("classes02+_2024-10-09.xlsx"),sheet="data",header=TRUE,
                  colTypes=rep(c("character","numeric"),c(2,12)))
str(d0)

################################################################################
##------------------------------------------------------------------------------

# id: identfifikačné číslo pozostávajúce z roku pozorovania a čísla spoločnosti 
# company: skratka názvu spoločnosti
# year: ročné obdobie, na ktoré sa vzťahujú dáta
# no_employees: priemerný prepočítaný počet zamestnancov
# assets_fixed: fixné aktíva na konci ročného obdobia (tis. Kč)
# pipeline_distrib_length: dĺžka vodovodnej distribučnej siete (km)
# costs: celkové prevádzkové náklady (tis. Kč)
# no_citizens: počet zásobovaných obyvateľov
# water_billed: objem fakturovanej vody (tis. m3)
# water_cleansed: objem vyčistenej odpadovej vody (tis. m3)
# sew2supply: podiel kanalizačnej k distribučnej sieti
# pop_density: hustota obyvateľstva v zásobovaných oblastiach (obyvatelia na km2)
# D: identifikátor prevládajúceho municipálneho vlastníctva (D = 1)
# F: identifikátor prevládajúceho zahraničného vlastníctva (F = 1)

str(d0)
head(d0) # tail(d0)
class(d0)
# four basic options how to extract a variable from a data frame:
d0$company
d0[,"company"]
d0[,2]
with(d0,company)
# another option: to be avoided
# attach(d0); company; detach(d0)


#########################################
# Task A ################################

## kvázi technické premenné:
# id: nominálny znak, ktorý sa ale neanalyzuje, ale používa sa na identifikáciu jednotlivých pozorovaní
# company: nominálny znak, ktorý vlastne identifikuje jednotlivé objekty (VH spoločnosti)
# year: ordinálny znak, ktorý usporadúva časy jednotivých pozorovaní (pre rôzne objekty)
## premenné, ktoré sú predmetom záujmu:
# no_employees: numerický znak, spojitý
# no_employees: histogram, graf jadrového odhadu hustoty, boxplot
# assets_fixed: numerický znak, spojitý
# assets_fixed: histogram, graf jadrového odhadu hustoty, boxplot
# pipeline_distrib_length: numerický znak, spojitý
# pipeline_distrib_length: histogram, graf jadrového odhadu hustoty, boxplot
# costs: numerický znak, spojitý
# costs: histogram, graf jadrového odhadu hustoty, boxplot
# no_citizens: numerický, diskrétny (spracúva sa s obmedzeniami ako spojitý znak pre veľký # obmien)
# no_citizens: histogram, boxplot
# water_billed: numerický znak, spojitý
# water_billed: histogram, graf jadrového odhadu hustoty, boxplot
# water_cleansed: numerický znak, spojitý
# water_cleansed: histogram, graf jadrového odhadu hustoty, boxplot
# sew2supply: numerický znak, spojitý
# sew2supply: histogram, graf jadrového odhadu hustoty, boxplot
# pop_density: numerický znak, spojitý
# pop_density: histogram, graf jadrového odhadu hustoty, boxplot
# D: nominálny znak, dichotomický
# D: stĺpcový graf, koláčový graf
# F: nominálny znak, dichotomický
# F: stĺpcový graf, koláčový graf

#########################################
# Task B - costs ########################

d0$costs

## costs HISTOGRAM - bežné početnosti
hist(d0$costs,xlab="Výška nákladov (tis. Kč)", 
     ylab="Frekvencia (# rokospoločností)",
     main="Rozdelenie celkových prevádzkových nákladov") 
(hh<-hist(d0$costs,plot=FALSE))
## costs HISTOGRAM - absolútne početnosti
hist(d0$costs/1000,xlab="Výška nákladov (mil. Kč)", 
     ylab="Frekvencia (# rokospoločností)",
     main="Rozdelenie celkových prevádzkových nákladov")
(hh<-hist(d0$costs/1000,plot=FALSE))
## costs HISTOGRAM - relatívne početnosti
hist(d0$costs/1000,xlab="Výška nákladov (mil. Kč)", 
     ylab="Relatívna frekvencia",
     main="Rozdelenie celkových prevádzkových nákladov",
     probability=TRUE)
(hh<-hist(d0$costs/1000,plot=FALSE,probability=TRUE))


## costs JADROVÝ ODHAD HUSTOTY
density(d0$costs/1000)
density(d0$costs/1000,na.rm=TRUE)
## or:
density(na.omit(d0$costs/1000))
plot(density(d0$costs/1000,bw=1000,na.rm=TRUE),xlab="Výška nákladov (mil. Kč)", 
     ylab="Relatívna frekvencia",
     main="Rozdelenie celkových prevádzkových nákladov"
)

#########################################
# Task B - year #########################

d0$year
table(d0$year)

pie(table(d0$year),main="Zastúpenie rokov vo vzorke")
barplot(table(d0$year),ylab="Počet spoločností",main="Zastúpenie rokov vo vzorke")

#########################################
# Task B - D ############################

d0$D
table(d0$D)

pie(table(d0$D),main="Prevládajúce municipálne vlastníctvo")
pie(table(d0$D),main="Prevládajúce municipálne vlastníctvo",labels=c("nie","áno"))
barplot(table(d0$D),names.arg=c("nie","áno"),ylab="Počet rokospoločností",main="Prevládajúce municipálne vlastníctvo")


#########################################
# Task C ################################

library(psych)
describe(d0)


#########################################
# Task D ################################

# no_employees: priemerný prepočítaný počet zamestnancov
win.graph(width=10,height=3)
op<-par(mfrow=c(1,3),mar=c(5,4,4,1)) # mar = c(bottom, left, top, right)
boxplot(d0$no_employees,range=Inf,
        main="Distribution of employee numbers",
        horizontal=TRUE,xlab="Average # of employees")
hist(d0$no_employees,
     main="Distribution of employee numbers",
     xlab="Average # of employees",ylab="Number of company-years")
hist(d0$no_employees,prob=TRUE,ylim=c(0,0.002),
     main="Distribution of employee numbers",
     xlab="Average # of employees",ylab="Density")
lines(dens<-density(d0$no_employees),col="red")
abline(v=dens$x[which.max(dens$y)],col="navyblue",lwd=2)
dens$x[which.max(dens$y)]
par(op)
dev.off()

# assets_fixed: fixné aktíva na konci ročného obdobia (tis. Kč)
win.graph(width=10,height=3)
op<-par(mfrow=c(1,3),mar=c(5,4,4,1)) # mar = c(bottom, left, top, right)
boxplot(d0$assets_fixed,range=Inf,
        main="Distribution of fixed assets",
        horizontal=TRUE,xlab="Fixed assets (thousands of Kč)")
hist(d0$assets_fixed,
     main="Distribution of fixed assets",
     xlab="Fixed assets (thousands of Kč)",ylab="Number of company-years")
hist(d0$assets_fixed,prob=TRUE,
     main="Distribution of fixed assets",
     xlab="Fixed assets (thousands of Kč)",ylab="Density")
lines(dens<-density(d0$assets_fixed,bw=5e+05),col="red")
abline(v=dens$x[which.max(dens$y)],col="navyblue",lwd=2)
dens$x[which.max(dens$y)]
par(op)
dev.off()

# pipeline_distrib_length: dĺžka vodovodnej distribučnej siete (km)
win.graph(width=10,height=3)
op<-par(mfrow=c(1,3),mar=c(5,4,4,1)) # mar = c(bottom, left, top, right)
boxplot(d0$pipeline_distrib_length,range=Inf,
        main="Distribution of pipelines length",
        horizontal=TRUE,xlab="Pipeline distribution length (kms)")
hist(d0$pipeline_distrib_length,
     main="Distribution of pipelines length",
     xlab="Pipeline distribution length (kms)",ylab="Number of company-years")
hist(d0$pipeline_distrib_length,prob=TRUE,ylim=c(0,5e-04),
     main="Distribution of pipelines length",
     xlab="Pipeline distribution length (kms)",ylab="Density")
lines(dens<-density(d0$pipeline_distrib_length,na.rm=TRUE),col="red")
abline(v=dens$x[which.max(dens$y)],col="navyblue",lwd=2)
dens$x[which.max(dens$y)]
par(op)
dev.off()

# costs: celkové prevádzkové náklady (tis. Kč)
# no_citizens: počet zásobovaných obyvateľov
# water_billed: objem fakturovanej vody (tis. m3)
# water_cleansed: objem vyčistenej odpadovej vody (tis. m3)
# pop_density: hustota obyvateľstva v zásobovaných oblastiach (obyvatelia na km2)

# sew2supply: podiel kanalizačnej k distribučnej sieti
win.graph(width=10,height=3)
op<-par(mfrow=c(1,3),mar=c(5,4,4,1)) # mar = c(bottom, left, top, right)
boxplot(d0$sew2supply,range=Inf,
        main="Distribution of the sewage 2 supply ratio",
        horizontal=TRUE,xlab="Sewage to distribution pipelines length")
hist(d0$sew2supply,
     main="Distribution of the sewage 2 supply ratio",
     xlab="Sewage to distribution pipelines length",ylab="Number of company-years")
hist(d0$sew2supply,prob=TRUE,
     main="Distribution of the sewage 2 supply ratio",
     xlab="Sewage to distribution pipelines length",ylab="Density")
lines(dens<-density(d0$sew2supply,na.rm=TRUE,bw=0.09),col="red")
abline(v=dens$x[which.max(dens$y)],col="navyblue",lwd=2)
dens$x[which.max(dens$y)]
par(op)
dev.off()


#########################################
# Task E ################################

win.graph(width=10,height=3)
op<-par(mfrow=c(1,3),mar=c(5,4,4,1)) # mar = c(bottom, left, top, right)
# no_employees: priemerný prepočítaný počet zamestnancov
# assets_fixed: fixné aktíva na konci ročného obdobia (tis. Kč)
# pipeline_distrib_length: dĺžka vodovodnej distribučnej siete (km)
qqnorm(d0$no_employees,main="Gaussian QQ plot for no_employees")
qqline(d0$no_employees)
qqnorm(d0$assets_fixed,main="Gaussian QQ plot for assets_fixed")
qqline(d0$assets_fixed)
qqnorm(d0$pipeline_distrib_length,main="Gaussian QQ plot for pipeline_distrib_length")
qqline(d0$pipeline_distrib_length)
par(op)
dev.off()

win.graph(width=10,height=3)
op<-par(mfrow=c(1,3),mar=c(5,4,4,1)) # mar = c(bottom, left, top, right)
# costs: celkové prevádzkové náklady (tis. Kč)
# no_citizens: počet zásobovaných obyvateľov
# water_billed: objem fakturovanej vody (tis. m3)
qqnorm(d0$costs,main="Gaussian QQ plot for costs")
qqline(d0$costs)
qqnorm(d0$no_citizens,main="Gaussian QQ plot for no_citizens")
qqline(d0$no_citizens)
qqnorm(d0$water_billed,main="Gaussian QQ plot for water_billed")
qqline(d0$water_billed)
par(op)
dev.off()

win.graph(width=10,height=3)
op<-par(mfrow=c(1,3),mar=c(5,4,4,1)) # mar = c(bottom, left, top, right)
# water_cleansed: objem vyčistenej odpadovej vody (tis. m3)
# sew2supply: podiel kanalizačnej k distribučnej sieti
# pop_density: hustota obyvateľstva v zásobovaných oblastiach (obyvatelia na km2)
qqnorm(d0$water_cleansed,main="Gaussian QQ plot for water_cleansed")
qqline(d0$water_cleansed)
qqnorm(d0$sew2supply,main="Gaussian QQ plot for sew2supply")
qqline(d0$sew2supply)
qqnorm(d0$pop_density,main="Gaussian QQ plot for pop_density")
qqline(d0$pop_density)
par(op)
dev.off()


#########################################
# Task F ################################

# no_employees: priemerný prepočítaný počet zamestnancov
## first checking symmetry
hist(d0$no_employees) 
# asymmetric --> must be log-transformed
hist(log(d0$no_employees))
# roughly symmetric
## working with logarithimized data
x<-log(d0$no_employees)
## INTERQUARTILE RANGE METHOD
quarts<-quantile(x,probs=c(0.25,0.50,0.75))
lo1<-as.numeric(quarts[1]-3*(quarts[3]-quarts[1]))
lo2<-as.numeric(quarts[1]-1.5*(quarts[3]-quarts[1]))
up2<-as.numeric(quarts[3]+1.5*(quarts[3]-quarts[1]))
up1<-as.numeric(quarts[3]+3*(quarts[3]-quarts[1]))
stripchart(x,pch=19,method="jitter",col=4,xlim=range(c(x,lo1,up1)))
abline(v=c(lo1,lo2,up2,up1),col="red")
outs1<-d0$id[which(x<=lo1|x>=up1)]
outs2<-d0$id[which(x<=lo2|x>=up2)]
d0[d0$id%in%c(outs1,outs2),] # which out
mean(d0$no_employees)
mean(d0[!(d0$id%in%c(outs1,outs2)),]$no_employees)
## Z-SCORE METHOD
zscores<-scale(x)
stripchart(zscores,pch=19,method="jitter",col=4,xlim=range(c(zscores,-4,4)))
abline(v=c(-4,-3,3,4),col="red")
outs1<-d0$id[which(abs(zscores)>=4)]
outs2<-d0$id[which(abs(zscores)>=3&abs(zscores)<4)]
d0[d0$id%in%c(outs1,outs2),] # which out
mean(d0$no_employees)
mean(d0[!(d0$id%in%c(outs1,outs2)),]$no_employees)

# assets_fixed: fixné aktíva na konci ročného obdobia (tis. Kč)
## first checking symmetry
hist(d0$assets_fixed) 
# asymmetric --> must be log-transformed
hist(log(d0$assets_fixed))
# roughly symmetric
## working with logarithimized data
x<-log(d0$assets_fixed)
## INTERQUARTILE RANGE METHOD
quarts<-quantile(x,probs=c(0.25,0.50,0.75))
lo1<-as.numeric(quarts[1]-3*(quarts[3]-quarts[1]))
lo2<-as.numeric(quarts[1]-1.5*(quarts[3]-quarts[1]))
up2<-as.numeric(quarts[3]+1.5*(quarts[3]-quarts[1]))
up1<-as.numeric(quarts[3]+3*(quarts[3]-quarts[1]))
stripchart(x,pch=19,method="jitter",col=4,xlim=range(c(x,lo1,up1)))
abline(v=c(lo1,lo2,up2,up1),col="red")
outs1<-d0$id[which(x<=lo1|x>=up1)]
outs2<-d0$id[which(x<=lo2|x>=up2)]
d0[d0$id%in%c(outs1,outs2),] # which out
mean(d0$assets_fixed)
mean(d0[!(d0$id%in%c(outs1,outs2)),]$assets_fixed)

## Z-SCORE METHOD
zscores<-scale(x)
stripchart(zscores,pch=19,method="jitter",col=4,xlim=range(c(zscores,-4,4)))
abline(v=c(-4,-3,3,4),col="red")
outs1<-d0$id[which(abs(zscores)>=4)]
outs2<-d0$id[which(abs(zscores)>=3&abs(zscores)<4)]
d0[d0$id%in%c(outs1,outs2),] # which out
mean(d0$assets_fixed)
mean(d0[!(d0$id%in%c(outs1,outs2)),]$assets_fixed)

# pipeline_distrib_length: priemerný prepočítaný počet zamestnancov
## first checking symmetry
hist(d0$pipeline_distrib_length) 
# asymmetric --> must be log-transformed
hist(log(d0$pipeline_distrib_length))
# roughly symmetric
## working with logarithimized data
x<-log(d0$pipeline_distrib_length)
## INTERQUARTILE RANGE METHOD
quarts<-quantile(x,probs=c(0.25,0.50,0.75),na.rm=TRUE)
lo1<-as.numeric(quarts[1]-3*(quarts[3]-quarts[1]))
lo2<-as.numeric(quarts[1]-1.5*(quarts[3]-quarts[1]))
up2<-as.numeric(quarts[3]+1.5*(quarts[3]-quarts[1]))
up1<-as.numeric(quarts[3]+3*(quarts[3]-quarts[1]))
stripchart(x,pch=19,method="jitter",col=4,xlim=range(c(x,lo1,up1),na.rm=TRUE))
abline(v=c(lo1,lo2,up2,up1),col="red")
outs1<-d0$id[which(x<=lo1|x>=up1)]
outs2<-d0$id[which(x<=lo2|x>=up2)]
d0[d0$id%in%c(outs1,outs2),] # which out
mean(d0$pipeline_distrib_length,na.rm=TRUE)
mean(d0[!(d0$id%in%c(outs1,outs2)),]$pipeline_distrib_length,na.rm=TRUE)
## Z-SCORE METHOD
zscores<-scale(x)
stripchart(zscores,pch=19,method="jitter",col=4,xlim=range(c(zscores,-4,4),na.rm=TRUE))
abline(v=c(-4,-3,3,4),col="red")
outs1<-d0$id[which(abs(zscores)>=4)]
outs2<-d0$id[which(abs(zscores)>=3&abs(zscores)<4)]
d0[d0$id%in%c(outs1,outs2),] # which out
mean(d0$pipeline_distrib_length,na.rm=TRUE)
mean(d0[!(d0$id%in%c(outs1,outs2)),]$pipeline_distrib_length,na.rm=TRUE)

# costs: celkové prevádzkové náklady (tis. Kč)
# no_citizens: počet zásobovaných obyvateľov
# water_billed: objem fakturovanej vody (tis. m3)
# water_cleansed: objem vyčistenej odpadovej vody (tis. m3)
# pop_density: hustota obyvateľstva v zásobovaných oblastiach (obyvatelia na km2)

# sew2supply: podiel kanalizačnej k distribučnej sieti
## first checking symmetry
hist(d0$sew2supply) 
# symmetric --> NO NEED TO TRANSFORM
## working with ORIGINAL data
x<-d0$sew2supply
## INTERQUARTILE RANGE METHOD
quarts<-quantile(x,probs=c(0.25,0.50,0.75),na.rm=TRUE)
lo1<-as.numeric(quarts[1]-3*(quarts[3]-quarts[1]))
lo2<-as.numeric(quarts[1]-1.5*(quarts[3]-quarts[1]))
up2<-as.numeric(quarts[3]+1.5*(quarts[3]-quarts[1]))
up1<-as.numeric(quarts[3]+3*(quarts[3]-quarts[1]))
stripchart(x,pch=19,method="jitter",col=4,xlim=range(c(x,lo1,up1),na.rm=TRUE))
abline(v=c(lo1,lo2,up2,up1),col="red")
outs1<-d0$id[which(x<=lo1|x>=up1)]
outs2<-d0$id[which(x<=lo2|x>=up2)]
d0[d0$id%in%c(outs1,outs2),]
mean(d0$sew2supply,na.rm=TRUE)
d0[d0$id%in%c(outs1,outs2),] # which out
mean(d0$sew2supply,na.rm=TRUE)
mean(d0[!(d0$id%in%c(outs1,outs2)),]$sew2supply,na.rm=TRUE)## Z-SCORE METHOD
## Z-SCORE METHOD
zscores<-scale(x)
stripchart(zscores,pch=19,method="jitter",col=4,xlim=range(c(zscores,-4,4),na.rm=TRUE))
abline(v=c(-4,-3,3,4),col="red")
outs1<-d0$id[which(abs(zscores)>=4)]
outs2<-d0$id[which(abs(zscores)>=3&abs(zscores)<4)]
d0[d0$id%in%c(outs1,outs2),] # which out
mean(d0$sew2supply,na.rm=TRUE)
mean(d0[!(d0$id%in%c(outs1,outs2)),]$sew2supply,na.rm=TRUE)






#########################################
# Task G ################################

library(MASS)
library(psych)
bowley<-function(x){
  if(length(x<-na.omit(x))<10) stop("too few observations")
  q1=quantile(x,0.25)
  q2=quantile(x,0.50)
  q3=quantile(x,0.75)
  skb=(q3-2*q2+q1)/(q3-q1)
  return(as.numeric(skb))
}
moors<-function(x){
  if(length(x<-na.omit(x))<14) stop("too few observations")
  q1_8=quantile(x,0.125)
  q2_8=quantile(x,0.250)
  q3_8=quantile(x,0.375)
  q4_8=quantile(x,0.500)
  q5_8=quantile(x,0.625)
  q6_8=quantile(x,0.750)
  q7_8=quantile(x,0.875)
  kum=(q7_8-q5_8+q3_8-q1_8)/(q6_8-q2_8)
  return(as.numeric(kum))
}

# -------------------------------------------------------
# no_employees: priemerný prepočítaný počet zamestnancov
# -------------------------------------------------------

mean(d0$no_employees)
median(d0$no_employees)
huber(d0$no_employees)$mu
# the effect of trim size upon the trimmed mean:
xx<-c(0:50/100) # trimming percentages
yy<-sapply(xx,function(tr) mean(d0$no_employees,trim=tr)) # trimmed means
plot(x=xx,y=yy,type="l")
# there is no optimal choice of trim --> skewed distribution
# one would choose trim around 0.40
mean(d0$no_employees,trim=0.40) # mean of the 20% middle observations 


sd(d0$no_employees)
mad(d0$no_employees)

as.numeric(describe(d0$no_employees)["skew"])
bowley(d0$no_employees)
as.numeric(describe(d0$no_employees)["kurtosis"])
moors(d0$no_employees)

# -------------------------------------------------------
# assets_fixed: fixné aktíva na konci ročného obdobia (tis. Kč)
# -------------------------------------------------------

mean(d0$assets_fixed)
median(d0$assets_fixed)
huber(d0$assets_fixed)$mu
# the effect of trim size upon the trimmed mean:
xx<-c(0:50/100) # trimming percentages
yy<-sapply(xx,function(tr) mean(d0$assets_fixed,trim=tr)) # trimmed means
plot(x=xx,y=yy,type="l")
# one would choose trim around 0.20
mean(d0$assets_fixed,trim=0.20) # mean of the 60% middle observations 


sd(d0$assets_fixed)
mad(d0$assets_fixed)

as.numeric(describe(d0$assets_fixed)["skew"])
bowley(d0$assets_fixed)
as.numeric(describe(d0$assets_fixed)["kurtosis"])
moors(d0$assets_fixed)

# -------------------------------------------------------
# pipeline_distrib_length: dĺžka vodovodnej distribučnej siete (km)
# -------------------------------------------------------

mean(d0$pipeline_distrib_length,na.rm=TRUE)
median(d0$pipeline_distrib_length,na.rm=TRUE)
huber(d0$pipeline_distrib_length)$mu
# the effect of trim size upon the trimmed mean:
xx<-c(0:50/100) # trimming percentages
yy<-sapply(xx,function(tr) mean(d0$pipeline_distrib_length,trim=tr,na.rm=TRUE)) # trimmed means
plot(x=xx,y=yy,type="l")
# one would choose trim around 0.25
mean(d0$pipeline_distrib_length,trim=0.25,na.rm=TRUE) # mean of the 50% middle observations 


sd(d0$pipeline_distrib_length,na.rm=TRUE)
mad(d0$pipeline_distrib_length,na.rm=TRUE)

as.numeric(describe(d0$pipeline_distrib_length)["skew"])
bowley(d0$pipeline_distrib_length)
as.numeric(describe(d0$pipeline_distrib_length)["kurtosis"])
moors(d0$pipeline_distrib_length)

# -------------------------------------------------------
# costs: celkové prevádzkové náklady (tis. Kč)
# no_citizens: počet zásobovaných obyvateľov
# water_billed: objem fakturovanej vody (tis. m3)
# water_cleansed: objem vyčistenej odpadovej vody (tis. m3)
# sew2supply: podiel kanalizačnej k distribučnej sieti
# -------------------------------------------------------

# -------------------------------------------------------
# pop_density: hustota obyvateľstva v zásobovaných oblastiach (obyvatelia na km2)
# -------------------------------------------------------

mean(d0$pop_density)
median(d0$pop_density)
huber(d0$pop_density)$mu
# the effect of trim size upon the trimmed mean:
xx<-c(0:50/100) # trimming percentages
yy<-sapply(xx,function(tr) mean(d0$pop_density,trim=tr)) # trimmed means
plot(x=xx,y=yy,type="l")
# one would choose trim around 0.20
mean(d0$pop_density,trim=0.20) # mean of the 60% middle observations 


sd(d0$pop_density)
mad(d0$pop_density)

as.numeric(describe(d0$pop_density)["skew"])
bowley(d0$pop_density)
as.numeric(describe(d0$pop_density)["kurtosis"])
moors(d0$pop_density)