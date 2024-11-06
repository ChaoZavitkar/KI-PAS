

###############################################################################
# Task B ######################################################################
###############################################################################

library(datasets)
pairs(swiss[,-5],col=1+(swiss$Catholic>50),pch=20+(swiss$Catholic>50))


###############################################################################
# Task B ######################################################################
###############################################################################


# visits (# návštev pacientov na urgente u daného lekára) -- numeric, discrete
# complaints (# sťažností) -- numeric, discrete
# residency (Y/N – atestácia v procese áno/ atestácia hotová nie) -- nominal, dichotomic
# gender (M/F – muž/žena – podľa pohlavia lekára) -- nominal, dichotomic 
# revenue (mzda lekára v US$ na hodinu) -- numeric, continuous 
# hours (celkový počet odpracovaných hodín služieb) -- numeric, discrete

library(faraway)
data(esdcomp) # str(esdcomp)
d0<-with(esdcomp,data.frame(
  cperhour=complaints/hours,visits,
  residency=ifelse(residency=="Y",1,0),
  genderM=ifelse(gender=="M",1,0),revenue,hours)
) # str(d0)
pairs(d0) 

op<-par(ask=TRUE)
for(i in 2:ncol(d0)) plot(y=d0$cperhour,x=d0[,i],
                          xlab=colnames(d0)[i],ylab="complaints per hour")
par(op)

cor(d0)
cor(d0)[1,-1]


###############################################################################
# Task C ######################################################################
###############################################################################


# Uvažujeme štyri premenné A až D. Viete, že spomedzi nich je premenná A 
# konštantou. Okrem toho máte k dispozícii tieto informácie:
# •	Premenné B a C sú silne priamo korelované, premenné B a D sú slabo 
# priamo korelované a medzi C a D je slabá nepriama korelácia.
# •	Korelačné koeficienty medzi premennými B a C, B a D, C a D majú po 
# usporiadaní vzostupne tieto hodnoty: –0.20, 0.15, 0.75.
# •	Smerodajná odchýlka premennej B je 1, smerodajná odchýlka premennej C 
# je 4 a smerodajná odchýlka premennej D je 2. 
# Zostrojte kovariančnú maticu a napíšte, čomu sa bude rovnať smerodajná 
# odchýlka nových dvoch premenných definovaných vzťahmi: X = 3A + B a Y = C + D. 
# Načrtnite rozptylovú maticu, ktorá zodpovedá (aspoň zhruba) danej situácii. 
# Zároveň uveďte, medzi ktorými premennými (ak vôbec) možno na základe tejto 
# informácie preukazne (alebo aspoň indikatívne) tvrdiť, že jestvuje vzájomná 
# lineárna súvislosť?

# correlation matrix A, B, C, D
corm0<-diag(4)
rownames(corm0)<-colnames(corm0)<-LETTERS[1:4]
corm0[2,3]<-corm0[3,2]<-0.75
corm0[2,4]<-corm0[4,2]<-0.15
corm0[3,4]<-corm0[4,3]<-c(-0.20)

sds<-c(0,1,4,2)

# covariance matrix A, B, C, D
covm0<-diag(sds)%*%corm0%*%diag(sds)

# stdev of X = C + D
sqrt(1^2*covm0[3,3]+2*1*1*covm0[3,4]+1^2*covm0[4,4])

# stdev of Y = 3A + B
sqrt(3^2*covm0[1,1]+2*3*1*covm0[1,2]+1^2*covm0[2,2])

# a sketch of the situation
library(MASS)
pairs(mvrnorm(n=130,mu=rep(0,4),Sigma=covm0))

# on the basis of a correlation/covariance matrix one can never
# be sure that the relationship is actually linear.

###############################################################################
# Task D ######################################################################
###############################################################################

covmat0<-matrix(c(10,-3,2,-3,15,6,2,6,18),nrow=3,byrow=TRUE)
cov2cor(covmat0) # correlation matrix of the original variables
# but also the covariance/correlation matrix of the standardized variables

# variance & stdev of A + B
1^2*covmat0[1,1]+2*1*(-1)*covmat0[1,3]+(-1)^2*covmat0[3,3]	
sqrt(1^2*covmat0[1,1]+2*1*(-1)*covmat0[1,3]+(-1)^2*covmat0[3,3])

# variance & stdev of X = 2A – 6Б 
2^2*covmat0[1,1]+2*2*(-6)*covmat0[1,2]+(-6)^2*covmat0[2,2]
sqrt(2^2*covmat0[1,1]+2*2*(-6)*covmat0[1,2]+(-6)^2*covmat0[2,2])				 
