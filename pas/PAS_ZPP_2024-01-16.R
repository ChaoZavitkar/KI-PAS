library(DescTools)

hist(Math$Size, freq = F, breaks = 10, col = "white", xlab = "Pocet st ve tride", ylab = "Absolutni cetnost", main = "Size")
lines(density(Math$Size, bw=50))

meze<-hist(Math$Size,plot=F)$breaks
pocty<-hist(Math$Size,plot=F)$counts
m<-which.max(pocty)
(A<-meze[m])

(h<-meze[2]-meze[1])
(d0<-pocty[m]-pocty[m-1])
(d1<-pocty[m]-pocty[m+1])
(modus_Size<- A + h*d0/(d0+d1))

hist(Math$ACTM, col = "white", xlab = "ACT skore", ylab = "Hustota", main = "ACTM")

ind.num<-c(3,4,5,6,7,8,9)
ciselne<-Math[,ind.num]
cor(ciselne)
pairs(ciselne, pch=19)

prom1<-Math$GPAadj
prom2<-Math$PlcmtScore
(tab<-table(prom1,prom2))

prom1<-Math$PlcmtScore
PlotQQ(prom1,pch=19,cex=0.8)
hist(prom1)

MeanCI(Math$SATM)
MedianCI(Math$SATM)

pbinom(1,6,0.2)

k = 8 # 8 prasklých
w = 50 # 15 prasklých v balíku
b = 150 # 150 vajíček
n = 20 #tahám 20 vajíček
dhyper(k,w,b,n)


x = 2100# pravděpodobnost že menší než tohle
mu = 2000# stredni hodnota,
sigma = 200# smerodatna odchylka
pnorm(2200,mu,sigma)-pnorm(1800,mu,sigma)

pnorm(2000,mu,sigma)**2

data <- data.frame(Grade = c("A+", "A", "A-", "B+", "B", "B-"))

# Definice numerické mapy
grade_mapping <- c("A+" = 4.3, "A" = 4.0, "A-" = 3.7, "B+" = 3.3, "B" = 3.0, "B-" = 2.7)

boxplot(Math$Grade~Math$Gender)
IQR(Math$Grade)
quantile(Math$Grade,0.25)
Math$Gender