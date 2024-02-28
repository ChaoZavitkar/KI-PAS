#############################
### Priklady na pravdepodobnost

### Diskretni rozdeleni
## Binomicke rozdeleni: n - pocet pokusu, p - pst uspechu
#   pocet uspechu v n-pokusech
# pbinom(k,n,p) - distribucni funkce
# dbinom(k,n,p) - pravdepodobnostni funcke
# stredni hodnota n*p
# rozptyl n*p*(1-p)

#Příklad 7
#a) ohlásí dva bankrot (x <= 1)
1-pbinom(1,100,0.035)
#b)
#P(x=3) + P(x=4) + P(x=5)
sum(dbinom(3:5, 100, 0.035))
#P(x=5) - (P<=2)
pbinom(5, 100,0.035)-pbinom(2,100,0.035)

#Příklad 8
#a)
dbinom(2,20,0.02)
#b)
dbinom(0,20,0.02)
#c)
pbinom(2,20,0.02)

#Příklad 9
pbinom(5,500,0.01)

#Příklad 10
#p=0,25 n=100 P(x>=35) = 1-P(x<34)
1-pbinom(34,100,1/4)

## Hypergeometricke rozdeleni: w - pocet bilych kouli v osudi, b - pocet cernych kouli v osudi
#   n - pocet kouli tazenych z osudi
#   pocet bilych kouli mezi n tazenymi
# phyper(k,w,b,n) - distribucni funkce
# dhyper(k,w,b,n) - pravdepodobnostni funcke
# stredni hodnota n*w/(w+b)
# rozptyl (n*w/(w+b))*(1-w/(w+b))*((w+b-n)/(w+b-1))

#Příklad 5 (počet spolehlivých s úvěrem, počet spolehlivých, počet nespolehlivých, kolika dává banka úvěr)
dhyper(15,35,15,20)

## Geometricke rozdeleni: p - pravdepodobnost uspechu
# cekani na prvni uspech, do prikazu se zadava pocet neuspechu pred prvnim uspechem
# pgeom(k,p) - distribucni funkce
# dgeom(k,p) - pravdepodobnostni funcke
# stredni hodnota (1-p)/p
# rozptyl (1-p)/p^2

#Příklad 15
#p=1-0.5076
#první budou 3 ženy, pak muž P(x=3)
dgeom(3, 1-0.5076)
#1-P(x<=9)
1-pgeom(9,0.4882)
#Binomické
pbinom(0,10,0.4882)
#kolik pokusů
(1-0.4882)/0.4882


## Poissonovo rozdeleni: lambda - stredni hodnota
# pocet udalosti
# ppois(k,lambda) - distribucni funkce
# dpois(k,lambda) - pravdepodobnostni funcke
# stredni hodnota lambda
# rozptyl lambda

#Příklad 6
#(přijde právě 5, průměr zákazníků za den)
dpois(5, 5)
#(přijde více jak 4, průměr zákazníků za den)
1-ppois(4, 5)

#Příklad 14
#a) 30/60*4=2 P(>=1) => 1-P(x<0)
1-ppois(0,2)
#b) 30/60*5=2.5
ppois(0,2.5)
#c) 30/60*10
dpois(5,5)
#d) 30/60*20
#kvantilová fce (inverzní od distibuční)
qpois(0.99,10)
plot(0:30,dpois(0:30,10),type = "h")


## Negativni binomicke rozdeleni: n - pocet uspechu, p - pst uspechu
# pnbinom(k,n,p) - distribucni funkce
# dnbinom(k,n,p) - pravdepodobnostni funcke

### Spojita rozdeleni
## Normalni rozdeleni: mu - stredni hodnota, sigma - smerodatna odchylka
# pnorm(x,mu,sigma) - distribucni funkce
# qnorm(x,mu,sigma) - kvantilová funkce

# hustota - vyska dospelych muzu
curve(dnorm(x,180,7),from=150,to=210, main="Hustota N(180, 49)",col="red",ylab="Hustota")
# distribucni funkce - vyska dospelych muzu
curve(pnorm(x,180,7),from=150,to=210, main="Distribucni funkce N(180, 49)",col="purple",ylab="Hustota")

#Příklad S1 Spojitá
curve(dnorm(x,115,sqrt(256)),from=70,to=160, main="Hustota N(115, 256)",col="red",ylab="Hustota")
curve(pnorm(x,115,16),from=70,to=160, main="Distribucni funkce N(115, 256)",col="purple",ylab="Hustota")
#a)
pnorm(120, 115, 16)
#b)
1-pnorm(105,115,16)
#c
pnorm(130,115,16)-pnorm(100,115,16)

#Příklad S5
#a)
1-pnorm(0.5,2,3)
#b)
pnorm(-1.2,2,3)
#c)
pnorm(1,2,3)-pnorm(-0.5,2,3)

#Příklad S3
qnorm(1/4,2,sqrt(5)) #spodní kvanil
qnorm(2/4,2,sqrt(5)) #půl kvantil
qnorm(3/4,2,sqrt(5)) #horní kvantil
qnorm(3/4,2,sqrt(5))-qnorm(1/4,2,sqrt(5)) #mezikvantilové rozpětí

#Příklad S4
qlnorm(1/2,6,sqrt(6))

curve(dlnorm(x,6,sqrt(2)),from=0.01,to=5000, main="Hustota LN(6, 2)",col="red",ylab="Hustota")
qlnorm(95/100,6,sqrt(2))

curve(plnorm(x,6,sqrt(2)),from=0.01,to=5000, main="Distribuční funkce LN(6, 2)",col="red",ylab="Hustota")

#Příklad S6
1-pnorm(18,23,sqrt(25))
pnorm(22,23,sqrt(25))-pnorm(8,23,sqrt(25))
qnorm(0.95,23,sqrt(25))
qnorm(0.25,23,sqrt(25))
c(qnorm(0.025,23,sqrt(25)), qnorm(0.975,23,sqrt(25)))

#Příklad S7
# 1.645=(x-20)/sqrt(25)
x = 1.645*sqrt(25)+20

## Lognormalni rozdeleni: mu , sigma (velicina ln(X) ~ N(mu, sigma^2))
# plnorm(x,mu,sigma) - distribucni funkce
# stredni hodnota exp(mu+sigma^2/2)
# rozptyl (exp(sigma^2)+2)*exp(2*mu+sigma^2)

## Exponencialni rozdeleni: int - intenzita
# pexp(x,int) - distribucni funkce
# qexp(x, int) - kvantilova funkce
# distribucni funkce P(X <= t) = 1 - exp(-int*t)
# stredni hodnota 1/int
# rozptyl 1/int^2

#Příklad S11 - distribuční fce
# 0,1 = 1-exp(-lamba*1000)
lamba = log(0.9)/1000*(-1)
# lambda = intenzita
1/lamba
1-pexp(365 ,lamba)
pexp(700, lamba)-pexp(200, lamba)
curve(dexp(x,lamba),from=0.01,to=40000, main="Hustota EXP(0.000105)",col="red",ylab="Hustota")

#Příklad S13
lamba=1/2000
#P(X >= 3000) = 1 - pexp()
1-pexp(3000, lamba)
#P(x <= 2000) = pexp()
pexp(2000, lamba)

qexp(0.05, lamba)

#################################
### Centralni limitni veta
## Rozdeleni souctu nezavislych, stejne rozdelenych nahodnych velicina
#  konverguje k normalnimu pro pocet techto velicin rostouci nade vsechny meze.

library(TeachingDemos)
clt.examp(1)
clt.examp(2)
clt.examp(5)
clt.examp(10)
clt.examp(50)