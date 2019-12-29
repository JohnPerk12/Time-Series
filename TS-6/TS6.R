#########   Problems 1

da=read.table("d-msft3dx0113.txt",header=T)
head(da)
msft=log(da$msft+1)
plot(msft,type='l')
acf(msft)
Box.test(msft,lag=10,type='Ljung')
t.test(msft)
m1=arima(msft,order=c(0,0,2),include.mean=F)
m1
tsdiag(m1)
Box.test(m1$residuals,lag=10,type='Ljung')
Box.test(m1$residuals^2,lag=10,type='Ljung')
m2=garchFit(~arma(0,2)+garch(1,1),data=msft,trace=F)
summary(m2)
m2=garchFit(~arma(0,1)+garch(1,1),data=msft,trace=F)
summary(m2)
plot(m2)
m3=garchFit(~arma(0,1)+garch(1,1),data=msft,trace=F,cond.dist="std")
summary(m3)
plot(m3)
pm3=predict(m3,4)
pm3

source("Igarch.R")
m4=Igarch(msft)
names(m4)
sigma.t=m4$volatility
resi=msft/sigma.t
acf(resi)
acf(resi^2)
Box.test(resi,lag=10,type='Ljung')
Box.test(resi^2,lag=10,type='Ljung')
length(msft)
v1=(1-0.973)*msft[3269]^2+.973*sigma.t[3269]^2
sqrt(v1)

source("garchM.R")
m5=garchM(msft)

#########   Problem 2

da1=read.table("m-ba3dx6113.txt",header=T)
head(da1)
ba=log(da1$ba+1)
t.test(ba)
Box.test(ba,lag=12,type='Ljung')
at=ba-mean(ba)
Box.test(at^2,lag=12,type='Ljung')
n1=garchFit(~garch(1,1),data=ba,trace=F)
summary(n1)
plot(n1)
n2=garchFit(~garch(1,1),data=ba,trace=F,cond.dist="std")
summary(n2)
plot(n2)
n2=garchFit(~garch(1,1),data=ba,trace=F,cond.dist="sstd")
summary(n2)
plot(n2)
tt=(.888-1)/.06
tt
n3=garchM(ba)

source("Tgarch11.R")
n4=Tgarch11(ba)
