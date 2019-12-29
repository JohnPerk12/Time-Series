library(tseries)
library(fBasics)
library(forecast)
library(lmtest)
library(fGarch) 
install.package("fGarch")

#########   Assignment 6    ###########

#########   Problems 1    ###########
#Read the data
da=read.table("d-msft3dx0113.txt",header=T)
head(da)

#EDA 1.1
basicStats(da$msft)
hist(da$msft)
qqnormPlot(da$msft)
tsdisplay(da$msft)


#1.1 and 1.2

msft=log(da$msft+1)
plot(msft,type='l')
tsdisplay(msft)
Box.test(msft,lag=10,type='Ljung')
t.test(msft)
m1=arima(msft,order=c(0,0,2),include.mean=F)
m1
tsdiag(m1)
Box.test(m1$residuals,lag=10,type='Ljung')
Box.test(m1$residuals^2,lag=10,type='Ljung')

#1.3
m2=garchFit(~arma(0,2)+garch(1,1),data=msft,trace=F)
summary(m2)
plot(m2)


#1.4
m2=garchFit(~arma(0,1)+garch(1,1),data=msft,trace=F)
summary(m2)
plot(m2)

m3=garchFit(~arma(0,1)+garch(1,1),data=msft,trace=F,cond.dist="std")
summary(m3)
plot(m3)
pm3=predict(m3,4)
pm3

#1.6
source("Igarch.R")
m4=Igarch(msft)
names(m4)

#1.7
sigma.t=m4$volatility
resi=msft/sigma.t
acf(resi)
acf(resi^2)
Box.test(resi,lag=10,type='Ljung')
Box.test(resi^2,lag=10,type='Ljung')
length(msft)

v1=(1-0.973)*msft[3269]^2+.973*sigma.t[3269]^2
sqrt(v1)

fin = predict(m4$volatility,4)
plot(fin)

source("garchM.R")
m5=garchM(msft)
predict(m5,4)
m5p


#########   Problem 2    ###########

da1=read.table("m-ba3dx6113.txt",header=T)
head(da1)
#Perform EDA
basicStats(da1$ba)
hist(da1$ba)
qqnormPlot(da1$ba)
tsdisplay(da1$ba)

ba=log(da1$ba+1)
t.test(ba)

Box.test(ba,lag=12,type='Ljung')
at=ba-mean(ba)
Box.test(at^2,lag=12,type='Ljung')

#Part 2.2
n1=garchFit(~garch(1,1),data=ba,trace=F)
summary(n1)
plot(n1)

#Part 2.3
n2=garchFit(~garch(1,1),data=ba,trace=F,cond.dist="std")
summary(n2)
plot(n2)
n2=garchFit(~garch(1,1),data=ba,trace=F,cond.dist="sstd")
summary(n2)
plot(n2)
forecast(n2,4)

tt=(.888-1)/.06
tt

#Part 2.4
n3=garchM(ba)

#Part 2.5
source("Tgarch11.R")
n4=Tgarch11(ba)


