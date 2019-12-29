#########   Problem 1       ##########

sp=log(da1$sprtrn+1)
acf(sp)
t.test(sp)
m1=garchFit(~garch(1,1),data=sp,trace=F)
summary(m1)
plot(m1)
m2=garchFit(~garch(1,1),data=sp,trace=F,cond.dist="std")
summary(m2)
plot(m2)
m3=garchFit(~garch(1,1),data=sp,trace=F,cond.dist="sstd")
summary(m3)
plot(m3)
predict(m3,5)
m4=garchFit(~aparch(1,1), data=sp, delta=2, include.delta=F,trace=F,cond.dist="sstd")
summary(m4)


#########   Problem 2       ##########
#########   FX: JPUS        ##########
#####  with T = 2210.

er <- ts(read.table('d-fxjpus0514.txt', colClasses="numeric", header = FALSE))
x2 <- exp(er)   # to plot antilog exchange rate as already logged

rtn=diff(log(x2))   # can use er
plot(rtn,type='l')
acf(rtn)
t.test(rtn)

Box.test(rtn,lag=10,type='Ljung')

require(fGarch)
m1=garchFit(~garch(1,1),data=rtn,trace=F)
summary(m1)

plot(m1)
m2=garchFit(~garch(1,1),include.mean=F,data=rtn,cond.dist="std",trace=F)
summary(m2)
plot(m2)
source("Igarch.R")
m3=Igarch(rtn)
m4=garchFit(~aparch(1,1),include.mean=F,delta=2,include.delta=F,trace=F,cond.dist="std")
summary(m4)

#### The following is doing the same thing.
m4=garchFit(~garch(1,1),include.mean=F,trace=F,cond.dist="std",leverage=T)
summary(m4)
#### In terms of percentage
y1=rtn*100
m5=Tgarch11(y1,cond.dist="std")

m6=garchFit(~garch(1,1),data=y1,trace=F,cond.dist="std",leverage=T)
summary(m6)
m6=garchFit(~garch(1,1),data=y1,trace=F,cond.dist="std",leverage=T,include.mean=F)
summary(m6)
