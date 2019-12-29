#### Consumer ####
#### Problem 1 ####

# p 94, green Tsay


#### Duration ####
#### Problem 2  ####


da=read.table("m-unempmean.txt",header=T)
head(da)
tdx=da[,1]+da[,2]/12
dur=da$Value
plot(tdx,dur,type='l',xlab='year',ylab='dur',main='Mean duration of unemp')
pacf(dur)
pacf(diff(dur))
m1=ar(diff(dur),method="mle")
m1$order
require(fUnitRoots)

help(adfTest)  ### Unit-root test 
adfTest(dur,lags=12,type="c")

ddur=diff(dur)
t.test(ddur)

m2=arima(ddur,order=c(12,0,0),include.mean=F)
m2

tsdiag(m2,gof=24)

m4=arima(ddur,order=c(2,0,1),seasonal=list(order=c(1,0,1),period=12),include.mean=F)
m4

tsdiag(m4,gof=24)

dim(da)

backtest(m2,ddur,750,1,inc.mean=F)
backtest(m4,ddur,750,1,inc.mean=F)



#### Oil Prices ####
#### Problem 3  ####

 
da=read.table("w-coilwtico.txt",header=T)
head(da)
dim(da)
tdx=c(1:1474)/52+1986
coil=da$Value
plot(tdx,coil,xlab='yeay',ylab='price',type='l',main="Weekly Crude Oil Prices")
plot(diff(coil),type='l')
plot(diff(log(coil)),type='l')
rtn=diff(log(coil))

t.test(rtn)

Box.test(rtn,lag=10,type='Ljung')

acf(rtn)
m1=ar(rtn,method="mle")
m1$order
m1=arima(rtn,order=c(11,0,0))
m1

c1=c(NA,NA,NA,NA,0,0,NA,NA,0,0,NA)
m2=arima(rtn,order=c(11,0,0),include.mean=F,fixed=c1)
m2

tsdiag(m2,gof=20)
m3=arima(rtn,order=c(3,0,2),include.mean=F)
m3

tsdiag(m3,gof=20)


