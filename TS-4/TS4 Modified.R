require(fBasics)
require(fpp)
require(knitr)
require(ggplot2)
require(ggthemes)
require(gridExtra)

#Part 1.A
d1 = read.table("m-umcsent.txt", header=T)
head(d1)

consent = d1$VALUE
tdx = d1[,1] + d1[,2] / 12  # assemble the time axis
tdx
plot(tdx, consent, xlab = "Year", ylab = "Sentiment", type = "l", main = "U of M Consumer Sentiment")

a <- consent
a.ts <- ts(a, frequency = 12)
consent_stl = stl(a.ts, s.window="periodic")
plot(consent_stl)

#Part 1.B
adf.test(consent)
tsdisplay(consent, main = "U of M Consumer Sentiment")

#Part 1.C
change = diff(consent)
tsdisplay(change, main = "U of M Consumer Sentiment, first order difference")
ar(change, method = "mle")
adf.test(change, k = 5)


#### Duration ####
#### Problem 2  ####

da=read.table("m-unempmean.txt",header=T)
head(da)

#Part 2.A
tdx=da[,1]+da[,2]/12
dur=da$Value

plot(tdx,dur,type='l',xlab='Year',ylab='Dur',main='Mean duration of unemp')
pacf(dur)
pacf(diff(dur))

m1=ar(diff(dur),method="mle")
m1$order

ddur=diff(dur)
t.test(ddur)

#Part 2.B
m2=arima(ddur,order=c(12,0,0),include.mean=F)
m2

tsdiag(m2,gof=24)

#Part 2.D
m4=arima(ddur,order=c(2,0,1),seasonal=list(order=c(1,0,1),period=12),include.mean=F)
m4
tsdiag(m4,gof=24)
dim(da)

accuracy(m2)
accuracy(m4)

#Part 2.F
#-------------------------------------------------------------------------------#
"backtest" <- function(m1,rt,orig,h,xre=NULL,fixed=NULL,inc.mean=TRUE){
  # m1: is a time-series model object
  # orig: is the starting forecast origin
  # rt: the time series
  # xre: the independent variables
  # h: forecast horizon
  # fixed: parameter constriant
  # inc.mean: flag for constant term of the model.
  #
  regor=c(m1$arma[1],m1$arma[6],m1$arma[2])
  seaor=list(order=c(m1$arma[3],m1$arma[7],m1$arma[4]),period=m1$arma[5])
  T=length(rt)
  if(orig > T)orig=T
  if(h < 1) h=1
  rmse=rep(0,h)
  mabso=rep(0,h)
  nori=T-orig
  err=matrix(0,nori,h)
  jlast=T-1
  for (n in orig:jlast){
    jcnt=n-orig+1
    x=rt[1:n]
    if (is.null(xre))
      pretor=NULL else pretor=xre[1:n]
    mm=arima(x,order=regor,seasonal=seaor,xreg=pretor,fixed=fixed,include.mean=inc.mean)
    if (is.null(xre))
      nx=NULL else nx=xre[(n+1):(n+h)]
    fore=predict(mm,h,newxreg=nx)
    kk=min(T,(n+h))
    # nof is the effective number of forecats at the forecast origin n.
    nof=kk-n
    pred=fore$pred[1:nof]
    obsd=rt[(n+1):kk]
    err[jcnt,1:nof]=obsd-pred
  }
  #
  for (i in 1:h){
    iend=nori-i+1
    tmp=err[1:iend,i]
    mabso[i]=sum(abs(tmp))/iend
    rmse[i]=sqrt(sum(tmp^2)/iend)
  }
  print("RMSE of out-of-sample forecasts")
  print(rmse)
  print("Mean absolute error of out-of-sample forecasts")
  print(mabso)
  backtest <- list(origin=orig,error=err,rmse=rmse,mabso=mabso)
}
#----------------------------------------------------------------------#

backtest(m2,ddur,750,1,inc.mean=F)
backtest(m4,ddur,750,1,inc.mean=F)



#### Oil Prices ####
#### Problem 3  ####

#Part 3.A
da=read.table("w-coilwtico.txt",header=T)
head(da)
dim(da)

tdx=c(1:1474)/52+1986
tdx

coil=da$Value
plot(tdx,coil,xlab='Year',ylab='Price',type='l',main="Weekly Crude Oil Prices")
plot(diff(coil),type='l')
plot(diff(log(coil)),type='l')
rtn=diff(log(coil))

t.test(rtn)

Box.test(rtn,lag=10,type='Ljung')

#Part 3.B
acf(rtn)
m1=ar(rtn,method="mle")
m1$order
m1=arima(rtn,order=c(10,0,0))
m1

c1=c(NA,NA,NA,NA,0,0,NA,NA,0,0,NA)
m2=arima(rtn,order=c(10,0,0),include.mean=F,fixed=c1)
m2
tsdiag(m2,gof=20)

t.test(m2)

Box.test(m2,lag=11,type='Ljung')

#Part 3.C
m3=arima(rtn,order=c(3,0,2),include.mean=F)
m3
tsdiag(m3,gof=20)

accuracy(m2)
accuracy(m3)

par(mfrow = c(2,1))
p_damped = plot(forecast(m2), type = "o", xlab = "Years", ylab = "Price")
p_linear = plot(forecast(m3), type = "o", xlab = "Years", ylab = "Price")

