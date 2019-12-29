#TS3 PREDICT 413

require(fBasics)
require(fpp)
require(knitr)
require(ggplot2)
require(ggthemes)
require(gridExtra)

da=read.table("m-umcsent.txt",header=T)
head(da)

csent=da$VALUE

adf.test(csent)

chnge = diff(csent)
tsdisplay(chnge, main = "U of M Consumer Sentiment, first order difference")

adf.test(chnge, k=5)

t.test(chnge)

Box.test(chnge, lag = 12, type = "Ljung")

ar(chnge, method = "mle")

#Part 3.c
model1 = arima(chnge, order= c(5,0,0)) 
print(model1)

#Part 3.c
model1=ar(chnge,method="mle")
model1
model1$order
acf(chnge)

#Part 3.d
poly1 = c(1, -model1$coef) # characteristic equation
roots1 = polyroot(poly1) # find the zeros of the complex polynomial print(roots1)
print(Mod(roots1))
print(2 * pi / acos(1.073056 / 1.493939))

#Part 3.e
model1_pred = predict(model1, 4)
print(model1_pred$pred)
print(model1_pred$se)
model1_pred_lcl = model1_pred$pred - 1.96 * model1_pred$se 
model1_pred_ucl = model1_pred$pred + 1.96 * model1_pred$se 
print(model1_pred_lcl)
print(model1_pred_ucl)

#Part 4.a
model1.se = sqrt(diag(vcov(model1))) 
model1.tratio = abs(model1$coef/model1.se) 
print(model1.tratio)
mask = c(0, NA, NA, 0, NA)

#Part 4.b
model2 = arima(chnge, order=c(5,0,0), include.mean = FALSE, transform.pars = FALSE, fixed = mask) 
print(model1)
print(model2)

tsdiag(model2, gof.lag = 24)
Box.test(model1$residuals, lag = 24, type = "Ljung")

#Part4.c
accuracy(model1)
accuracy(model2)
Box.test(model1$residuals, lag = 5, type = "Ljung")
Box.test(model2$residuals, lag = 3, type = "Ljung")
model1$aic
model2$aic

#Part 4.d
poly2 = c(1, -model2$coef) # characteristic equation
roots2 = polyroot(poly2) # find the zeros of the complex polynomial 
print(roots2)
print(Mod(roots2))
print(2 * pi / acos(1.157751 / 1.594751))

#---Start Backtest---#

"backtest" <- function(m1,rt,orig,h,xre=NULL,fixed=NULL,inc.mean=TRUE){
  # m1: is a time-series model object
  # orig: is the starting forecast origin
  # rt: the time series
  # xre: the independent variables
  # h: forecast horizon
  # fixed: parameter constraint
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

backtest(model2,chnge,380,1,inc.mean=F)
backtest(model1,chnge,380,1,inc.mean=F)








#---OLD CODE---#

csent=da$VALUE
tdx=da[,1]+da[,2]/12
plot(tdx,csent,xlab='Year',ylab='Sentiment',type='l',main="UM Consumer Sentiment")
acf(csent)
pacf(csent)
pacf(diff(csent))  ### select lag = 5.
adf.test(csent,k=5)
adf.test(csent,k=5,type="ct")
adf.test(csent,k=5,type="nc")
chg=diff(csent)
t.test(chnge)
Box.test(chnge,lag=12,type='Ljung')

m1=ar(chnge,method="mle")
m1$order
acf(chnge)
m2=arima(chnge,order=c(5,0,0),include.mean=F)
m2

tsdiag(m2) ### Model checking

p1=c(1,-m2$coef)
r1=polyroot(p1)
r1
m2p=predict(m2,4)   ### prediction 1 to 4-step ahead
names(m2p)
m2p$pred
m2p$se
lcl=m2p$pred-1.96*m2p$se
lcl
ucl=m2p$pred+1.96*m2p$se
ucl
c1=c(0,NA,NA,0,NA)
m3=arima(chg,order=c(5,0,0),include.mean=F,fixed=c1)
m3

tsdiag(m3,gof=24)  ### Model checking

p2=c(1,-m3$coef)
s2=polyroot(p2)
s2

#---Start Backtest---#

"backtest" <- function(m1,rt,orig,h,xre=NULL,fixed=NULL,inc.mean=TRUE){
  # m1: is a time-series model object
  # orig: is the starting forecast origin
  # rt: the time series
  # xre: the independent variables
  # h: forecast horizon
  # fixed: parameter constraint
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


backtest(model2,chg,380,1,inc.mean=F)

