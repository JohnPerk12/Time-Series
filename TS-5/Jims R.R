library(tseries)
library(fBasics)
library(forecast)
library(lmtest)
da=read.table("m-PastorStambaugh.txt",header=T)
head(da)
plot(da[,2],type='l')
plot(da[,3],type='l')
xt1<-da[,2]
hist(xt1)
basicStats(xt1)
qqnormPlot(xt1)
shapiro.test(xt1)
t.test(xt1)
acf(xt1)
acf(da[,3])
pacf(xt1)
tsdisplay(xt1)
adf.test(xt1,k=6,alternative = "stationary")
# Do we need  differening ?.
ndiffs(xt1)
#No

# DO we need seasonal Difference
nsdiffs(xt1)
#no

m2=arima(xt1,order=c(5,0,0))
m2
coeftest(m2)
my3_1=arima(xt1,order = c(1,0,1))
my3_1
coeftest(my3_1)
# lets see if the auto arima can beat my best model
my4_1 <- auto.arima(xt1)
my4_1
coeftest(my4_1)
# Rats - auto arima(Arima(1,0,2)) beat me in comparsion with AIC 1657.54 compared to mine 1672.78
# check out the diagnostics
tsdiag(m2, gof=24)
tsdiag(my3_1,gof=24)
#my model diagnostics was terrible
tsdiag(my4_1,gof = 24)
# Lets plot out residuals in a larger veiw
plot(m2$residuals)
plot(my4_1$residuals)
# There is a big residal just greater than 300
hist(m2$residuals,breaks = "FD", xlab = "Residuals", main = "Histogram of Residuals")

which.min(m2$residuals)
dim(da)
i303=rep(0,605)
i303[303]=1

m3=arima(da[,2],order=c(5,0,0),xreg=i303)
m3
tsdiag(m3)
coeftest(m3)
c1=c(NA,0,0,NA,0,0,0)
m3=arima(da[,2],order=c(5,0,0),xreg=i303,fixed=c1)
m3
tsdiag(m3)
# by cleaning up this outlier the AIC was much smaller
# in the forecast package there is tsclean function
xtqclean <- tsclean(xt1)
my4clean_1 <- arima(xtqclean,order = c(1,0,2))
my4clean_1
tsdiag(my4clean_1)
plot(my4clean_1$residuals)
#lets see if we get a differnt auto.arima with a cleaned dataset
auto.arima(xtqclean)
# yes we do with a poorer AIC ??????
ndiffs(xtqclean)
# Okay let's clean up this one outlier and let auto.arima get another shot
my_4_cleanone_1=arima(xt1,order=c(1,0,2),xreg=i303)
my_4_cleanone_1
tsdiag(my_4_cleanone_1)
coeftest(my_4_cleanone_1)
c2=c(NA,NA,NA,NA,NA)
my_5_1=arima(xt1,order=c(1,0,2),xreg=i303,fixed=c2)
my_5_1
tsdiag(my_5_1)
# the auto.arima has the best model modifing the one outlier




######   Problem 2     ##########

da=read.table("q-earn-msft.txt",header=T)
msft=da[,3]
xt=log(msft)
plot(xt)
basicStats(xt)
qqnormPlot(xt)
hist(xt,prob =T, ylim= c(0,0.5),col= "red")
lines(density(xt),lwd=2)
t.test(xt)
shapiro.test(xt)

tsdisplay(xt)

#Part B
m1=ar(xt,method="mle")
xt$order
names(m1)
# match the order to the k
adf.test(xt,k=4,alternative = "stationary")### select lag = 4.
# so the Augmented Dickey_Fuller Test reveals a p value 0.639 and we can not reject the null
# so we accept the NULL that there is a unit-root presence
# if you change the lag to 1 and 4 you can also reject the null and determine their is a unit root
# so let's do another unit-root test the KPSS test
# For this test the null is there is no unit-root which is opposite the Dickey FUller test
kpss.test(xt)
# do we need a second difference
adf.test(diff(xt),k=4, alternative = "stationary")
kpss.test(diff(xt))
kpss.test(diff(diff(xt)))
# by the kpss test we need 2 differencing
# There appears to be seasonal conponent.
acf(xt)
acf(diff(xt))
tsdisplay(diff(xt))
acf(diff(diff(xt),4))


my1_2 <- arima(xt,order = c(0,1,1),seasonal = list(order=c(0,1,1),period=4))
my1_2
summary(my1_2)
coeftest(my1_2, level =0.90)
tsdiag(my1_2,gof=24)
# lets give auto.arima to have a change
my2_2 <- auto.arima(xt)
my2_2
coeftest(my2_2)
tsdiag(my2_2)

#####Part C#####
# Suggested model
m4=arima(xt,order=c(0,1,1),seasonal=list(order=c(0,0,1),period=4))
m4
tsdiag(m4,gof=20)

m5=arima(xt,order=c(0,1,1),seasonal=list(order=c(0,0,1),period=4))
m5
tsdiag(m5,gof=20)

#####Part D######

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
backtest(my1_2,xt,81,1)
backtest(my2_2,xt,81,1)
backtest(m5,xt,81,1)

forecast(m5, t=81)

# My first model was the best!!!!!!!!!!! with out of sample test


######   Problem 3    ##########

da=read.table("m-FamaBlissdbndyields.txt",header=T)
dim(da)
head(da)
y1t=da[,2]; y3t=da[,3]
#1-year Bond EDA
basicStats(da$yield1)
qqnormPlot(da$yield1)
shapiro.test(da$yield1)
t.test(da$yield1)
hist(da$yield1)
#Three yield Bond EDA
basicStats(da$yield3)
shapiro.test(da$yield3)
t.test(da$yield3)
hist(da$yield3)
qqnormPlot(da$yield3)

#Part B
m1_3=lm(y3t~y1t)
coef(m1_3)
plot(y1t,y3t,col="blue",abline(a=coef(m1_3,b=0)))
summary(m1_3)
AIC(m1_3)
acf(m1_3$residuals)
pacf(m1_3$residuals)
plot(y3t,residuals(m1_3),xlab = "yield3")


#Part C
d1t=diff(y1t); d3t=diff(y3t)
m2_3=lm(d3t~-1+d1t)
summary(m2_3)
AIC(m2_3)
# The difference model has a much better AIC but the target is less
acf(m2_3$residuals)
pacf(m2_3$residuals)
plot(d3t,residuals(m2_3),xlab = "Diff yield3")



m3_3=ar(m2_3$residuals,method="mle")
m3_3$order
m4_3=arima(d3t,order=c(5,0,0),xreg=d1t,include.mean=F)
m4_3
tsdiag(m4_3,gof=24)  ## saved into a file
coeftest(m4_3)
c1=c(NA,NA,0,NA,NA,NA)
m5=arima(d3t,order=c(5,0,0),xreg=d1t,include.mean=F,fixed=c1)
m5
tsdiag(m5,gof=24)  ### very close to that of the above AR(5) model.


#Lets have auto.armima to d3t and then we will add d1t
my2_3 <- auto.arima(y3t)
my2_3
# I will now add the yeild one year to this
my3_3 <- arima(y3t,order=c(1,1,2), xreg = y1t, include.mean = F)
my3_3
# I will now take away the not signficant coeffecients
coeftest(my3_3)
c2_3 <-c(0,0,NA,NA)
my4_3 <- arima(y3t,order=c(1,1,2), xreg = y1t, include.mean = F,fixed = c2_3)
my4_3
tsdiag(my4_3)

#This has a better AIC that the given model




######   Problem 4    ##########

m6=arima(y3t,order=c(6,0,0),xreg=y1t)
m6
tsdiag(m6,gof=24)
coeftest(m6)
c2=c(NA,0,NA,NA,0,NA,NA,NA)
m7=arima(y3t,order=c(6,0,0),xreg=y1t,fixed=c2)
m7
tsdiag(m7,gof=24)
p1=c(1,-m7$coef[1:6])
p1

#Part C
s1=polyroot(p1)
s1
Mod(s1)
1/Mod(s1)

p=c(0.2567, 0.1570, 0.0821, -0.3357, 1)
r=polyroot(p)
r

coefs <-c(1/2,-5,8,-4,1)
roots <-polyroot(coefs)
roots


#################
### Parking lot
#################

######   Problem 1      ##########

#da=read.table("m-FamaFrench.txt",header=T)
#fix(da)
#head(da)
#acf(hml)
#m1=arima(hml,order=c(0,0,1))
#m1
#tsdiag(m1) 
#pm1=predict(m1,2)
#pm1
#lcl=pm1$pred-1.96*pm1$se
#ucl=pm1$pred+1.96*pm1$se
#lcl
#ucl

