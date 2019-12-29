install.packages("PerformanceAnalytics")
#### load libraries ####
library(PerformanceAnalytics)
library(quantmod)
library(tseries)
library(fBasics)
library(forecast)
library(lmtest)
library(fGarch) 
require(MTS)

#### import data ####
# download data
symbol.vec = c("XOM", "CVX")
getSymbols(symbol.vec, from ="2013-01-01", to = "2016-12-31")
colnames(XOM)
colnames(CVX)

head(XOM, n=10)
head(CVX, n=10)

xy <- merge(XOM, CVX, by.x = 1, by.y = 1, all.x = TRUE, all.y = TRUE)
head(xy, n=10)

df <- xy[, c(6,12)]
head(df, n=10)

#-------------------------------------------------------------------------------------------#

#-------Part 1.1 EDA-------#
basicStats(df$XOM.Adjusted)
hist(df$XOM.Adjusted, main="Exxon Mobile Daily Adjusted Closing Price")
qqnormPlot(df$XOM.Adjusted)
tsdisplay(df$XOM.Adjusted, main="Exxon Mobile Daily Adjusted Closing Price")
d1=density(df$XOM.Adjusted)
plot(d1$x,d1$y,xlab='returns',ylab='density',main= "Exxon Mobil Density Plot",type='l')
t.test(df$XOM.Adjusted)

basicStats(df$CVX.Adjusted)
hist(df$CVX.Adjusted, main="Chevron Daily Adjusted Closing Price")
qqnormPlot(df$CVX.Adjusted)
tsdisplay(df$CVX.Adjusted, main="Chevron Daily Adjusted Closing Price")
d2=density(df$CVX.Adjusted)
plot(d2$x,d2$y,xlab='returns',ylab='density',main= "Chevron Density Plot",type='l')
t.test(df$CVX.Adjusted)

xom = df$XOM.Adjusted

# Need the hypothesis test and the 95% confidence interval for the daily log returns of each stock
#Log returns of XOM
df$lxom = log(df$XOM.Adjusted)
tsdisplay(df$lxom, main="Log Transformed data for Exxon Mobile")
hist(df$lxom, main="Log Transformed data for Exxon Mobile")
t.test(df$lxom)

diff.lxom = diff(lxom)
diff.lcvx = diff(lcvx)

df$diff.lxom = diff(lxom)
tsdisplay(df$diff.lxom, main="Log Transformed and First Differenced Data for Exxon Mobile")
hist(df$diff.lxom, main="Log Transformed and First Differenced Data for Exxon Mobile")
t.test(df$diff.lxom)

diff.lxom = diff(lxom)

#Log Returns of CVX and also calculate first difference
df$lcvx = log(df$CVX.Adjusted)
tsdisplay(df$lcvx, main="Log Transformed data for Chevron")
hist(df$lcvx, main="Log Transformed data for Chevron")
head(df$lcvx, n=5)
t.test(df$lcvx)

df$diff.lcvx = diff(df$lcvx)
tsdisplay(df$diff.lcvx, main="Log Transformed and First Differenced Data for Chevron")
hist(df$diff.lcvx, main="Log Transformed and First Differenced Data for Chevron")


Box.test(df$diff.lxom,lag=4,type='Ljung')
atlxom = df$diff.lxom-mean(df$diff.lxom)
Box.test(atlxom^2,lag=4,type='Ljung')

Box.test(df$diff.lcvx,lag=4,type='Ljung')
atlcvx = df$diff.lcvx-mean(df$diff.lcvx)
Box.test(atlcvx^2,lag=4,type='Ljung')

# Will also need to identify trends, seasonality, and embedded business cycles. 

#-------------------------------------------------------------------------------------------#

#-------Part 1.2 Box-Jenkins------#
mar1=arima(df$diff.lxom,order=c(4,1,1),include.mean=F)
mar1
tsdiag(mar1,gof=10)

mar2=arima(df$diff.lcvx,order=c(4,1,1),include.mean=F)
mar2
tsdiag(mar2,gof=10)

accuracy(mar1)
accuracy(mar2)

#-------------------------------------------------------------------------------------------#

#-------Part 1.3 Forecasts------#
plot(forecast(mar1),main="Exxon Mobile First Month Forecast")
forecast(mar1,31)
mar1_pred = predict(mar1, 1)
print(mar1_pred)
print(mar1_pred$se)

mar1_lxom = mar1_pred$pred - 1.96 * mar1_pred$se 
mar1_lxom2 = mar1_pred$pred + 1.96 * mar1_pred$se 
print(mar1_lxom)
print(mar1_lxom2)


plot(forecast(mar2),main="Chevron First Month Forecast")
forecast(mar2,31)
mar2_pred = predict(mar2, 1)
print(mar2_pred$pred)
print(mar2_pred$se)
mar2_lcvx = mar2_pred$pred - 1.96 * mar2_pred$se 
mar2_lcvx2 = mar2_pred$pred + 1.96 * mar2_pred$se 

print(mar2_lcvx)
print(mar2_lcvx2)

par(mfrow = c(2,1))
Exxon = plot(forecast(mar1_pred), type = "o", xlab = "Years", ylab = "Price")
Chevron = plot(forecast(mar2_pred), type = "o", xlab = "Years", ylab = "Price")


#-------------------------------------------------------------------------------------------#

#-------Part 1.4 ARMA-GARCH------#

#Gaussian ARMA GARCH for XOM
mlxom=log(df$XOM.Adjustable+1)

n1=garchFit(~arma(0,1)+garch(1,1),data=df$lxom,trace=F)
summary(n1)
plot(n1)

#Gaussian ARMA GARCH for CVX
n2 = garchFit(~arma(0,1)+garch(1,1),data=df$lcvx,trace=F)
summary(n2)
plot(n2)


#ARMA GARCH with Student-t innovation series----LXOM
m3 = garchFit(~arma(0,1)+garch(1,1),data=df$lxom,trace=F,cond.dist="std")
summary(m3)
plot(m3)

#ARMA GARCH with Student-t innovation series----Lcvx
m4 = garchFit(~arma(0,1)+garch(1,1),data=df$lcvx,trace=F,cond.dist="std")
summary(m4)
plot(m4)


#1 step ahead mean and volatility forecast for LXOM
pm3 = predict(n1,1)
pm3
t.test(pm3)

#1 step ahead mean and volatility forecast for Lcvx
pm4 = predict(n2,1)
pm4

t.test(pm4)

#-------------------------------------------------------------------------------------------#

#-------Part 1.5 Cross-Correlation--------#
require(fBasics)
library(PerformanceAnalytics)
library(rugarch)
library(car)
library(rmgarch)
library(curl)
library(devtools)

# compute rolling correlations
#
# chart.RollingCorrelation(lcvx, lxom, width=30)

# create combined data series
lxom.lcvx = merge(diff.lxom,diff.lcvx)


cor.fun = function(x){
  cor(x)[1,2]
}

cov.fun = function(x){
  cov(x)[1,2]
}

roll.cov = rollapply(as.zoo(lxom.lcvx), FUN=cov.fun, width=30,
                     by.column=FALSE, align="right")
roll.cor = rollapply(as.zoo(lxom.lcvx), FUN=cor.fun, width=30,
                     by.column=FALSE, align="right")
par(mfrow=c(2,1))
plot(roll.cov, main="30-day rolling covariances",
     ylab="covariance", lwd=2, col="blue")
grid()
abline(h=cov(lxom.lcvx)[1,2], lwd=2, col="red")
plot(roll.cor, main="30-day rolling correlations",
     ylab="correlation", lwd=2, col="blue")
grid()
abline(h=cor(lxom.lcvx)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))


#-------------------------------------------------------------------------------------------#

#-------Part 1.6 Normal-DCC(1,1) Model--------#

# Normal-DCC(1,1) model  for problem.
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,1), 
                                                model = "sGARCH"), 
                          distribution.model = "norm")

# dcc specification - GARCH(1,1) for conditional correlations
dcc.garch11.spec = dccspec(uspec = multispec( replicate(2, garch11.spec) ), 
                           dccOrder = c(1,1), 
                           distribution = "mvnorm")
dcc.garch11.spec

dcc.fit <- dccfit(dcc.garch11.spec, data = lxom.lcvx)
dcc.fit

class(dcc.fit)
slotNames(dcc.fit)
names(dcc.fit@mfit)
names(dcc.fit@model)


# covariance and correlation series
cov.fit <- rcov(dcc.fit)[1,2,]
ts.plot(cov.fit)

cor.fit <- rcor(dcc.fit)[1,2,]
ts.plot(cor.fit)

# Plot the in-sample conditional covariances and correlations
dcc.fcst = dccforecast(dcc.fit, n.ahead=31)
dcc.fcst
class(dcc.fcst)
slotNames(dcc.fcst)
class(dcc.fcst@mforecast)
names(dcc.fcst@mforecast)


# many method functions - see help on DCCforecast class
# rshape, rskew, fitted, sigma, plot, rcor, rcov, show

# show forecasts
cv <- rcov(dcc.fcst)[[1]][1,2,]
plot.ts(c(cov.fit,cv))

cr <- rcor(dcc.fcst)[[1]][1,2,]
ts.plot(c(cor.fit,cr))














