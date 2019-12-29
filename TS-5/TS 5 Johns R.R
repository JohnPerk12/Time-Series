require(fBasics)
require(fpp)
require(knitr)
require(ggplot2)
require(ggfortify)
require(ggthemes)
require(gridExtra)

d2 = read.table("m-PastorStambaugh.txt", header=T) 
head(d2)

t2 = ts(d2$PS_LEVEL, start = c(1962, 8), frequency = 12)
autoplot(t2, main = "Monthly Market Liquidity of Professors, Pastors and Stambaugh", ylab = "Month Market Liquidity", xlab = "Years")

t2_stl = stl(t2, s.window="periodic")
autoplot(t2_stl, main = "STL Decomposition of Monthly Market Liquidity of Professors, Pastors, and Stambaugh")

t2_acf = acf(t2, plot = FALSE)
autoplot(t2_acf, main = "ACF of Monthly Market Liquidity of Professors, Pastors, and Stambaugh")

m2 = arima(t2, order = c(5,0,0))
print(m2)

m2oi = which.min(m2$residuals)
d2$outlier = 0
d2$outlier[m2oi] = 1
m3 = arima(d2$PS_LEVEL, order = c(5,0,0), xreg = d2$outlier)

tsdiag(m3)

m3.se = sqrt(diag(vcov(m3))) 
m3.tratio = abs(m3$coef/m3.se) 
print(m3.tratio)

m4_mask = c(NA, NA, NA, 0, NA, NA, NA)

m4 = arima(d2$PS_LEVEL, order = c(5,0,0), xreg = d2$outlier, fixed = m4_mask)

print(m4)

tsdiag(m4)

#--------------------Part 2 Microsoft---------------------#

d3 = read.table("q-earn-msft.txt", header=T) 
head(d3)

t3 = ts(d3$value, start = c(1986, 2), frequency = 4)
autoplot(t3, main = "Quarterly earnings per share of Microsoft", ylab = "Quarterly Earnings per share", xlab = "Years")

t3_stl = stl(t3, s.window="periodic")
autoplot(t3_stl, main = "STL Decomposition of Quarterly Earnings per share of Microsoft")

t3_acf = acf(t3, plot = FALSE)
autoplot(t3_acf, main = "ACF of Quarterly Earnings per share of Microsoft")

t4 = ts(log(d3$value), start = c(1986, 2), frequency = 4)
autoplot(t4, main = "Log Quarterly earnings per share of Microsoft", ylab = "Quarterly Earnings per share", xlab = "Years")

t4_acf = acf(t4, plot = FALSE)
autoplot(t4_acf, main = "ACF of Log Quarterly Earnings per share of Microsoft")
m5 = arima(t4, order = c(0,1,1), seasonal = list(order=c(0,1,1), period=4))
print(m5)

m6 = arima(t4, order = c(0,1,1), seasonal = list(order=c(0,0,1), period=4))
print(m6)

source('backtest.R')
backtest(m5, t4, 81, h=1, inc.mean=F)
backtest(m6, t4, 81, h=1, inc.mean=F)


#------------------------Part 3-------------------------------#

d4 = read.table("m-FamaBlissdbndyields.txt", header=T)
head(d4)

t5 = ts(d4$yield1, start = c(1961), frequency = 12)
t6 = ts(d4$yield3, start = c(1961), frequency = 12)
autoplot(t5, main = "Fama-Bliss Bond Yeilds, 1 Year Maturity", ylab = "Bond Yields", xlab = "Years")
autoplot(t6, main = "Fama-Bliss Bond Yeilds, 3 Year Maturity", ylab = "Bond Yields", xlab = "Years")

m7 = lm(t6~t5) 
summary(m7)

d1t = diff(t5) 
d3t = diff(t6)
m8 = lm(d3t~1 + d1t) 
summary(m8)

m8_acf = acf(m8$residuals, plot = FALSE)
autoplot(m8_acf, main = "ACF of Model 8 Residuals")
m8_pacf = pacf(m8$residuals, plot = FALSE)
autoplot(m8_pacf, main = "PACF of Model 8 Residuals")

m9 = arima(t6, order=c(6,0,0), xreg = t5)
print(m9)

tsdiag(m9, gof.lag = 24)

mask2 = c(NA,0,NA,NA,0,NA,NA,NA)
m10 = arima(t6, order = c(6,0,0), xreg = t5, fixed = mask2)
print(m10)


p1 = c(1, -m10$coef[1:6])
s1 = polyroot(p1)
print(s1)


Mod(s1)

1 / Mod(s1)


























