require(fBasics)

#------------------------------------------------------------------
### Part 1 ###
da <- read.table("m-ge3dx8113.txt",header=T)
head(da)
# a) basic stats of raw data
basicStats(da$ge)
basicStats(da$vwretd)
basicStats(da$ewretd)
basicStats(da$sprtrn)
# b) Log returns of the raw data
basicStats(exp(da$ge)-1)
basicStats(exp(da$vwretd)-1)
basicStats(exp(da$ewretd)-1)
basicStats(exp(da$sprtrn)-1)
# c) Test the Null Hypothesis
t.test(da$ge)
# e) obtain emperical density plot
d1=density(da$ge)
d2=density(da$sprtrn)
par(mfcol=c(1,2))
plot(d1$x,d1$y,xlab='returns',ylab='density',main= "GE",type='l')
plot(d2$x,d2$y,xlab='returns', ylab='density', main='SP', type='l')

#------------------------------------------------------------------
### Part 2 ###
ge=da$ge
lr <- (exp(da$ge)-1)
lr
t.test(lr)
skewness(ge)
tm3=skewness(ge)/sqrt(6/length(ge))
tm3

kurtosis(ge)
tk=kurtosis(ge)/sqrt(24/length(ge))
tk

#------------------------------------------------------------------
### Part 3 ###
require(forecast)
suppressMessages(require(fpp))

# a) Make a plot of the data
plot(visitors)
plot  

# b) forecast the next two years using Holt-Winters' multiplicative method
aust <- window(visitors)
fit_multi <- hw(aust,seasonal="multiplicative")
print(fit_multi)
plot(fit_multi)
plot

# d) compare with exponential or damped and compare
fit_multi_damped <- hw(aust,seasonal="multiplicative",damped=TRUE)
plot(forecast(fit_multi_damped))
plot

fit_multi_exp <- hw(aust,seasonal="multiplicative",exponential=TRUE)
plot(forecast(fit_multi_exp))
plot

fit_multi_exp_damped <- hw(aust,seasonal="multiplicative",
                           exponential=TRUE,damped=TRUE)
plot(forecast(fit_multi_exp_damped))
plot

accuracy(fit_multi)
accuracy(fit_multi_damped)
accuracy(fit_multi_exp_damped)

#------------------------------------------------------------------
### Part 4 ###

# a)
fit_multi <- hw(aust,seasonal="multiplicative")
plot(fit_multi)
plot
hist(residuals(fit_multi),nclass=20)
plot
plot(residuals(fit_multi))
plot
accuracy(fit_multi)

# b)
fit_mam <- ets(visitors, model="ZZZ")
plot(forecast(fit_mam))
hist(residuals(fit_mam),nclass=20)
plot(residuals(fit_mam))
accuracy(fit_mam)

# c)
fit_ana_box <- ets(visitors,additive.only=TRUE,lambda=TRUE)
plot(forecast(fit_ana_box))
hist(residuals(fit_ana_box),nclass=20)
plot(residuals(fit_ana_box))
accuracy(fit_ana_box)

# d)
fit_naive <- snaive(visitors,lambda=TRUE)
plot(forecast(fit_naive))
hist(residuals(fit_naive),nclass=20)
plot(residuals(fit_naive))
accuracy(fit_naive)

# e)
fit_stld <- stlf(visitors,method="ets",lambda=TRUE) 
plot(forecast(fit_stld))

hist(residuals(fit_stld),nclass=20)
plot(residuals(fit_stld))
accuracy(fit_stld)



