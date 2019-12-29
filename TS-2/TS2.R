require (fBasics)
require (fpp)
require(knitr)
require(ggplot2)
require (ggthemes)
require (gridExtra)

### Problem 1 ###

da=read.table("d-nflx3dx0913.txt",header=T)
head(da)
da_stats = basicStats(da)
kable(da_stats[c('Mean', 'Stdev','Skewness', 'Kurtosis', 'Minimum', 'Maximum'), -(1:2)],
      caption='Basic Statistics of the Simple Return Series')

###Log Returns of the Data###
#--1. A

d1b = log(da[,-(1:2)]+1)  # Log Transform, +1 as an offset so that we don't compute log(0)
d1b
d1b_stats = basicStats(d1b)
kable(d1b_stats[c('Mean', 'Stdev', 'Skewness', 'Kurtosis', 'Minimum', 'Maximum'),], 
      caption='Basic Statistics of the Log Transformed Simple Return Series')


###Density Plots and QQ Plots###
#--1. B
d1=density(rtn$nflx)
d2=density(rtn$sprtrn)
par(mfcol=c(1,2))
plot(d1$x,d1$y,xlab='Returns',ylab='Density',main='Netflix',type='l')
plot(d2$x,d2$y,xlab='Returns',ylab='Density',main='SP',type='l')


###Test the Null Hypothesis####
#--1. C
t.test(rtn$nflx)


### Problem 2 ###
d2 = read.table("d-nflx3dx0913.txt", header=T)
head(d2)

##Part A
d2l = log(d2[,-(1:2)] + 1) # Log Transform, +1 as an offset so that we don’t compute log( 
st = skewness(d2l$nflx) / sqrt(6 / length(d2l$nflx)) # compute skewness test 
print(paste("Skewness Statistic: ", st))

p_st = 2 * (1 - pnorm(abs(st))) # computing the p-value 
print(paste("p-value: ", p_st))

##Part B

nflx=rtn$nflx
tm3=skewness(nflx)/sqrt(6/length(nflx))
tm3
tk=kurtosis(nflx)/sqrt(24/length(nflx))
tk

t.test(d2l)


### Problem 3 ###

##Load Data###
data(ukcars)
ukcars_d = window(ukcars, start=1997)
###Plot the Data###
plot(ukcars_d, type="o", xlab = "Years", ylab = "UK Car Production (In Thousands)")

###Decompose in STL###
#--3. A
fit_stl = stl(ukcars_d, s.window = "periodic")
plot(fit_stl)

#Acquire seasonal Factors
seas_adj = seasadj(fit_stl)
seas_factors = fit_stl$time.series[2:11, "seasonal"]

###Forecast the next two years using damped method###
fit_damped_seas_adj = holt(seas_adj, damped = TRUE)
print(fit_damped_seas_adj)
plot(fit_damped_seas_adj, xlab = "Years", ylab = "UK Car Production (In Thousands)")
print(fit_damped_seas_adj$model)
print(accuracy(fit_damped_seas_adj))
resea_fit_damed_seas_adj = fit_damped_seas_adj$mean + seas_factors
plot(ukcars_d, type = "o", xlab = "Years", ylab = "UK Car Production (In Thousands)", xlim = c(1997, 2008))
lines(resea_fit_damed_seas_adj, type = "o", col = "blue")

###Forecast the next two years using Holt###
f_linear = holt(seas_adj)
print(f_linear)
plot(f_linear, xlab = "Years", ylab = "UK Car Production (In Thousands)")
print(f_linear$model)
print(accuracy(fit_linear))
re_linear = f_linear$mean + seas_factors
plot(ukcars_d, type = "o", xlab = "Years", ylab = "UK Car Production (In Thousands)", xlim = c(1997, 2008))
lines(re_linear, type = "o", col = "blue")

####Ets() for a seasonal model###
f_ets = ets(ukcars_d, model = "ZZZ")
print(f_ets)
plot(forecast(f_ets), xlab = "Years", ylab = "UK Car Production (In Thousands)")
print(accuracy(f_ets))

### Compare Models ###
print(paste("Additive-Damped Model RMSE: ", accuracy(fit_damped_seas_adj)[2]))
print(paste("Holt Model RMSE: ", accuracy(f_linear)[2]))
print(paste("ETS Model RMSE: ", accuracy(f_ets)[2]))


### Which Model is more reasonable? ###
par(mfrow = c(3,1))
p_damped = plot(forecast(fit_damped_seas_adj), type = "o", xlab = "Years", ylab = "Production")
p_linear = plot(forecast(f_linear), type = "o", xlab = "Years", ylab = "Production")
p_ets = plot(forecast(f_ets), type = "o", xlab = "Years", ylab = "Production")
