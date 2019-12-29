#R code Assignment 1


require(fBasics)
require(quantmod)   
require(fpp)        
require(knitr)      
require(ggplot2)   
require(ggthemes)   
require(gridExtra)

### Part 1 ###

da <- read.table("m-ge3dx8113.txt",header=T)
head(da)


###PART A----- Compute the Sample Mean etc.###
da_stats = basicStats(da)
kable(da_stats[c('Mean', 'Stdev','Skewness', 'Kurtosis', 'Minimum', 'Maximum'), -(1:2)],
      caption='Basic Statistics of the Simple Return Series')


###PART B--- Transform log returns###
d1b = log(da[,-(1:2)]+1)  # Log Transform, +1 as an offset so that we don't compute log(0)
d1b_stats = basicStats(d1b)
kable(d1b_stats[c('Mean', 'Stdev', 'Skewness', 'Kurtosis', 'Minimum', 'Maximum'),], 
      caption='Basic Statistics of the Log Transformed Simple Return Series')

###PART C--- Test the null hypothesis###
t.test(d1b$ge)


###PART D--- Obtain the empirical density Plots###
pge = ggplot(d1b, aes(ge)) +
  stat_density(alpha = 0.4) +
  labs(x="Returns", y="Density") +
  ggtitle("GE") + theme_fivethirtyeight()

psprtrn = ggplot(d1b, aes(sprtrn))+
  stat_density(alpha = 0.4) +
  labs(x="Returns", y="Density")+
  ggtitle("S&P") + theme_fivethirtyeight()

grid.arrange(pge, psprtrn, ncol=2)

####PART 2####

##---PART A##
d2 = read.table("m-ge3dx8113.txt", header=T)
head(d2)

d2l = log(d2[,-(1:2)]+1)  # Log Transform, +1 as an offset so that we don't compute log(0)
t.test(d2l$ge)

##---PART B##
st = skewness(d2l$ge) / sqrt(6 / length(d2l$ge))  # compute skewness test
paste(2*(1-pnorm(abs(st))))  # computing the p-value

##---PART C##
kt = kurtosis(d2l$ge) / sqrt(24 / length(d2l$ge))  # compute kurtosis test
paste(2*(1-pnorm(abs(kt))))


#### PART 3####
#Part A- Time Plot
ts.visitors = visitors  # comes from fpp package
df.visitors = as.data.frame(visitors)
plot(ts.visitors)

#PART B/C
aust = window(visitors)
fit_multi = hw(aust, seasonal="multiplicative")
print(fit_multi)
plot(fit_multi)

#PART D
fit_multi_damped = hw(aust, seasonal="multiplicative", damped=TRUE)
plot(forecast(fit_multi_damped))

fit_multi_exp = hw(aust, seasonal="multiplicative", exponential=TRUE)
plot(forecast(fit_multi_exp))

fit_multi_exp_damped= hw(aust, seasonal="multiplicative", exponential=TRUE, damped=TRUE)
plot(forecast(fit_multi_exp_damped))

##Part E###
accuracy(fit_multi)
accuracy(fit_multi_damped)
accuracy(fit_multi_exp)
accuracy(fit_multi_exp_damped)

###------PART 4------###
#Part A - Holt-Winters
fit_multi = hw(aust, multiplicative=TRUE)
plot(fit_multi)
hist(residuals(fit_multi), nclass=20)
plot(residuals(fit_multi))
accuracy(fit_multi)

#Part B - ETS
fit_mam = ets(visitors, model="ZZZ")
plot(forecast(fit_mam))
hist(residuals(fit_mam), nclass=20)
plot(residuals(fit_mam))
accuracy(fit_mam)

#Part C - ETS Applied to Box Cox
fit_ana_box = ets(visitors, additive.only = TRUE, lambda = TRUE)
plot(forecast(fit_ana_box))
hist(residuals(fit_ana_box), nclass=20)
plot(residuals(fit_ana_box))
accuracy(fit_ana_box)

#Part D - Seasonal Naive Applied to Box Cox
fit_naive = snaive(visitors, lambda = TRUE)
plot(forecast(fit_naive))
hist(residuals(fit_naive), nclass=20)
plot(residuals(fit_naive))
accuracy(fit_naive)

#Part E - STL Decomposition Applied to Box Cox Followed by ETS Applied to the seaonal Data
fit_stld = stlf(visitors, method = "ets", lambda = TRUE)
plot(forecast(fit_stld))
hist(residuals(fit_stld), nclass=20)
plot(residuals(fit_stld))
accuracy(fit_stld)

#Part F - Comparison of all of them
accuracy(fit_multi)
accuracy(fit_mam)
accuracy(fit_ana_box)
accuracy(fit_naive)
accuracy(fit_stld)


















#----------------------------OLD-----------------------------#
basicStats(exp(da$ge)-1)
basicStats(exp(da$vwretd)-1)
basicStats(exp(da$ewretd)-1)
basicStats(exp(da$sprtrn)-1)

basicStats(da$ge)
basicStats(da$vwretd)
basicStats(da$ewretd)
basicStats(da$sprtrn)
t.test(da$ge)
d1=density(da$ge)
d2=density(da$sprtrn)
par(mfcol=c(1,2))
plot(d1$x,d1$y,xlab='returns',ylab='density',main=‘GE’,type='l')
plot(d2$x,d2$y,xlab='returns',ylab='density',main='SP',type='l')

### Part 2 ###

ge=da$ge
t.test(ge)
tm3=skewness(ge)/sqrt(6/length(ge))
tm3
kurtosis(ge)
tk=kurtosis(ge)/sqrt(24/length(ge))
tk

### Parts 3 and 4  ###

# See Chapter 7 (Exponential Smoothing), Hyndeman text
# https://www.otexts.org/fpp/

suppressMessages(require(fpp))
plot(visitors)
plot  

aust <- window(visitors)
fit_multi <- hw(aust,seasonal="multiplicative")
print(fit_multi)

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

fit_multi <- hw(aust,seasonal="multiplicative")
plot(fit_multi)
plot
hist(residuals(fit_multi),nclass=20)
plot
plot(residuals(fit_multi))
plot
accuracy(fit_multi)

fit_mam <- ets(visitors, model="ZZZ")
plot(forecast(fit_mam))
hist(residuals(fit_mam),nclass=20)
plot(residuals(fit_mam))
accuracy(fit_mam)

fit_ana_box <- ets(visitors,additive.only=TRUE,lambda=TRUE)
plot(forecast(fit_ana_box))
hist(residuals(fit_ana_box),nclass=20)
plot(residuals(fit_ana_box))
accuracy(fit_ana_box)

fit_naive <- snaive(visitors,lambda=TRUE)
plot(forecast(fit_naive))
hist(residuals(fit_naive),nclass=20)
plot(residuals(fit_naive))
accuracy(fit_naive)

fit_stld <- stlf(visitors,method="ets",lambda=TRUE) plot(forecast(fit_stld))
hist(residuals(fit_stld),nclass=20)
plot(residuals(fit_stld))
accuracy(fit_stld)

################################################
#####     Parking lot
################################################\

### Part 1 ###

da=read.table("d-nflx3dx0913.txt",header=T)
head(da)
basicStats(da$nflx)
basicStats(da$vwretd)
basicStats(da$ewretd)
basicStats(da$sprtrn)
rtn=log(da[,3:6]+1)   ### Compute log returns
basicStats(rtn$nflx)
basicStats(rtn$vwretd)
basicStats(rtn$ewretd)
basicStats(rtn$sprtrn)
t.test(rtn$nflx)
d1=density(rtn$nflx)
d2=density(rtn$sprtrn)
par(mfcol=c(1,2))
plot(d1$x,d1$y,xlab='returns',ylab='density',main='Netflix',type='l')
plot(d2$x,d2$y,xlab='returns',ylab='density',main='SP',type='l')
