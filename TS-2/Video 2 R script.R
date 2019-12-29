library(fBasics)

# Simulate Gaussian white noise process

set.seed(101)
y = rnorm(100,sd=1)
y.bar = mean(y)
g.hat = acf(y,lag.max=10,type="covariance",plot=F)
r.hat = acf(y,lag.max=10,type="correlation",plot=F)
par(mfrow=c(1,2))

series <- as.timeSeries(y)
head(series)
plot(series, type = "l")
acfPlot(series)

# AR(1) stationary process

set.seed(1231)
e = rnorm(100,sd=1)
e.start = rnorm(25,sd=1)
y.ar1 = 1 + arima.sim(model=list(ar=0.75), n=100, innov=e, start.innov=e.start)

series <- as.timeSeries(y.ar1)
head(series)
par(mfrow=c(1,2))
plot(series, type = "l")
acfPlot(series)

# Signal + noise process

set.seed(112)
eps = rnorm(100,sd=1)
eta = rnorm(100,sd=0.5)
z = cumsum(eta)
y = z + eps
dy = diff(y)
par(mfrow=c(1,2))
series <- as.timeSeries(y)
head(y)
plot(y, type = "l")
acfPlot(y)

# Differencing

series <- as.timeSeries(diff(y))
head(series)
plot(series, type = "l")
acfPlot(series)


