download.file('http://stat-athens.aueb.gr/~jbn/papers/files/14/14_bivpois_RDATA.zip', 
              f <- tempfile())
unzip(f, exdir=tempdir())
load(file.path(tempdir(), '.RData'))

load('bivpois.RData')


## Simulate VAR(2)-data
library(dse)
library(vars)
## Setting the lag-polynomial A(L) 
Apoly   <- array(c(1.0, -0.5, 0.3, 0,
                   0.2, 0.1, 0, -0.2,
                   0.7, 1, 0.5, -0.3) ,
                 c(3, 3, 3))

## Setting Covariance to identity-matrix
B <- diag(3)
B

## Setting constant term to 5 and 10
TRD <- c(1, 10)

## Generating the VAR(2) model 
var3  <- ARMA(A = Apoly, B = B, TREND = TRD)

## Simulating 500 observations
varsim <- simulate(var3, sampleT = 500, noise = list(w = matrix(rnorm(1500),nrow = 500, ncol = 3)), rng = list(seed = c(123456))) 

## Obtaining the generated series
varsim <- matrix(rnorm(1500),500,3)  


vardat <- matrix(varsim, nrow = 500, ncol = 3)



varsim <- matrix(rnorm(1500),500,3)  
colnames(varsim) <- c("y1", "y2", "y3")

## Plotting the series
plot.ts(varsim, main = "", xlab = "")
## Determining an appropriate lag-order
infocrit <- VARselect(varsim, lag.max = 3, type = "const")
## Estimating the model
varsimest <- VAR(varsim, p = 2, type = "const", season = NULL, exogen = NULL)
## Alternatively, selection according to AIC
varsimest <- VAR(varsim, type = "const",lag.max = 3, ic = "SC")
## Checking the roots
roots <- roots(varsimest)

causality(varsimest, cause = "y1")$Granger
causality(varsimest, cause = "y2")$Granger
causality(varsimest, cause = "y3")$Granger

## Causality tests
## Granger and instantaneous causality


var.causal <- causality(varsimest, cause = "y2")
causality(var.causal, cause = "y3")$Granger


