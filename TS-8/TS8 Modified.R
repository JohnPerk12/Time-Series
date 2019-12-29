#########   Assignment 8    ##########

#########   Problem 1       ##########
library(tseries)
library(fBasics)
library(forecast)
library(lmtest)
library(fGarch) 
require(MTS)
data=read.table("q-fdebt.txt",header=T)
data
#----EDA for hbfin---#
basicStats(data$hbfin)
hist(data$hbfin)
qqnormPlot(data$hbfin)
tsdisplay(data$hbfin)

#----EDA for hbfrbn---#
basicStats(data$hbfrbn)
hist(data$hbfrbn)
qqnormPlot(data$hbfrbn)
tsdisplay(data$hbfrbn)

# Now take appropriate transformation and difference.
# Define this new data as z.
logdata = log(data)
z=apply(logdata,2,diff)
z=data.frame(z[,3],z[,4])
colnames(z)=c("hbfin", "hbfrbn")
plot(z$hbfin, type="l")
plot(z$hbfrbn, type="l")

source("ccm.R")
ccm(z,5)

source("mq.R")
mq(z,10)


#########   Problem 2       ##########

data("mts-examples",package="MTS")
head(qgdp)
#-----EDA UK-----#
basicStats(qgdp$uk)
hist(qgdp$uk)
qqnormPlot(qgdp$uk)
tsdisplay(qgdp$uk)

#-----EDA CA-----#
basicStats(qgdp$ca)
hist(qgdp$ca)
qqnormPlot(qgdp$ca)
tsdisplay(qgdp$ca)

#-----EDA US-----#
basicStats(qgdp$us)
hist(qgdp$us)
qqnormPlot(qgdp$us)
tsdisplay(qgdp$us)

dat2=data.frame(qgdp$uk,qgdp$ca,qgdp$us)
colnames(dat2)=c("uk", "ca", "us")
logdat2=log(dat2)
datgrowth=apply(logdat2,2,diff)

source("ccm.R")
ccm(datgrowth,5)

growth=100*datgrowth

m1=VAR(growth,p=4)

m2=refVAR(m1, thres=1.96)
MTSdiag(m2)









