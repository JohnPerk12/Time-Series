#########   Assignment 8    ##########

#########   Problem 1       ##########

require(MTS)
data=read.table("q-fdebt.txt",header=T)

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
dat2=data.frame(qgdp$uk,qgdp$ca,qgdp$us)
colnames(dat2)=c("uk", "ca", "us")
logdat2=log(dat2)
datgrowth=apply(logdat2,2,diff)
growth=100*datgrowth

m1=VAR(growth,p=4)
m2=refVAR(m1, thres=1.96)
MTSdiag(m2)
