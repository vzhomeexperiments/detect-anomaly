#Imagine that you have a time series with some periodic component which you want to eliminate. 
#The periodic component is clearly different from zero, you want to retain only zero mean random noise and
#some events which can be e.g. malfunction of the machine
#The idea is to use the fact that the normal periodic component has lower frequency (longer period) than
#the malfunction event (which lasts maybe some minutes or seconds)

rm(list=ls())

library(tidyverse)
library('lubridate')
library(waveslim)
#library(plyr)
library(Rwave)

#This example takes fictitious web traffic (clicks or sessions) in 5 seconds intervals
#First step: generate random sample of click times (normally distributed)

clicks <- function(N, st="2017/01/01", et="2017/01/31") {
  st <- as.POSIXct(as.Date(st))
  et <- as.POSIXct(as.Date(et))
  dt <- as.numeric(difftime(et,st,unit="sec"))
  ev <- sort(runif(N, 0, dt))
  rt <- st + ev
}

set.seed(42)
clicktime <- clicks(1500000)

#Click time - build 5 second blocks and summarize
fivesec <- ceiling((hour(clicktime)*60*60 + minute(clicktime)*60 + second(clicktime))/5)
Traffic <- data.frame(clicktime, fivesec)
Traffic <- group_by(Traffic, fivesec)
Result <- summarise(Traffic, count = n()) #Summarize clicks in 5 second intervals
Result$count <- Result$count/10+30
#plot(Result$count)


#Add a periodic, lower frequency component + slight positive trend
b <- 0.0003 #Frequency (should be low)
a <- 50 #Amplitude
set.seed(1)
y1 <- a*sin(b*Result$fivesec*pi)+Result$fivesec*0.002
#plot(y1)
Result$count <- Result$count + y1
#plot(Result$count)

#Add three traffic peaks (quadratic) with approx. 3 minute duration in the middle and at the end. Should have length of 36 5-sec intervals

#length(Result$fivesec[ceiling(nrow(Result)/2):ceiling(nrow(Result)/1.992)])
#plot((-(Result$fivesec[ceiling(nrow(Result)/2):ceiling(nrow(Result)/1.992)]-mean(Result$fivesec[ceiling(nrow(Result)/2):ceiling(nrow(Result)/1.992)]))^2+abs(min(-(Result$fivesec[ceiling(nrow(Result)/2):ceiling(nrow(Result)/1.992)]-mean(Result$fivesec[ceiling(nrow(Result)/2):ceiling(nrow(Result)/1.992)]))^2)))/4,ylab="")

Result$count[ceiling(nrow(Result)/2):ceiling(nrow(Result)/1.992)] <- Result$count[ceiling(nrow(Result)/2):ceiling(nrow(Result)/1.992)] + (-(Result$fivesec[ceiling(nrow(Result)/2):ceiling(nrow(Result)/1.992)]-mean(Result$fivesec[ceiling(nrow(Result)/2):ceiling(nrow(Result)/1.992)]))^2+abs(min(-(Result$fivesec[ceiling(nrow(Result)/2):ceiling(nrow(Result)/1.992)]-mean(Result$fivesec[ceiling(nrow(Result)/2):ceiling(nrow(Result)/1.992)]))^2)))/4
Result$count[ceiling(nrow(Result)/3):ceiling(nrow(Result)/2.992)] <- Result$count[ceiling(nrow(Result)/3):ceiling(nrow(Result)/2.992)] + -(Result$fivesec[ceiling(nrow(Result)/3):ceiling(nrow(Result)/2.992)]-mean(Result$fivesec[ceiling(nrow(Result)/3):ceiling(nrow(Result)/2.992)]))^2+abs(min(-(Result$fivesec[ceiling(nrow(Result)/3):ceiling(nrow(Result)/2.992)]-mean(Result$fivesec[ceiling(nrow(Result)/3):ceiling(nrow(Result)/2.992)]))^2))
Result$count[(nrow(Result)-36):nrow(Result)] <- Result$count[(nrow(Result)-36):nrow(Result)] + (-(Result$fivesec[(nrow(Result)-36):nrow(Result)]-mean(Result$fivesec[(nrow(Result)-36):nrow(Result)]))^2+abs(min(-(Result$fivesec[(nrow(Result)-36):nrow(Result)]-mean(Result$fivesec[(nrow(Result)-36):nrow(Result)]))^2)))/3
#plot(x)
#plot(Result$count)

attach(Result)

#ONLY FOR ANALYSIS PURPOSES: Now show the spectrogram - see that a lot of variance in the middle frequency area (2 min peaks) is located where we placed it
out.wave <- morlet(y1 = Result$count, x1 = Result$fivesec, p2 = 13, dj = 0.1, siglvl = 0.99)
wavelet.plot(out.wave, useRaster=NA, add.coi=TRUE)

#Now decompose the series into parts
n5secs <- length(Result$count)
nPwrs <- 10 #You can play with the number of frequency intervals used
#nPwrs <- trunc(log(n5secs)/log(2)) - 1 #Or you can use an automatic period selection
dat.mra <- mra(ceiling(Result$count),wf="la8",J=nPwrs,method="modwt",boundary="periodic") #Or boundary "reflection" if it works better
#Available wavelet filters: "la8", "haar", "d4", and "mb4

#Plot decomposition of the original time series - single graphs are parts in the resp. frequency intervals
secsLabels <- 2^(1:nPwrs)
par(mar=c(3,2,2,2),mgp=c(1.25,0.25,0),tcl=0.5,xaxs="i",yaxs="i")
plot(Result$fivesec,rep(1,n5secs),type="n", axes=FALSE, ylab="",xlab="",ylim=c(-3,70),xlim=c(0,20000))

title(main="Multiresolution decomposition of traffic",line=0.75)
axis(side=1)
mtext("5 Seconds",side=1,line = 1.25)

Offset <- 0

for(i in (nPwrs+1):1){
  x <- scale(dat.mra[[i]]) + Offset
  lines(Result$fivesec,x)
  abline(h=Offset,lty="dashed")
  mtext(names(dat.mra)[[i]],side=2,at=Offset,line=0)
  mtext(secsLabels[i],side=4,at=Offset,line=0)
  Offset <- Offset+5
}

dat.mra1 <- as.data.frame(dat.mra)

#Show single components
plot(eval(parse(text = paste0("dat.mra$S",nPwrs)))) #Periodic component - very good filtering result
plot(rowSums(dat.mra1[,3:nPwrs])) #Peaks + some noise
#You can see that the algorithm is not able to decompose the series 100% correct but for our purposes it is enough
plot(dat.mra$D2+dat.mra$D1) #White noise

#Now you can take the peaks + all noise part and test if it is significantly different from zero
test1 <- rowSums(dat.mra1[(n5secs-11000):(n5secs-10964),1:nPwrs]) #Control group
test2 <- rowSums(dat.mra1[(n5secs-36):n5secs,1:nPwrs]) #Experimental group at the end of the series

t.test(test1,alternative = "two.sided",conf.level = 0.99) #Not significant at 99% level
t.test(test2,alternative = "two.sided",conf.level = 0.99) #clearly significant at 99% level
#You see that our fictitious peaks could be detected