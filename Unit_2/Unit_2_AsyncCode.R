#  MSDS 6373- Time Series Analysis - Unit 2 Async Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 05/14/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 1)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

# Section / Video 2.3.3 Frequency Domain

# Time series with B: 2pi(0.025)  Period: 1/0.025 Frequency: 0.025
t= seq(1,100,length=100)
y1 = sin(2*pi*0.025*t)
plot(t,y1,type='l')

# Time series with B: 2pi(0.1)  Period: 1/0.100 Frequency: 0.100
y2 = sin(2*pi*0.1*t+1)
plot(t,y2,type='l')

# Time series with B: 2pi(0.15)  Period: 1/0.150 Frequency: 0.150
y3 = sin(2*pi*0.15*t+2.5)
plot(t,y3,type='l')

# Time Series with multiple periods/frequencies
ysum=y1+y2+y3
plot(t,ysum,type='l')

# Section / Video 2.3.5 Concept Check - Frequency Domain

t= seq(1,100,length=100)

# Function a
y5.a = sin(2*pi*0.08*t+5)
plot(t,y5.a,type='l')

# Function b
y5.b = 3*t+10
plot(t,y5.b,type='l')

# Function c
y5.c = sin(2*pi*0.08*t+5)+0.3*t+10
plot(t,y5.c,type='l')


#Fourier Expansion for f(x) = x
t= seq(-3,3,length=7)
m=1
index = 0
for (x in t){
  index = index + 1
  sum=0
  for (n in m) {
    sum=sum+((-1^n)/n)*sin(n*x)
  }
y[index] = 2*sum
}

plot(t,y,type='l')

#--------------------- Section 2.6.4 Spectral Density / Frequency Domain
library(tswge)

# Dataset built into tswge (Pennsylvania temperature dataset)
data(patemp) 
plotts.wge(patemp) #plot of timeseries

parzen.wge(patemp) #give you spectral Density

# Natural log of monthly international airline passengers (in 1000s) from Jan'49-Dec'60
data("airlog")
plotts.wge(airlog)

parzen.wge(airlog)

# Bat echolocation signal
data(bat)
plotts.wge(bat)

parzen.wge(bat)

# Classic Sunspot Data: 1749-1924
data("sunspot.classic")
plotts.wge(sunspot.classic)

parzen.wge(sunspot.classic)

#--- Combined graphics function ----
# Displays dataset, acf, spectral density
plotts.sample.wge(patemp)

plotts.sample.wge(airlog)

plotts.sample.wge(bat)

plotts.sample.wge(sunspot.classic)

#--- You can play with the defaults in parzen function ---
plotts.wge(ysum) # we know there are 3 distinct frequencies
parzen.wge(ysum) # Default parameters didn't pick up the distinct frequencies
parzen.wge(ysum, trunc = 70) # now does a better job of identifying these peaks


#-- Inclass Assignment 
data(llynx) # The log (base 10) of the annual number of lynx trapped in the Mackenzie River
            # district of the North-West Canada

plotts.wge(llynx)
parzen.wge(llynx)
plotts.sample.wge(llynx)

