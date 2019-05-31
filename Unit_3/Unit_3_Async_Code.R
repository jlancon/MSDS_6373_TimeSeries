#  MSDS 6373- Time Series Analysis - Unit 3 Async Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 05/21/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 2 & 3)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

# Section / Video 3.3.1 Linear Filter / Slides 11
#Low Pass Filter (moving average filter)
data("fig1.21a")
plotts.wge(fig1.21a)

ma=filter(fig1.21a,rep(1,5))/5

#?filter()

#High Pass Filter (difference filter)
dif=diff(fig1.21a,lag=1)
plot(dif,type='l',ylim=c(-10, 10))
#?diff()

# Section / Video 3.3.1 Filter / Slides 13
# Examples
Realization = gen.sigplusnoise.wge(200,coef = c(5,3),freq = c(.1,.45),vara = 10,sn=1)

#Create a Low Pass Filter (moving average filter) 5pt
ma=filter(Realization,rep(1,5))/5
plot(ma, type='l', ylim=c(-15,15))
parzen.wge(ma[!is.na(ma)]) #removes na values

#Create a High Pass Filter (Difference Filter) lag=1
dif=diff(Realization,lag=1)
plot(dif,type='l',ylim=c(-15, 15))
parzen.wge(dif[!is.na(dif)])

# Concept Check 3.3.2 Question
# Generate the realization given by the following
Realization = gen.sigplusnoise.wge(200,coef = c(5,0),freq = c(.1,0), vara = 10, sn = 1)
ma = filter(Realization,rep(1,5))/5
plot(ma, type='l', ylim=c(-15,15))
# Is this a low-pass or high-pass filter? low-pass
# Is the code a moving-average smoother or an example of a differencing filter? Moving-average

# Section / Video 3.5.1 Filter / Slides 37
# AR(1) with phi1=0.95  (Positive phi1)
slide37_a = gen.arma.wge(100, phi=0.95, plot = TRUE,sn=2)
acf(slide37_a)
plotts.sample.wge(slide37_a)
dev.off()
# AR(1) with phi=0.70
slide37_b = gen.arma.wge(100, phi=0.70, plot = TRUE,sn=2)
acf(slide37_b)
plotts.sample.wge(slide37_b)
dev.off()

# Section / Video 3.5.2 Filter / Slides 44
# AR(1) with phi1=-0.95  (Negative phi1)
slide44_a = gen.arma.wge(100, phi=-0.95, plot = TRUE,sn=2)
acf(slide44_a)
plotts.sample.wge(slide44_a)
dev.off()
# AR(1) with phi= -0.70
slide44_b = gen.arma.wge(100, phi=-0.70, plot = TRUE,sn=2)
acf(slide44_b)
plotts.sample.wge(slide44_b)
dev.off()


# Section / Video 3.5.5 AR(1) Models / Slides 47,48,49

# for AR(1) models, phi is a constant and theta retains its default values

?gen.arma.wge()
#gen.arma.wge(n, phi=0, theta=0, vara = 1, plot = TRUE,sn=0)

gen.arma.wge(250) #creating a unique white noise realization every time it is run
gen.arma.wge(250, sn=1) # creates a repeatable white noise realization (sn = seed value)

# sn=0 (default) generates a new (randomly obtained) realization each time.
# Setting sn > 0 allows you to generate the same realization each time you
# apply the command with the same sn


# Creating realization that is not white noise, has autocorrelation
# AR(1)
gen.arma.wge(n=100,phi=0.95) # Each time run is different:  with a phi = 0.95
                             # data seems to be wandering significantly
gen.arma.wge(n=100,phi=0.70) # Realizations are still wandering but not as much
                             # as with a higher phi

gen.arma.wge(n=100,phi=-0.95) # Realization with negative phi
                              # data is oscillating back and forth

gen.arma.wge(n=100,phi=0.95, sn=5) # Settng seed for repeatability
gen.arma.wge(n=100,phi=-0.95, sn=5)


# Plot the true theoretical ACF and spectral density.
plotts.true.wge(phi= 0.95)

plotts.true.wge(phi= -0.95)

# plot realization of length 100, true autocorrelation
# and spectral density for an AR(1)

plotts.true.wge(phi=0.95)

# Generate and plot AR(1) realization and place it in
# vector x
x = gen.arma.wge(n=200,phi=0.6)
# plot realization in x along with sample autocorrelations,
# periodogram, and Parzen window-based spectral estimator
plotts.sample.wge(x)


#####------Concept Check 3.5.6 -----#
x = gen.arma.wge(n=100,phi = -0.5,sn=7)
plotts.sample.wge(x)

#####------Concept Check 3.5.7 -----#
plotts.true.wge(n=100,phi=-0.5)

###  Async Video 3.5.8 - phi greater than 1 ###
gen.arma.wge(n=100,phi=c(.9999))#nearly nonstationary
gen.arma.wge(n=100,phi=c(1)) #error message
gen.arima.wge(n=100,d=1) #ARIMA case (similar to .9999)
gen.arma.wge(n=50,phi=c(1.1)) #error message

# Non stationary models with phi_1 = 1.1
# have to use loops, tswge will not work

n=50
x=rep(0,50)
a=rnorm(n)
x[1:50]=0
for(k in 2:n) {
  x[k]=1.1*x[k-1]+a[k]
}
plotts.wge(x)