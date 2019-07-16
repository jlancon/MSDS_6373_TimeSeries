#  MSDS 6373- Time Series Analysis - Section 1
#  Unit 3 :  Homework
#  Problem:  2.1, 2.3, 3.4 from textbook

#  Team Member:  Jeffery Lancon
#  
#  Date: 05/14/2019
#
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 1)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))


##### Problem 2.1
#Generate a realization of length n=100 from the signal-plus-noise
#model in problem 1.6.
# a) Apply a third-order low-pass Butterworth filter to the original
#    realization with cutoff of 0.2
# b) Apply a third-order high pass Butterworth filter to the original
#    realization with cutoff point of 0.2
# c) Apply a third-order low-pass Butterworth filter with cutoff point
#    of 0.2 to the high-pass filter to realization in (b)
#
# For the original data and the three filtered realizations obtained
# in (a-c), plot the following:
#    i) The realization
#   ii) The sample autocorrelation
#  iii) The Parzen window-based spectral density estimate using the 
#       default truncation point
#
#Discuss the cyclic behavior in the original data along with that for
# (a-c) and discuss the effect of the filters applied.

# Realization from 1.6
# X_t = 3cos(2*pi*(0.05)t + 0) + 1.5cos(2*pi*(0.35)t + 2) + a_t
x = gen.sigplusnoise.wge(n=100,coef=c(3,1.5),freq=c(0.05,0.35),psi=c(0,2))
plotts.sample.wge(x)


# a) Apply a third-order low-pass Butterworth filter to the original
#    realization with cutoff of 0.2

#butterworth.wge(x, order, type, cutoff,plot=TRUE)
x_low = butterworth.wge(x,order=3,type='low',cutoff = 0.2)
dev.off()
acf(x_low$x.filt)
plotts.sample.wge(x_low$x.filt)

# b) Apply a third-order high-pass Butterworth filter to the original
#    realization with cutoff of 0.2
x_high =butterworth.wge(x,order=3,type='high',cutoff = 0.2)
dev.off()
acf(x_high$x.filt)
plotts.sample.wge(x_high$x.filt)
# c) Apply a third-order low-pass Butterworth filter with cutoff point
#    of 0.2 to the high-pass filter to realization in (b)
x_high_low = butterworth.wge(x_high$x.filt,order=3,type='low',cutoff = 0.2)
dev.off()
acf(x_high_low$x.filt)
plotts.sample.wge(x_high_low$x.filt)



# Using the signal-plus-noise realization generated in Problem 2.1, 
# difference the data.  That is, compute the realization of length 99
# given by : y = Xt - Xt-1, where Xt denotes the original realization.
# Plot the following:
#   a)	The difference data (i.e., yt)
#   b)	The sample autocorrelations
#   c)	The Parzen window-based spectral density estimate using the
#       default truncation point
# Discuss the effect of the differencing.  What type of filter 
# (high-pass or low-pass) is the difference?  How does it compare
# with the high-pass Butterworth filter for filtering out the 
# frequency 0.05?

#Create a High Pass Filter (Difference Filter) lag=1
dif_1=diff(x,lag=1)
dev.off()
plot(dif_1,type='l',ylim=c(-7, 7))
acf(dif_1)
parzen.wge(dif_1[!is.na(dif_1)]) 
plotts.sample.wge(dif_1)


#Additional Problem
# Apply a 5-point moving average to the series you created in 2.1.
# How does it compare to the difference and Butterworth filters?
# Specifically, is it a low pass or high pass filter?  
  
#Create a Low Pass Filter (moving average filter) 5pt
#?filter() filter(x, filter, method = c("convolution", "recursive"),
#                 sides = 2, circular = FALSE, init)
ma_5=filter(x,rep(1,5)/5)
plot(ma_5, type='l', ylim=c(-6,6))
parzen.wge(ma_5[!is.na(ma_5)]) #removes na values
plotts.sample.wge(ma_5[!is.na(ma_5)])


##### Problem 3.4
#Using the same random number seed in each case, generate realizations
# of length 200 from AR(1) processes (1-phi*B)X_t = a_t for phi = +/- 0.9
# , +/- 0.5 and var(a_t) = 1.
#  a) Plot the true autocorrection
#  b) Plot the true spectral density
#  c) Find (sigma_x)^2
#  d) Plot the realization
#  e) Plot the sample autocorrelation
#  f) For phi = 0.9, repeat steps (a-c) with (sigma_a)^2 = 10
#     What differences and similarities do you observe?

# Not possible to use a seed value when generating a true
# plotts.true.wge(n=100, phi=0, theta=0, lag.max=25, vara = 1)

# Generate Realization/ACF/SPectral Density of phi = 0.9
x_.9 = plotts.true.wge(n=200, phi=0.9) 
x_.9_Gen = gen.arma.wge(n=200, phi=0.9,vara = 1, sn=1)

# Generate Realization/ACF/SPectral Density of phi = -0.9
x_N.9 = plotts.true.wge(n=200, phi=-0.9)
x_N.9_Gen = gen.arma.wge(n=200, phi=-0.9,vara = 1, sn=1)

# Generate Realization/ACF/SPectral Density of phi = 0.9
x_.5 = plotts.true.wge(n=200, phi=0.5) 
x_.5_Gen = gen.arma.wge(n=200, phi=0.5,vara = 1, sn=1)

# Generate Realization/ACF/SPectral Density of phi = -0.9
x_N.5 = plotts.true.wge(n=200, phi=-0.5)
x_N.5_Gen = gen.arma.wge(n=200, phi=-0.5,vara = 1, sn=1)

# Generate Realization/ACF/SPectral Density of phi = 0.9 var=10
x_.9V10 = plotts.true.wge(n=200, phi=0.9,vara = 10) 
x_.9V10Gen = gen.arma.wge(n=200, phi=0.9,vara = 10, sn=1)

# Generate Realization/ACF/SPectral Density of phi = -0.9 var=10
x_N.9V10 = plotts.true.wge(n=200, phi=-0.9,vara = 10)
x_N.9V10_Gen = gen.arma.wge(n=200, phi=-0.9,vara = 10, sn=1)

# Generate Realization/ACF/SPectral Density of phi = 0.9 var=10
x_.5V10 = plotts.true.wge(n=200, phi=0.5, vara = 10) 
x_.5V10_Gen = gen.arma.wge(n=200, phi=0.5,vara = 10, sn=1)

# Generate Realization/ACF/SPectral Density of phi = -0.9 var=10
x_N.5V10 = plotts.true.wge(n=200, phi=-0.5, vara = 10)
x_N.5V10_Gen = gen.arma.wge(n=200, phi=-0.5,vara = 10, sn=1)


