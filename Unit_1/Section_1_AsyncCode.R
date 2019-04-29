#  MSDS 6373- Time Series Analysis - Section 1

#  Team Member:  Jeffery Lancon
#  
#  Date: 04/28/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 1)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))


#------------------ Screencast for Stationary Mean ------------------
#--------------------------------------------------------------------

a1 = gen.sigplusnoise.wge(100,coef=c(5,0),freq = c(.1,0), psi = c(3,0),vara = 3, plot = FALSE)
b1 = gen.sigplusnoise.wge(100,coef=c(5,0),freq = c(.1,0), psi = c(3,0),vara = 3, plot = FALSE)
c1 = gen.sigplusnoise.wge(100,coef=c(5,0),freq = c(.1,0), psi = c(3,0),vara = 3, plot = FALSE)
d1 = gen.sigplusnoise.wge(100,coef=c(5,0),freq = c(.1,0), psi = c(3,0),vara = 3, plot = FALSE)
e1 = gen.sigplusnoise.wge(100,coef=c(5,0),freq = c(.1,0), psi = c(3,0),vara = 3, plot = FALSE)
plot(a1,type = "l")
lines(b1,col = "blue", type = "l")
lines(c1,col = "red", type = "l")
lines(d1,col = "green", type = "l")
lines(e1,col = "purple", type = "l")


#----------------------

a2 = gen.sigplusnoise.wge(100,coef=c(5,0),freq = c(.1,0), psi = c(runif(1,0,2*pi),0),vara = 3, plot = FALSE)
b2 = gen.sigplusnoise.wge(100,coef=c(5,0),freq = c(.1,0), psi = c(runif(1,0,2*pi),0),vara = 3, plot = FALSE)
c2= gen.sigplusnoise.wge(100,coef=c(5,0),freq = c(.1,0), psi = c(runif(1,0,2*pi),0),vara = 3, plot = FALSE)
d2 = gen.sigplusnoise.wge(100,coef=c(5,0),freq = c(.1,0), psi = c(runif(1,0,2*pi),0),vara = 3, plot = FALSE)
e2 = gen.sigplusnoise.wge(100,coef=c(5,0),freq = c(.1,0), psi = c(runif(1,0,2*pi),0),vara = 3, plot = FALSE)
plot(a2,type = "l")
lines(b2,col = "blue", type = "l")
lines(c2,col = "red", type = "l")
lines(d2,col = "green", type = "l")

par(mfrow = c(2,1))
plot(a1,type = "l")
lines(b1,col = "blue", type = "l")
lines(c1,col = "red", type = "l")
lines(d1,col = "green", type = "l")
lines(e1,col = "purple", type = "l")
plot(a2,type = "l")
lines(b2,col = "blue", type = "l")
lines(c2,col = "red", type = "l")
lines(d2,col = "green", type = "l")
lines(e2,col = "purple", type = "l")

#------------------- Slide 60 Unit 1
y=gen.arma.wge(n=250)
Time = seq(1250,lenght.out=250)
plot(Time,y)

y = gen.arma.wge(n = 250)
Time = seq(1,250,length.out = 250)
plot(Time,y,type = 'l')

plotts.wge(y) #from tswge package


#------------------- Slide 85 & 93 Unit 1
# Autocorrelation of Time Series with Dependent
# Observations with tswge (Lag = 5)
Y5 =  c(5.1,5.2,5.5,5.3,5.1,4.8,4.5,4.4,4.6,4.6,4.8,5.2,5.4,5.6,5.4,5.3,5.1,
        5.1,4.8,4.7,4.5,4.3,4.6,4.8,4.9,5.2,5.4,5.6,5.5,5.5)
Time = seq(1,5,length = 30)
plot(Time,Y5, main = "Regression of Y on Time",ylim = c(4,6.2), cex.axis = 1)

acf(Y5,plot = FALSE, lag.max = 20)

acf(Y5,plot = TRUE, lag.max = 20)


#------------------- Slide 95 Unit 1
# Autocorrelation of Time Series with Independent
# Observations with tswge (Lag = 1)
Realization1 = gen.arma.wge(n = 250)
Time = seq(1,250,length.out = 250)
plot(Time,Realization1)

acf(Realization1,plot = FALSE,lag.max = 20)

acf(Realization1,plot = TRUE,lag.max = 20)

#----------------- Slide 102 Unit 1
bb = gen.arma.wge(500,.96,sn = 5)
plot(bb,type='l')
acf(bb)


#----------------- Slide 103 Unit 1
aa = gen.arma.wge(500,-0.96,sn = 2)
plot(aa,type='l')
acf(aa)


#--------------- Slide 106 & 107 Unit 1  - Example of Weak Stationary Timeseries
#---- Equal Covariance
Realization = gen.arma.wge(500,0.95,0,plot = TRUE,sn = 784)

par(mfrow=c(3,1))
acf(Realization[1:250],plot = TRUE,col='red')

acf(Realization[251:500],plot = TRUE,col='blue')

acf(Realization[101:350],plot = TRUE,col='green')
# Note all acf plots are nearly identical

### Second Example ----
Realization = gen.arma.wge(500,-0.95,0,plot = TRUE,sn = 45)

par(mfrow=c(3,1))
acf(Realization[1:250],plot = TRUE,col='red')

acf(Realization[251:500],plot = TRUE, col='blue')

acf(Realization[101:350],plot = TRUE, col ='green')
# Note all acf plots are nearly identical


#--------------- Slide 108 Unit 1  - Example of Weak Stationary Timeseries
#---- Un-Equal Covariance
data("doppler")
par(mfrow=c(1,1))
plot(doppler,type='l',xlim=c(0,2000),ylim = c(-0.5,0.5),xlab = 'Time')

par(mfrow=c(2,1))
acf(doppler[1:251],plot=TRUE, ylim = c(-1,1),col="blue")
acf(doppler[251:500],plot = TRUE,ylim = c(-1,1),col='red')

#----- Async Question 1.7.6 ------
# Which data set provides the most evidence against the stationary
# autocovariance (Condition 3)?
data('lavon')
data("noctula")

par(mfrow=c(6,1))
plot(lavon,type='l',xlim=c(0,120),ylim = c(min(lavon),max(lavon)),xlab = 'Time',col='blue')
plot(noctula,type='l',xlim=c(0,120),ylim = c(min(noctula),max(noctula)),xlab = 'Time',col='red')

acf(lavon[1:48],plot=TRUE, ylim = c(-1,1),col="blue")
acf(lavon[49:96],plot=TRUE, ylim = c(-1,1),col="blue")
acf(noctula[1:56],plot=TRUE, ylim = c(-1,1),col="red")
acf(noctula[57:112],plot=TRUE, ylim = c(-1,1),col="red")

#----- Async Question 1.8.2 ------
# Which data set provides the most evidence against the stationary
# autocovariance (Condition 3)?
data('whale')

par(mfrow=c(3,1))
plot(whale,type='l',xlim=c(0,120),ylim = c(min(whale),max(whale)),xlab = 'Time',col='blue')

acf(whale[1:60],plot=TRUE, ylim = c(-1,1),col="blue")
acf(whale[51:120],plot=TRUE, ylim = c(-1,1),col="blue")

#----- Async Question 1.8.4 ------
# Check out the US Treasury Bond Rate data (TenYearBondRate.csv).
# We will never know for sure, but pick the best answer with respect
# to the question: Is this time series stationary?
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))
TB <- read.csv(file='10_year_bond_rate_2010-2015.csv', header=TRUE, sep=",")
head(TB)

Price = TB$Adj.Close
head(Price)

par(mfrow=c(3,1))
plot(Price,type='l',xlim=c(0,length(Price)),ylim = c(min(Price),max(Price)),xlab = 'Time',col='blue')

acf(Price[1:750],plot=TRUE, ylim = c(-1,1),col="blue")
acf(Price[751:1500],plot=TRUE, ylim = c(-1,1),col="blue")

mean(Price)

#---------- Slide 127

xdf = read.csv(file.choose(),header = TRUE)
x = as.numeric(paste(xdf$Adj.Close))
x = x[!is.na(x)]
n=length(x) #n = 1509
nlag=1508 #n-1
m=mean(x) # 2.457421
v=var(x,na.rm = TRUE) # 0.350049
gamma0=var(x)*(n-1)/n # 0.3498171
aut=acf(x,lag.max=1508) #n-1
sum=0
for (k in 1:nlag) {sum=sum+(1-k/n)*aut$acf[k+1]*gamma0}
vxbar=2*sum/n+gamma0/n #note the mult of sum by 2
vxbar

Alt_vxbar = (1 + 2*sum)*(v/n)

# Confidence Interval - Slide 128

# 95% Confidence Interval for Adj.Close bond data
MOE = 1.96*sqrt(vxbar)
LL = mean(x) - MOE
UL = mean(x) + MOE

print(paste0("Lower Limit: ",LL))
print(paste0("Upper Limit: ",UL))
