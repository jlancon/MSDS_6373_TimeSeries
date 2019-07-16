#  MSDS 6373- Time Series Analysis - Unit 6 Async Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 06/11/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 1)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))


#----Section 6.2.4 Signal + Noise Modeling---- Slide 85 & 93 Unit 1
# Generating Signal + Noise using tswge

# Random generating signal plus noise models
?gen.sigplusnoise.wge()
#Generate a realization from the model
# x(t)=coef[1]*cos(2*pi*freq[1]*t+psi[1])+coef[2]*cos(2*pi*freq[2]*t+psi[2])+a(t)

gen.sigplusnoise.wge(100,b0=2,b1=4,vara = 100)
gen.sigplusnoise.wge(100,b0=0,b1=0,vara = 10)
gen.sigplusnoise.wge(100,b0=0,b1=0,phi=0.975,vara = 10)


# - Generate the following model:
# X_t = 5cos(2pi*(0.1)t + 2.5) + Z_t   (Z_t ~ AR(1))

gen.sigplusnoise.wge(100,coef = c(5.0,0),freq = c(0.1,0),psi = c(2.5,0),phi=c(0.975),var=5)

# Generating an AR(4)
# Generating: (1 - 0.975B)(1 - 0.2B + 0.45B2)(1 + 0.53B)X_t = a_t
# (1 - 0.645B + 0.02225B^2 - 0.969B^3 - 0.2325B^4)X_t = a_t
# Minic global temperature data

# mult.wge can take the factored form and merge them together
parms = mult.wge(c(0.975),c(0.20,-0.45),c(-0.53))
parms$model.coef
gen.arma.wge(160,phi = parms$model.coef,vara = 1)

# Generating ARIMA(0,1,0) Slide 33-34
# ARIMA(0,1,0)
  # (1-B)X_t = a_t
  x = gen.arima.wge(200,phi=0,var=1,d=1,sn=31)
  acf(x)

  # (1-B)(1 - 1.5B + 0.8B^2)X_t = a_t
  x2=gen.arima.wge(n=200, phi=c(1.5,-.8),d=1)
  plotts.sample.wge(x2)
  
  ## (1-B)^2(1 - 1.5B + 0.8B^2)X_t = a_t
  x3=gen.arima.wge(n=200, phi=c(1.5,-.8),d=2,theta=-.8)
  plotts.sample.wge(x3)
  
  
# Differencing Data - Async 6.4.1 Slide 40-41

# the following command differences the data in x
?artrans.wge()
y=artrans.wge(x,phi.tr=1)
# This simply means that y(i) = x(i) - x(i-1)
# y has length n-1 because x(1) has no x(0) before it.
# Example
x = c(1,3,6,10,25)
y = artrans.wge(x,phi.tr = 1)
y # shows the 4 differences


# Stationarize ARIMA(0,1,0) Data - Async 6.4.2 Slide 43-44

x = gen.arima.wge(200,phi = 0, var = 1,d = 1,sn = 31)
acf(x)
#Differencing
Xtilde = artrans.wge(x,1)
plotts.wge(Xtilde)
acf(Xtilde)
aic5.wge(artrans.wge(x,1))

# Stationarize ARIMA(2,1,0) Data - Async 6.4.2 Slide 48-51
# (1-1.5B + 0.8B^2)(1-B)X_t = a_t
a = gen.arima.wge(200,phi=c(1.5,-0.8),var = 1, d=1,sn=31)
acf(a)

model = mult.wge(fac1=c(1.5,-0.8),fac2=1)
    # $char.poly
    # 1 - 2.5*x + 2.3*x^2 - 0.8*x^3 
    # 
    # $model.coef
    # [1]  2.5 -2.3  0.8
factor.wge(model$model.coef)
    # Coefficients of Original polynomial:  
    #   2.5000 -2.3000 0.8000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1-1.5000B+0.8000B^2    0.9375+-0.6092i      0.8944       0.0917

# Pulling the (1-B) out of the model
x = gen.arima.wge(200,phi=c(1.5,-0.8),var = 1, d=1,sn=31)
firstDif = artrans.wge(x,phi.tr = 1) #Take out the (1-B)
par(mfrow=c(1,1))
aic5.wge(firstDif) #Check the structure of the noise
parzen.wge(firstDif)


# Stationarize ARIMA(2,2,1) Data - Async 6.4.2 Slide 52-54
# (1-1.5B + 0.8B^2)(1-B)^2X_t = (1 + 0.8B)a_t
a = gen.arima.wge(200,phi=c(1.5,-0.8),theta = -0.8, var = 1, d=2,sn=21)
acf(a)
p=parzen.wge(a,trunc=40)
ar = mult.wge(fac1=c(1.5,-0.8),fac2=1, fac3 = 1)
factor.wge(ar$model.coef)
    # Coefficients of Original polynomial:  
    #   3.5000 -4.8000 3.1000 -0.8000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1-1.5000B+0.8000B^2    0.9375+-0.6092i      0.8944       0.0917

factor.wge(-.8)
    # Coefficients of Original polynomial:  
    #   -0.8000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+0.8000B             -1.2500               0.8000       0.5000

###### Now we are going to remove the (1-B)^2 from the model
x = gen.arima.wge(200,phi=c(1.5,-0.8),var = 1, d=2,sn=31)
firstDif = artrans.wge(x, 1) #Take out the (1-B)
SecondDif = artrans.wge(firstDif, 1) #Take out another (1-B)
par(mfrow=c(1,1))
parzen.wge(SecondDif)
aic5.wge(SecondDif)

model2 = mult.wge(fac1=c(1.5,-0.8),fac2=1,fac3 = 1)

# Concept Checks 6.4.3
# (1 - 0.6B + 0.8B^2)(1-B)^2X_t =  (1 + 0.3B)a_t
# ARIMA(2,2,1)
# True ACF at lag = 5 rho5
x2 = gen.arima.wge(500,phi=c(0.6,-0.8),theta=c(-0.3),var = 1, d=2,sn=37)
acf5 = acf(x2)

parms = mult.wge(c(0.6,-0.8),c(1),c(1))
parms$model.coef
    # [1]  2.6 -3.0  2.2 -0.8
factor.wge(phi=c(2.6,-3.0,2.2,-0.8))
    # Coefficients of Original polynomial:  
    #   2.6000 -3.0000 2.2000 -0.8000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1-0.6000B+0.8000B^2    0.3750+-1.0533i      0.8944       0.1956

parzen.wge(x2)



# Concept Checks 6.4.4
# (1 - 0.6B + 0.8B^2)(1-B)^2X_t =  (1 + 0.3B)a_t
# ARIMA(2,2,1)
x3 = gen.arima.wge(500,phi=c(0.6,-0.8),var = 1, d=2,sn=35)
firstDif = artrans.wge(x3, 1) #Take out the (1-B)
SecondDif = artrans.wge(firstDif, 1) #Take out another (1-B)
par(mfrow=c(1,1))
aic5.wge(SecondDif)
# Five Smallest Values of  aic 
# p    q        aic
# 7     2    0 0.05428990
# 17    5    1 0.05553058
# 11    3    1 0.05797019
# 8     2    1 0.05825895
# 10    3    0 0.05826038


# Stationarize ARUMA Models Data - Async 6.5.4 Slide 70-71
# (1-B^4)X_t = a_t
x1 = gen.aruma.wge(n=80,s=4,sn=6)
plotts.sample.wge(x1)

# (1-B+0.6B^2)(1-B^4)X_t = (1 + 0.5)a_t
x2=gen.aruma.wge(n=80, phi=c(1,-0.6),s=4,theta=-0.5, sn=6)
plotts.sample.wge(x2)

factor.wge(phi = c(1,-0.6))
factor.wge(phi = c(-0.5))

# (1-B+0.6B^2)(1-B^12)X_t = (1 + 0.5)a_t
x3=gen.aruma.wge(n=180, phi=c(1,-.6),s=12,theta=-.5, sn=6)
plotts.sample.wge(x3,lag.max=48)


# Concept Checks 6.5.5
# (1 - 0.6B + 0.94B^2)(1-B^6)X_t =  (1 + 0.3B)a_t
x4=gen.aruma.wge(n=200, phi=c(0.6,-0.94),s=6,theta=-0.3, sn=19)

## Stationarize ARUMA Models Data - Async 6.6.1 Slide 74
# (1-B^4)X_t = a_t
x=gen.aruma.wge(n=80, s=4, sn = 81) #tswge function to generate ARIMA and Seasonal Models
Dif = artrans.wge(x,c(0,0,0,1)) #Take out the (1-B^4)
aic5.wge(Dif) #Check the structure of the noise

## Stationarize ARUMA Models Data - Async 6.6.1 Slide 75
#(100.4B-0.6B^2 + 0.74B^3)(1-B^12)X_t = (1+ 0.7B)a_t
x=gen.aruma.wge(n=80, phi = c(.4,.6,-.74), theta = c(-.7), s=12, sn = 31)
Dif = artrans.wge(x,c(rep(0,11),1)) #Take out the (1-B^12)
aic5.wge(Dif) #Check the structure of the noise


# Concept Checks 6.6.2
# (1 - 0.6B + 0.80B^2)(1-B^12)X_t =  (1 + 0.3B + 0.7B^2)a_t
x5=gen.aruma.wge(n=500, phi=c(0.6,-0.80),s=12,theta=c(-0.3,-0.7), sn=37)
Dif5 = artrans.wge(x5,c(rep(0,11),1))
aic5.wge(Dif5) #Check the structure of the noise

## ARUMA Factot Tables - Async 6.6.4 Slide 77 - 79
# (1-B^4)
factor.wge(phi = c(0,0,0,1))
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+1.0000B             -1.0000               1.0000       0.5000
    # 1+0.0000B+1.0000B^2    0.0000+-1.0000i      1.0000       0.2500
    # 1-1.0000B              1.0000               1.0000       0.0000

# (1-B^12)
factor.wge(phi = c(rep(0,11),1))
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.0000B+1.0000B^2    0.5000+-0.8660i      1.0000       0.1667
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1-1.7321B+1.0000B^2    0.8660+-0.5000i      1.0000       0.0833
    # 1+1.0000B+1.0000B^2   -0.5000+-0.8660i      1.0000       0.3333
    # 1-0.0000B+1.0000B^2    0.0000+-1.0000i      1.0000       0.2500
    # 1+1.7321B+1.0000B^2   -0.8660+-0.5000i      1.0000       0.4167
    # 1+1.0000B             -1.0000               1.0000       0.5000


## Stationarize ARUMA Models Data - Async 6.6.5 Slide 82
#(1 + 0.2B - 0.4B^2 - 0.49B^3 - 1B^12 + 0.2B^13 + 0.4B^14 + 0.49B^15)X_t = (1+ 0.92B)a_t
factor.wge(phi = c(-0.2,0.4,0.49,rep(0,8),1,0.2,-0.4,-0.49))

#looking at factor tables, we can reduce the above equation to:
#(1-0.88B)(1+1.08B + 0.55B^2)(1-B^12)X_t = (1+0.29B)a_t

factor.wge(c(0,0,0,1)) #Factor Table (1-B^4)
factor.wge(c(0,0,0,0,0,0,0,0,0,0,0,1))#Factor Table (1-B^12)
factor.wge(c(rep(0,3),1))
factor.wge(c(rep(0,11),1))
factor.wge(c(rep(0,4),1))#Factor Table (1-B^5)

#Try same with another mode;
#(1 + 0.3B - 0.8B^2 + 1B^5 - 0.3B^6 + 0.8B^7)X_t = (1 + 0.29B)a_t
factor.wge(phi = c(-0.3,0.8,0,0,1,0.3,-0.8))

# Concept Checks 6.6.6
# (1 + 0.5B - 0.2B^2 - 1B^4 - 0.5B^5 + 0.2B^6)X_t = (1-0.92B)a_t
factor.wge(c(-0.5,0.2,0,1,0.5,-0.2))

# Concept Checks 6.6.7
# (1 + 0.3B - 1.2B^2 - 0.4B^3 - 0.5B^5 - 1.0B^12 - 0.3B^13 + 1.2B^14 + 0.4B^15)X_t = a_t
factor.wge(c(-0.3,1.2,0.4,0,0.5,0,0,0,0,0,0,1,0.3,-1.2,-0.4))

# Airline Competition Async 6.7.2 Slide 90
data(airlog)
plotts.wge(airlog)
plotts.sample.wge(airlog)

SA1 = artrans.wge(airlog,1) # take first differences of the data
plotts.sample.wge(SA1)

SA12 = artrans.wge(airlog,c(rep(0,11),1))# take the 12th difference of the data (1-B12)

# take the 12th difference of the first difference (1-B)(1-B12)
SA1_12 = artrans.wge(SA1,c(rep(0,11),1)) 

Parzen = aic.wge(SA12, p = 13) #Phi(B)(1-B12)(Xt-mu) = at
Box = aic.wge(SA1_12, q = 13) #(1-B)(1-B12) (Xt-mu) = Theta(B)at
WoodwardAndGray = aic.wge(SA1_12, p = 12) # #Phi(B)(1-B)(1-B12) (Xt-mu) = at
Parzen$value
Box$value
WoodwardAndGray$value
# We will return to this competition when we use the model to forecast!

