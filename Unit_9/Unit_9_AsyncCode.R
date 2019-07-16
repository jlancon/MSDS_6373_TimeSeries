#  MSDS 6373- Time Series Analysis - Unit 9 Async Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 07/01/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 7)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

#----Section 9.3.2 ARMA(p,q) Parameter Estimation ---- Slide 17 - 18
# Generating Predictions using tswge

# AR(2,1) Model  (1 - 1.6B + 0.8B2)(X_t - 50) = (1-0.8B)a_t,  vara = 5
x21 = gen.arma.wge(n=100, phi = c(1.6,-0.8),theta = c(0.8),vara = 5, sn=55)
x21 = x21+50
plotts.wge(x21)
?est.arma.wge()
est.arma.wge(x21,p=2,q=1)
    # $phi
    # [1]  1.6443723 -0.8146435
    # 
    # $theta
    # [1] 0.8699951
    # $avar
    # [1] 5.186772
mean(x21)
    # [1] 49.9592

# Estimated Model:
#(1- 1.644B + 0.814B2)(X_t - 49.959) = (1-0.867B)a_t,  vara = 5.19


# AR(4) Model  (1 + 0.7B2 - 0.1B3 + 0.72B4)(X_t - 20) = a_t,  vara = 10
x40 = gen.arma.wge(n=100, phi = c(0,-0.7,0.1,-0.72),vara = 10, sn=72)
x40 = x40+20
plotts.wge(x40)
?est.ar.wge()
est.ar.wge(x40,p=4,type = 'mle')
    # $phi
    # [1]  0.05656755 -0.68759141  0.11266029 -0.68184277
    # $avar
    # [1] 8.776949
mean(x40)
    #[1] 19.98281

# Estimated Model:
#(1 0.0566B + 0.6876B2 - 0.1127B3 + 0.6818B4)(X_t - 19.98) = a_t,  vara = 8.776



#----Concept Check 9.3.3 ----
# Consider Model (1 - 0.3B + 0.7B2)(X_t - 37) = (1 + 0.4B)a_t var_a=4
# Generate Realization of 200 observations (seed=27)
# Estimate Parameters
?gen.arma.wge()
x9.3.3 = gen.arma.wge(n=200, phi = c(0.3,-.7),theta = c(-0.4), vara = 4,sn=27)
x9.3.3 = x9.3.3 + 37
plotts.wge(x9.3.3)

est.arma.wge(x9.3.3, p=2,q=1)
    # $phi
    # [1]  0.3123135 -0.6911635
    # $theta
    # [1] -0.5347094
    # $avar
    # [1] 3.943131
mean(x9.3.3)
    # [1] 37.17206

#----Section 9.5.6 AR(p) Parameter Estimation ---- Slide 36 - 37
# Generating Predictions using tswge function
#?est.ar.wge()
# Generate Data from AR(2): (1-1.6B+0.9B^2)X_t = a_t

x=gen.arma.wge(n=200,phi=c(1.6,-.9),vara=2,sn=33)
plotts.wge(x)

# Yule-Walker Estimates
x.yw=est.ar.wge(x,p=2,type='yw')
    # $phi
    # [1]  1.5699655 -0.8723009
    # $avar
    # [1] 2.123846
    # $aic
    # [1] 0.7832286
x.yw

# Burg Estimates
x.burg=est.ar.wge(x,p=2,type='burg')
x.burg
    # $phi
    # [1]  1.584557 -0.886247
    # $avar
    # [1] 2.120722
    # $aic
    # [1] 0.7817567

# MLE Estimates
x.mle=est.ar.wge(x,p=2,type='mle')
x.mle$phi
    # [1]  1.5806671 -0.8807305
    # $avar
    # [1] 2.121261
    # $aic
    # [1] 0.7820105

#ACF and Spectral Density
plotts.sample.wge(x)


#----Concept Check 9.5.7 ---- Burg Estimates (Model from 9.3.3)
# Consider Model (1 - 0.3B + 0.7B2)(X_t - 37) = (1 + 0.4B)a_t var_a=4
# Generate Realization of 200 observations (seed=27)
# Create Realization
x9.3.3 = gen.arma.wge(n=200, phi = c(0.3,-0.7),theta = c(-0.4), vara = 4,sn=27)
x9.3.3 = x9.3.3 + 37
plotts.wge(x9.3.3)

#Estimate Parameters - Burg
x9.5.7_burg = est.ar.wge(x9.5.7, p=2,type = 'burg')
# Answer: None of the above.  YW and Burg are only for AR models

#----Concept Check 9.5.8 ---- Burg Estimates
# Consider Model (1 - 0.3B + 0.7B2)(X_t - 37) = a_t var_a=4
# Generate Realization of 200 observations (seed=27)
# Create Realization
x9.5.8 = gen.arma.wge(n=200, phi = c(0.3,-0.7), vara = 4,sn=27)
x9.5.8 = x9.5.8 + 37
plotts.wge(x9.5.8)

#Estimate Parameters - Burg
x9.5.8_burg = est.ar.wge(x9.5.8, p=2,type = 'burg')


#----Section 9.6.1 AR(p) Comparing Model Estimation ---- Slide 39 - 41
# Generating Predictions using tswge function
# Consider Model (1-2.195B + 1.994B2 - 0.796B3)X_t = a_t
x=gen.arma.wge(n=100,phi=c(2.195,-1.994,.796), sn=53)
x.yw=est.ar.wge(x,p=3,type='yw')
    # Coefficients of Original polynomial:  
    #   1.3413 -0.5734 0.1059

x.burg=est.ar.wge(x,p=3,type='burg')
    # Coefficients of Original polynomial:  
    #   2.1204 -1.9000 0.7531 

x.mle=est.ar.wge(x,p=3,type='mle')
    # Coefficients of Original polynomial:  
    #   2.1411 -1.9164 0.7616 


#----Section 9.7 Estimation: Noise Variance ---- Slide 49
# Generating Predictions using tswge function
# Consider Model (1-2.195B + 1.994B2 - 0.796B3)X_t = a_t
x=gen.arma.wge(n=100,phi=c(2.195,-1.994,.796), sn=53)
x.mle=est.ar.wge(x,p=3,type='mle')
  # Coefficients of Original polynomial:  
  #   2.1411 -1.9164 0.7616

# Check out the white noise variance estimate avar
x.mle$avar
    # [1] 1.007455

# Note that the 'residuals' are teh a_hats found by backcasting
# avar is the mean of these squared residuals (the variance assumes zero mean)
mean(x.mle$res^2)
    # [1] 1.007455

mean(x$mle$res^2)


#----Concept Check 9.8.3 ---- Model Identification
# Dataset 'MaybeWhiteNoise1.csv
# Identify if it is white noise
# Create Realization
WhiteNoise1 = read.csv(file.choose(),header=TRUE)
plotts.wge(WhiteNoise1$x)
acf(WhiteNoise1$x)
# The sample autocorrelations are consistent with white noise, and the 
# realization looks to have a mean of zero and constant variance.

#----Concept Check 9.8.4 ---- Model Identification
# Dataset 'MaybeWhiteNoise2.csv
# Identify if it is white noise
# Create Realization
WhiteNoise2 = read.csv(file.choose(),header=TRUE)
plotts.wge(WhiteNoise2$x)
acf(WhiteNoise2$x)
# The sample autocorrelations look to be consistent with white noise
# although the realization shows evidence of nonconstant standard deviation.


#----Section 9.10.2 AIC Demo ---- Slide 86 - 87
# Generating Predictions using tswge function
# Consider Model (1-2.55B + 2.42B2 - 0.855B3)X_t = a_t vara=1
#  (1 - 0.95B)(1 - 1.6B + 0.9B2)X_t = a_t
# fig3.16a is the realization from the AR(3) model 

data("fig3.16a")
# Plotts.sample.wge() provides a look at the data
plotts.sample.wge(fig3.16a)

aic.wge(fig3.16a,p=0:5,q=0:2, type='aic')
    # $type
    # [1] "aic"
    # $value
    # [1] 0.07040062
    # $p
    # [1] 3
    # $q
    # [1] 0
    # $phi
    # [1]  2.5245680 -2.3447622  0.8104239
    # $theta
    # [1] 0
    # $vara
    # [1] 1.030867

mean(fig3.16a)
    # [1] 1.572201

# Final Model (1-2.525B + 2.344B2 - 0.810B3)(X_t - 1.572) = a_t var_a=1.031


# Another example with AR(2,1)
# Consider Model (1-1.6B + 0.9B2)(X_t - 10) = (1-0.8B)a_t  var_ahat=1.0
x=gen.arma.wge(n=100,phi=c(1.6,-.9),theta=.8,sn=67)
x=x+10
plotts.sample.wge(x)

# no type listed below so it will use aic
aic.wge(x,p=0:8,q=0:4)
# picks ARMA(2,1)
    # $type
    # [1] "aic"
    # $value
    # [1] 0.1534323
    # $p
    # [1] 2
    # $q
    # [1] 1
    # $phi
    # [1]  1.6194830 -0.9131788
    # $theta
    # [1] 0.868127
    # $vara
    # [1] 1.076196

# Final Model: (1-1.62B + 0.91B2)(X_t - 10.08) = (1-0.87B)a_t  var_ahat=1.08

est.arma.wge(x,p=2,q=1)
    # Coefficients of Original polynomial:  
    #   1.6195 -0.9132 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.6195B+0.9132B^2    0.8867+-0.5557i      0.9556       0.0891



#----Section 9.10.2 AIC Demo ---- Slide 94 - 97
# Selection of p & q using AIC5  - (ARMA(3,1))
# Consider Model (1-2.30B + 1.92B2 - 0.56B3)(X_t - 30) = (1 + 0.8B)a_t vara=1

x31=gen.arma.wge(n=75,phi=c(2.3,-1.92,+0.56),theta=-0.8,sn=61)
x31=x31+30
plotts.sample.wge(x31)
aic5.wge(x31,p=0:8,q=0:2)
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 20    6    1 0.04601791
    # 23    7    1 0.05788330
    # 11    3    1 0.06646122
    # 26    8    1 0.07820027
    # 14    4    1 0.08249288

est.arma.wge(x31,p=6,q=1)
    # Coefficients of Original polynomial:  
    #   2.1358 -1.8220 0.8034 -0.5187 0.5518 -0.2954 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.7790B+0.8890B^2    1.0006+-0.3518i      0.9429       0.0538
    # 1-1.1361B+0.7250B^2    0.7835+-0.8749i      0.8515       0.1338
    # 1+0.7792B+0.4583B^2   -0.8500+-1.2080i      0.6770       0.3476

# picks (6,1) try BIC
aic5.wge(x31,p=0:8,q=0:2,type="bic")
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 11    3    1  0.2209604
    # 9     2    2  0.2613963
    # 14    4    1  0.2678919
    # 20    6    1  0.2932166
    # 17    5    1  0.2990913
# BIC picks (3,1) – decide to use it
# showing est.arma.wge results for ARMA(3,1)

est.arma.wge(x31,p=3,q=1)
    # Coefficients of Original polynomial:  
    #   2.0570 -1.5284 0.3760 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.5523B+0.7449B^2    1.0419+-0.5068i      0.8631       0.0720
    # 1-0.5047B              1.9813               0.5047       0.0000
    # $theta
    # [1] -0.9392223
    # $avar
    # [1] 0.9353148
mean(x31)
    # mean is 25.74

#Final Model:  (1-2.06B + 1.53B2 - 0.38B3)(X_t - 25.74) = (1 + 0.94)a_t Var_ahat = 0.94
#              (1-1.55B + 0.74B2)(1-0.50B)(X_t - 25.74) = (1 + 0.94)a_t 


#----Concept Check 9.10.5,6,7 ---- Model Identification
# Dataset 'inflation.csv
# Identify the favored ARMA(p,q) model
# using AIC5
Data = read.csv(file.choose(),header=TRUE)
plotts.wge(Data$Inflation)
plotts.sample.wge(Data$Inflation)
acf(Data$Inflation) #Definitely autocorrelation

aic5.wge(Data$Inflation,p=0:5,q=0:3, type='aic')
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 20    4    3   2.108402
    # 18    4    1   2.109741
    # 8     1    3   2.110716
    # 10    2    1   2.110736
    # 23    5    2   2.111800

aic5.wge(Data$Inflation,p=0:5,q=0:3, type='bic')
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 10    2    1   2.134678
    # 7     1    2   2.137678
    # 8     1    3   2.140643
    # 11    2    2   2.142690
    # 14    3    1   2.142817

# We should look at the ACF and spectral density to gather more 
# information about the characteristics and features of the data. In 
# addition we should look at the factor table for each model to help match 
# the model with those characteristics. We will also need to use our 
# intuition and any domain knowledge we may have to choose a model that 
# makes the most sense and will be the most useful (because both of them 
# are likely not the “correct” model).



#----Concept Check 9.12.2 ---- Model Identification
# Dataset 'armawhatpq1.csv
# Model Identification: Box-Jenkins
# Identify the favored ARMA(p,q) model
Data = read.csv(file.choose(),header=TRUE)
plotts.wge(Data$x)
plotts.sample.wge(Data$x)
acf(Data$x)

aic5.wge(Data$x,p=0:5,q=0:3, type='aic')
    # Five Smallest Values of  aic 
    #       p    q         aic
    # 11    2    2 -0.03227869
    # 14    3    1 -0.03216856
    # 18    4    1 -0.03150759
    # 15    3    2 -0.03148804
    # 22    5    1 -0.03093857

aic5.wge(Data$x,p=0:5,q=0:3, type='bic')
    # Five Smallest Values of  bic 
    #       p    q         bic
    # 11    2    2 -0.01827643
    # 14    3    1 -0.01816630
    # 18    4    1 -0.01470489
    # 15    3    2 -0.01468533
    # 21    5    0 -0.01357632

DataIn = read.csv(file.choose(),header=TRUE)
plotts.wge(DataIn$Inflation)
plotts.sample.wge(DataIn$Inflation)
acf(DataIn$Inflation)
dev.off()
pacf(DataIn$Inflation,lag.max=25)

#----Section 9.13.1 Put it All together Demo ---- Slide 125
# Dataset 'PutItTogether.csv
# Determine model
Together = read.csv(file.choose(),header=TRUE)
plotts.wge(Together$x)
aic5.wge(Together$x)
    # Five Smallest Values of  aic 
    #       p    q         aic
    # 9     2    2 -0.07985840
    # 12    3    2 -0.06708593
    # 15    4    2 -0.05356675
    # 18    5    2 -0.03918021
    # 16    5    0 -0.01122233

# Use estimate p & q to get estimates of phi and thetas
m = est.arma.wge(Together$x,p = 2,q=2)
    # Coefficients of Original polynomial:  
    #   0.9465 -0.8278 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9465B+0.8278B^2    0.5717+-0.9387i      0.9098       0.1629

# forecast model forward 20 segments
fore.arma.wge(Together$x, phi = m$phi,theta = m$theta,n.ahead = 20)


#----Section 9.13.2 Put it All together Demo ---- Slide 127 - 129
# Dataset 'Jetfuel.csv
# Original Estimated Model:  (1-0.967B)(X_t-2.20) = (1 + 0.477B)a_t
# Determine model and forecast next 8 months of data
Fuel = read.csv(file.choose(),header=TRUE)
plotts.sample.wge(Fuel$Price)

aic5.wge(Fuel$Price)
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 5     1    1  -4.195998
    # 10    3    0  -4.190446
    # 6     1    2  -4.172064
    # 8     2    1  -4.171285
    # 13    4    0  -4.165614

# Use estimate p & q to get estimates of phi and thetas
Jetest = est.arma.wge(Fuel$Price,p = 1,q = 1)
    # Coefficients of Original polynomial:  
    #   0.9674 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9674B              1.0337               0.9674       0.0000

# forecast model forward 20 segments
fore.arma.wge(Fuel$Price, phi = Jetest$phi,theta = Jetest$theta,n.ahead = 8)


#----Section 9.13.3 Put it All together Demo ---- Slide 131 - 134
# Dataset Canadian Lynx 'llynx'
# Determine model and forecast next 8 months of data
# Original Model Estimate: (1 - 0.7B - 0.1B2 + 0.2B3 + 0.3B4)(X_t - 2.9) = (1 + 0.6B)a_t
# ARMA(4,1)

# Determine model
data("llynx")
plotts.wge(llynx)

#Use aic5.wge() or aic.wge() to identify estimates of p and q
aic5.wge(llynx)
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 14    4    1  -2.951518
    # 16    5    0  -2.951301
    # 13    4    0  -2.949515
    # 12    3    2  -2.942965
    # 17    5    1  -2.936873

# Estimate the model of p and q to get phi(s) and theta(s)
estllynx = est.arma.wge(llynx, p = 4,q = 1)
    # Coefficients of Original polynomial:  
    #   0.6823 0.0630 -0.2056 -0.2602 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.4916B+0.8311B^2    0.8974+-0.6308i      0.9116       0.0975
    # 1+0.8093B+0.3131B^2   -1.2923+-1.2343i      0.5596       0.3787

fore.arma.wge(llynx,phi = estllynx$phi, theta = estllynx$theta,n.ahead = 24,limits = FALSE)


# As a note TONG's model for llynx dataset is and ARMA(11,0).  We will expand out
# the defaults of aic5.wge() to include p = 0:15, q= 0:2, to see if we can
# replicate.

aic5.wge(llynx,p = 0:15, q = 0:2)
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 37   12    0  -3.128705
    # 35   11    1  -3.121965
    # 34   11    0  -3.121655
    # 36   11    2  -3.115068
    # 40   13    0  -3.113269

# Estimate the model of p and q to get phi(s) and theta(s)
estllynx_TONG = est.arma.wge(llynx, p = 11,q = 0)
    # Coefficients of Original polynomial:  
    #   1.1676 -0.5446 0.2662 -0.3094 0.1540 -0.1463 0.0569 -0.0294 0.1346 0.2021 -0.3394 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.5707B+0.9697B^2    0.8099+-0.6126i      0.9847       0.1031
    # 1-0.5516B+0.8686B^2    0.3175+-1.0249i      0.9320       0.2022
    # 1-1.7772B+0.8325B^2    1.0674+-0.2487i      0.9124       0.0364
    # 1+0.5722B+0.8125B^2   -0.3521+-1.0521i      0.9014       0.3014
    # 1+1.3817B+0.7656B^2   -0.9024+-0.7014i      0.8750       0.3948
    # 1+0.7781B             -1.2851               0.7781       0.5000


fore.arma.wge(llynx,phi = estllynx_TONG$phi,n.ahead = 24,limits = FALSE)


#----Concept Check 9.13.4,5,6 Put it All together Demo
# Dataset 'TexasGasPrice.csv'

# Determine model and forecast next 8 months of data
Gas = read.csv(file.choose(),header=TRUE)
plotts.sample.wge(Gas$Price)

#Use aic5.wge() or aic.wge() to identify estimates of p and q
aic5.wge(Gas$Price) #AIC Model
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 7     2    0  -6.131525
    # 8     2    1  -6.124641
    # 10    3    0  -6.123937
    # 9     2    2  -6.116413
    # 13    4    0  -6.116182
aic5.wge(Gas$Price,type = 'bic')
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 7     2    0  -6.082895
    # 8     2    1  -6.059802
    # 10    3    0  -6.059098
    # 5     1    1  -6.049422
    # 6     1    2  -6.049014

# Estimate the model of p and q to get phi(s) and theta(s)
estGas = est.arma.wge(Gas$Price, p = 2,q = 0)
    # Coefficients of Original polynomial:  
    #   1.3812 -0.4077 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9536B              1.0487               0.9536       0.0000
    # 1-0.4276B              2.3386               0.4276       0.0000

#Forecast 1 and 8 steps
foreGas = fore.arma.wge(Gas$Price,phi = estGas$phi,n.ahead = 8,limits = FALSE)
# 1 week ahead
foreGas$f[1] #[1] 2.087504
foreGas$f[8] #[1] 2.094299