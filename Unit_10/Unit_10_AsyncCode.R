#  MSDS 6373- Time Series Analysis - Unit 10 Async Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 07/04/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 7)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf

#----Section 10.2.2 ARIMA(p,d,q) ---- Slide 08 - 10
# Dow Jones Dataset
data("dowjones2014")
aic.wge(dowjones2014,p=0:5,q=0:2)
    # $value
    # [1] 9.480696
    # $p
    # [1] 1
    # $q
    # [1] 0
    # $phi
    # [1] 0.9815898
    # $theta
    # [1] 0
    # $vara
    # [1] 12897.94
mean(dowjones2014)
    # [1] 16777.69

# Selected AR(1) model:
#(1 - 0.9816B)(X_t - 16678) = a_t
# Question: Should we use this (stationary) model or a model that
# includes a (1-B) factor?


#----Section 10.3.2 ARIMA(p,d,q) ---- Slide 17 - 23
# First Example
# Model with a Unit Root
# (1 - B)(1 - 1.2B + 0.8B2)X_t = a_t

# y= artrans.wge(x,phi.tr = 1)
# This simply means that y(i) = x(i) - x(i-1)
# y has a length of n-1
x = c(1,3,6,10,25)
y= artrans.wge(x,phi.tr = 1)
y

# Generate ARIMA(2,1,0) data
xd1 = gen.arima.wge(n=200,phi = c(1.2,-0.8),d=1,sn=56)
#difference the data
xd1.dif = artrans.wge(xd1,phi.tr = 1)

# Use aic5 to determine model of differenced data
aic5.wge(xd1.dif,p=0:5,q=0:2)
    # Five Smallest Values of  aic 
    #       p    q         aic
    # 7     2    0 -0.04329736
    # 10    3    0 -0.04194853
    # 8     2    1 -0.04141050
    # 9     2    2 -0.03293416
    # 13    4    0 -0.03228853
est.ar.wge(xd1.dif,p=2) #
    # Coefficients of Original polynomial:  
    #   1.2650 -0.8022 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.2650B+0.8022B^2    0.7884+-0.7905i      0.8957       0.1252
    # $phi
    # [1]  1.265005 -0.802240
    # $avar
    # [1] 0.929324
mean(xd1)
    # [1] -21.21951

# Predicted Model (Non-Stationary)
# (1-B)(1-1.265B + 0.802B2)(X_t + 21.22) = a_t  var_ahat = -.930

#---Lets try the same model as a stationary model ---
# Use aic5 to determine model of differenced data
aic5.wge(xd1,p=0:5,q=0:2)
    # Five Smallest Values of  aic 
    # p    q         aic
    # 13    4    0 -0.02334489
    # 11    3    1 -0.02210195
    # 10    3    0 -0.02086132
    # 12    3    2 -0.01514269
    # 16    5    0 -0.01471386
est.ar.wge(xd1,p=4)
    # Coefficients of Original polynomial:  
    #   2.3546 -2.2921 1.0456 -0.1102 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9957B              1.0043               0.9957       0.0000
    # 1-1.2139B+0.7629B^2    0.7955+-0.8233i      0.8735       0.1277
    # 1-0.1450B              6.8953               0.1450       0.0000
    # $phi
    # [1]  2.3546329 -2.2920781  1.0456011 -0.1101713
    # $avar
    # [1] 0.9295115

# Predicted Model (Stationary)
# (1-2.35B + 2.29B2 - 1.05B3 + 0.11B4)(X_t + 21.22) = a_t  var_ahat = -.929


#----Section 10.3.3 Concept Check
# Dataset '10_year_bond_rate_2010-2015.csv'
# Plot the series and ACF
# Discuss your thoughts on the stationarity of the data.
bonds = read.csv(file.choose(),header=TRUE)
plotts.sample.wge(bonds$Close)

#The bond data from 2010-2015 has a substantial wandering 
#behavior with no appearance of wandering around a mean value.
#The ACF shows a slowly dampening autocorrelation, indicative 
#of a non-stationary realization. The sample spectral density 
#also show a dominant frequency at f=0.  Preliminary conclusion
#would be to classify this realization as non-stationary.

#----Section 10.3.4 Concept Check
#Differencing the data
# Does the data now look like white noise?
bonds_1 = artrans.wge(bonds$Close, phi.tr = 1)
acf(bonds_1)
#The differenced data look like white noise, and the 
#ACF has very small autocorrelations that are mostly all 
#inside the limit lines.

#----Section 10.3.5 Concept Check
# Use aic5.wge (and the AIC) to estimate the p and q of 
#an ARMA model fit to the differenced data. 
#Is p = 0, q = 0 in the top five?
aic5.wge(bonds_1)
    # Five Smallest Values of  aic 
    # p    q        aic
    # 1    0    0  -5.853517
    # 2    0    1  -5.853260
    # 4    1    0  -5.853195
    # 5    1    1  -5.853110
    # 7    2    0  -5.852798

#----Section 10.3.6 Concept Check
#Now let’s model the bond data as a stationary model. 
#Use aic5.wge() on the original data (not differenced). 
#What model does the AIC favor?
aic5.wge(bonds$Close)
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 4     1    0  -5.855658
    # 5     1    1  -5.855329
    # 7     2    0  -5.855259
    # 8     2    1  -5.854569
    # 10    3    0  -5.854552


#----Section 10.4.1 ARIMA(p,d,q) ---- Slide 24- 26
# Model with two Unit Roots
# (1 - B)^2(1 - 1.2B + 0.6B2)X_t = a_t vara = 1.0
x=gen.arima.wge(n=200,d=2,phi = c(1.2,-0.6),sn=132,vara = 1.0)
# difference the data
x.d1 = artrans.wge(x,phi.tr = 1)
# difference the data again since the differenced data
#  still has non-stationary ARIMA characteristics
x.d2 = artrans.wge(x.d1,phi.tr = 1)
# x.d2 appears to be stationary now
aic5.wge(x.d2)
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 7     2    0 0.05568245
    # 10    3    0 0.05857863
    # 8     2    1 0.05972395
    # 9     2    2 0.06047420
    # 16    5    0 0.06126876
#AIC picks an AR(2)
# Estimate the phi parameters
est.ar.wge(x.d2,p=2)
    # Coefficients of Original polynomial:  
    #   1.2724 -0.6827 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.2724B+0.6827B^2    0.9319+-0.7722i      0.8263       0.1101
    # $phi
    # [1]  1.2724446 -0.6827008
    # $avar
    # [1] 1.026015
mean(x)
    # [1] 1511.921
#Estimated model with 2 roots:
# (1 - B)^2(1 - 1.27B + 0.683B2)(X_t - 1512) = a_t var_ahat = 1.03


#----Section 10.5.2 ARIMA(2,1,0) Forecasting ---- Slide 32- 33
# Forecasting Model with one Unit Roots
# Fitted Model (Non-Stationary)
# (1-B)(1-1.265B + 0.802B2)(X_t + 21.22) = a_t  var_ahat = -.930
fore.aruma.wge(xd1,d=1,phi = c(1.27,-0.8),n.ahead = 50)

# Forecasting Model with two Unit Roots
# Fitted Model (Non-Stationary)
# (1 - B)^2(1 - 1.27B + 0.683B2)(X_t - 1512) = a_t var_ahat = 1.03
fore.aruma.wge(x, d=2, phi = c(1.27,-0.683),n.ahead = 50)
?fore.aruma.wge()


#----Section 10.6.1 ARIMA(p,d,q) Dow Jones Example ---- Slide 35- 41

data("dowjones2014")
dow = dowjones2014
plotts.sample.wge(dow)
# Difference the data
dow.1 = artrans.wge(dow,phi.tr = 1)
aic5.wge(dow.1,p=0:5,q=0:2)
    #Five Smallest Values of  aic 
    #       p    q        aic
    # 14    4    1   9.449719
    # 18    5    2   9.478392
    # 1     0    0   9.479449
    # 3     0    2   9.482976
    # 7     2    0   9.484535
#Selected a ARMA(4,1) model, now we estimate the phis
est.arma.wge(dow.1,p=4,q=1)
    # Coefficients of Original polynomial:  
    #   0.9266 0.1356 -0.0240 -0.1243 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.8512B              1.1749               0.8512       0.0000
    # 1-0.6821B              1.4661               0.6821       0.0000
    # 1+0.6067B+0.2140B^2   -1.4172+-1.6321i      0.4626       0.3638
    # $phi
    # [1]  0.9265955  0.1355600 -0.0240474 -0.1242563
    # $theta
    # [1] 0.9999996
    # $avar
    # [1] 12111.49
mean(dow)
    #[1] 16777.69
# Notice the theta close to the unit circle
#ARMA(4,1) model
#(1-0.93B-0.14B^2+0.2B^3+0.12B^4)(X_t-16778)=(1-B)a_t var_ahat = 12112

#Fractored version of model
#(1-0.85B)(1-0.68B)(1+0.61B+0.214B^2)(X_t-16778)=(1-B)a_t
# The two factors(1-0.85B) and (1-0.68B) work to cancel out the (1-0.999996)MA factor

# Try BIC model selection
aic5.wge(dow.1,p=0:5,q=0:2, type = 'bic')
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 1    0    0   9.493495
    # 4    1    0   9.514914
    # 2    0    1   9.515016
    # 3    0    2   9.525113
    # 7    2    0   9.526672
#Selected a ARMA(0,0) model, white noise
acf(dow.1)
# Should have modeled the data as white noise

#Dow Model ARIMA(0,1,0)
#(1-B)(X_t - 16778) = a_t

#We can write the model without the mean
# True for all ARIMA models with d=>1

# Dow Jones Model
# (1-B)X_t = a_t
#Note that the differenced data Y_t = (1-B)X_t is white noise

vara_hat = (sd(dow.1))^2
    # [1] 13036.04

# Final Model: (1-B)X_t = a_t  var_ahat = 13036

fore.aruma.wge(dow,d=1,n.ahead=20,limits=FALSE)
# Simply predicts the last value



#----Section 10.7.1 ARIMA(p,d,q) General Model - Overfitting ---- Slide 47- 52
# Tiao/Tsay and Overfit Tables
# 1 Unit Root
# Model (1 - B)(1 - 1.2B + 0.8B^2)X_t = a_t
# generate ARIMA(2,1,0) data as before
xd1 = gen.arima.wge(n=200,phi=c(1.2,-0.8),d=1,sn=56)
# Fit an AR(6) and AR(8) to this realization
est.ar.wge(xd1,p=6,type='burg')
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9944B              1.0056               0.9944       0.0000
    # 1-1.1978B+0.7584B^2    0.7897+-0.8337i      0.8708       0.1293
    # 1-0.4472B+0.0881B^2    2.5391+-2.2157i      0.2967       0.1142
    # 1+0.2879B             -3.4738               0.2879       0.5000

est.ar.wge(xd1,p=8,type='burg')
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9951B              1.0049               0.9951       0.0000
    # 1-1.1324B+0.7314B^2    0.7741+-0.8763i      0.8552       0.1348
    # 1-1.0807B+0.4241B^2    1.2740+-0.8572i      0.6513       0.0943
    # 1+0.4968B             -2.0129               0.4968       0.5000
    # 1+0.3615B+0.2464B^2   -0.7334+-1.8761i      0.4964       0.3093


#----Section 10.7.3 Concept Check
#Use est.ar.wge and Burg estimates to fit an AR(6) and AR(8) to
# the bond-adjusted close prices, and verify that there is evidence
# of a (1-B) factor
# Dataset 10 Year Bond Rate 2010-2015.csv
bonds = read.csv(file.choose(),header=TRUE)
plotts.sample.wge(bonds$Adj.Close)
est.ar.wge(bonds$Adj.Close,p=6,type='burg')
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9965B              1.0035               0.9965       0.0000
    # 1+0.3041B+0.3020B^2   -0.5035+-1.7487i      0.5495       0.2946
    # 1+0.5472B             -1.8273               0.5472       0.5000
    # 1-0.8213B+0.2589B^2    1.5863+-1.1605i      0.5088       0.1005


#----Section 10.7.4 ARIMA(p,d,q) General Model - Overfitting ---- Slide 54- 59
# Tiao/Tsay and Overfit Tables
# 2 Unit Root
# Model (1 - B)^2(1 - 1.2B + 0.6B^2)X_t = a_t
# generate ARIMA(2,2,0) data as before
x = gen.arima.wge(n=200,phi=c(1.2,-0.6),d=2,sn=132)
# Fit an AR(8) and AR(10) to this realization
est.ar.wge(x,p=8,type='burg')
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.9913B+0.9918B^2    1.0039+-0.0208i      0.9959       0.0033
    # 1-1.1880B+0.6220B^2    0.9550+-0.8341i      0.7887       0.1143
    # 1+0.6064B             -1.6490               0.6064       0.5000
    # 1-0.5554B              1.8004               0.5554       0.0000
    # 1-0.2302B+0.2848B^2    0.4042+-1.8297i      0.5337       0.2154
est.ar.wge(x,p=10,type='burg')
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.9896B+0.9900B^2    1.0048+-0.0208i      0.9950       0.0033
    # 1-1.3363B+0.7233B^2    0.9237+-0.7275i      0.8505       0.1062
    # 1-0.7636B              1.3096               0.7636       0.0000
    # 1-0.5498B+0.5347B^2    0.5141+-1.2673i      0.7312       0.1887
    # 1+0.7074B             -1.4136               0.7074       0.5000
    # 1+0.5761B+0.3306B^2   -0.8713+-1.5052i      0.5750       0.3335
# NOte: (1-B)(1-B) = 1-2B+B^2)

#----Section 10.7.5 Concept Check
#Useing the dataset (Zero_One_or_TwoRootsOfOne.csv) determine if there are 
# zero, one , or two factors with roots of 1. Use the ACF and overfitting with
# factor tables.
Roots = read.csv(file.choose(),header=TRUE)
plotts.sample.wge(Roots$x)
est.ar.wge(Roots$x,p=8,type='burg')
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.9528B+0.9554B^2    1.0220+-0.0472i      0.9774       0.0073
    # 1-0.6838B              1.4625               0.6838       0.0000
    # 1-0.3805B+0.4006B^2    0.4749+-1.5069i      0.6329       0.2014
    # 1+0.9128B+0.3417B^2   -1.3357+-1.0689i      0.5845       0.3926
    # 1+0.1329B             -7.5268               0.1329       0.5000
Roots.d1 = artrans.wge(Roots$x,phi.tr = 1)
Roots.d2 = artrans.wge(Roots.d1,phi.tr = 1)
plotts.sample.wge(Roots.d2)


#----Section 10.8.1 Dickey-Fuller Test for Unit Root ---- Slide ??- ??
# H_0: Model has a root of +1
# H_A: The model does not have a root of +1
library(tseries)
# 10 realization were generated from a stationary model (1-ph1B)X_t = a_t
#For phi1 = 0.9, 0.95, and 0.975
# Realizations length n=100 and 200
# In each case, we recorded the number of times out of 10 that the
# Dickey-Fuller test 'failed to reject' the null hypotheses (i.e incorrectly
# decided there was a unit root)

# phi1 = 0.9 n=100 (9/10 failed to reject) incorrect answer
# phi1 = 0.95 n=100 (9/10 failed to reject) incorrect answer
# phi1 = 0.975 n=100 (10/10 failed to reject) incorrect answer
# phi1 = 0.9 n=200 (5/10 failed to reject) incorrect answer
# phi1 = 0.95 n=200 (6/10 failed to reject) incorrect answer
# phi1 = 0.975 n=200 (8/10 failed to reject) incorrect answer

# Not a good indicator of non-stationary models

#----Section 10.8.2 Concept Check
# Generating data from a stationary series, to test the null hypothesis
# of the Dickey-Fuller (Ho: The model has a root of +1)(Nonstationary)
# Type I error : We fail reject the null hypothesis when it is truly true.
x = gen.arma.wge(200,phi = c(.9), sn = 5)
adf.test(x)
    # Augmented Dickey-Fuller Test
    # data:  x
    # Dickey-Fuller = -3.1162, Lag order = 5, p-value = 0.1086
    # alternative hypothesis: stationary

iterations = 200
phi = 0.95
alpha = 0.05
counter = 0
for ( i in seq(1:iterations)){
  x = gen.arma.wge(200,phi = c(phi))
  c = adf.test(x)
  #print (c$alternative)
  if (c$p.value <= alpha) {
    print(c$p.value)
    counter = counter + 1
  }
}
DFpower = counter / iterations
print(paste0('Power of Dickey-Fuller test: ',DFpower))


#----Section 10.9.1 Seasonal Model Identification ---- Slide 70- 77
# How to identify a seasonal Model
# (1-B^s)X_t = a_t
# Plot the data
# Look at the ACFs and factor tables.
#Seasonal Factor Table s=4
factor.wge(phi = c(0,0,0,1))
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+1.0000B             -1.0000               1.0000       0.5000
    # 1+0.0000B+1.0000B^2    0.0000+-1.0000i      1.0000       0.2500
    # 1-1.0000B              1.0000               1.0000       0.0000
#Generate data with a seasonal factor
x = gen.aruma.wge(n=48,s=4,sn=23)
est.ar.wge(x,p=8,type = 'burg') #overfactoring to find roots
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+0.0148B+0.9826B^2   -0.0075+-1.0088i      0.9912       0.2512
    # 1+0.9905B             -1.0096               0.9905       0.5000
    # 1-0.9556B              1.0465               0.9556       0.0000
    # 1-0.5126B+0.2985B^2    0.8585+-1.6164i      0.5464       0.1723
    # 1+0.4753B             -2.1041               0.4753       0.5000
    # 1+0.0593B             -16.8658               0.0593       0.5000

x.s4 = artrans.wge(x,phi.tr = c(0,0,0,1))
# transformed data appears to be be white noise (ACFs)
aic5.wge(x.s4,type = 'bic')
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 1    0    0 0.06883316
    # 2    0    1 0.14400701
    # 4    1    0 0.14745558
    # 3    0    2 0.20123845
    # 7    2    0 0.22200295

vara_hat = (sd(x.s4))^2
    # [1] 1.005835

# Model: Y_t = (1-B^4)X_t   wnv = 1.006


#----Section 10.9.3 Seasonal Model Identification ---- Slide 70- 77
# How to identify a seasonal Model with ARMA terms
# (1-B^12)(1 - 1.5B + 0.8B^2)(X_t - 50) = a_t
# Plot the data
# Look at the ACFs and factor tables.
#Seasonal Factor Table s=12
factor.wge(phi = c(rep(0,11),1))
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.0000B+1.0000B^2    0.5000+-0.8660i      1.0000       0.1667
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1-1.7321B+1.0000B^2    0.8660+-0.5000i      1.0000       0.0833
    # 1+1.0000B+1.0000B^2   -0.5000+-0.8660i      1.0000       0.3333
    # 1-0.0000B+1.0000B^2    0.0000+-1.0000i      1.0000       0.2500
    # 1+1.7321B+1.0000B^2   -0.8660+-0.5000i      1.0000       0.4167
    # 1+1.0000B             -1.0000               1.0000       0.5000
#Generate data with a seasonal factor
x = gen.aruma.wge(n=200,s=12,phi=c(1.5,-0.8),sn=87)
x = x+50
d15 =est.ar.wge(x,p=15,type = 'burg') #overfactoring to find roots
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.7269B+0.9978B^2    0.8653+-0.5034i      0.9989       0.0839
    # 1+1.0027B+0.9948B^2   -0.5040+-0.8667i      0.9974       0.3338
    # 1-1.0036B+0.9944B^2    0.5046+-0.8666i      0.9972       0.1661
    # 1-0.0142B+0.9907B^2    0.0072+-1.0047i      0.9953       0.2489
    # 1+1.7320B+0.9904B^2   -0.8744+-0.4951i      0.9952       0.4180
    # 1+0.9788B             -1.0217               0.9788       0.5000
    # 1-0.9522B              1.0502               0.9522       0.0000
    # 1-1.5043B+0.7732B^2    0.9727+-0.5892i      0.8793       0.0867
    # 1+0.1523B             -6.5660               0.1523       0.5000
plotts.sample.wge(x,lag.max = 60)

# Transform data Y_t = (1-B^12)X_t
y = artrans.wge(x,phi.tr = c(rep(0,11),1))
plotts.sample.wge(y)
# transformed data appears to still contain patterns based on the
# realizatoin and the ACF plot
aic5.wge(y,p=0:13,q=0:3,type = 'bic') # good rule of thumb, overfactor
# the transformed data with p = the seasonal factor removed.
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 7     2    0  0.1192915
    # 10    3    0  0.1422426
    # 8     2    1  0.1424706
    # 9     2    2  0.1692218
    # 13    4    0  0.1700640
# Suggests an ARMA(2,0) be fit to the transformed data
est.ar.wge(y,p=2)
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.4653B+0.7597B^2    0.9644+-0.6215i      0.8716       0.0911
    # $phi
    # [1]  1.4652837 -0.7596714
    # $avar
    # [1] 1.036377
mean(x)
    # [1] 49.77867
#Final Model: (1-B^12)(1-1.465B + 0.76B^2)(X_t-49.78) = a_t vara_hat = 1.036


#----Section 10.9.4 Concept Check
# Use SWA flight delay data (SWADelay.csv).
# Use est.ar.wge() to overfit an AR(15) model with Burg estimates.
# Look at the factor tables and answer the question.
# What is the root associated with the factor that would be matched
# with the (1-B) term from the 1-B12) factor table?
SWA = read.csv(file.choose(),header=TRUE)
SWADelay = SWA$arr_delay
delay15 =est.ar.wge(SWADelay,p=15,type = 'burg')
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9700B              1.0310               0.9700       0.0000
    # 1-0.9946B+0.9342B^2    0.5323+-0.8872i      0.9666       0.1640
    # 1-1.6636B+0.9322B^2    0.8923+-0.5259i      0.9655       0.0848
    # 1+0.9229B             -1.0836               0.9229       0.5000
    # 1+1.6328B+0.8443B^2   -0.9669+-0.4994i      0.9189       0.4241
    # 1-0.1460B+0.8319B^2    0.0877+-1.0928i      0.9121       0.2372
    # 1+0.7038B+0.7209B^2   -0.4882+-1.0719i      0.8490       0.3180
    # 1+0.7885B+0.5243B^2   -0.7520+-1.1584i      0.7241       0.3416
    # 1-0.7182B              1.3924               0.7182       0.0000
factor.wge(phi = c(rep(0,11),1))
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.0000B+1.0000B^2    0.5000+-0.8660i      1.0000       0.1667
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1-1.7321B+1.0000B^2    0.8660+-0.5000i      1.0000       0.0833
    # 1+1.0000B+1.0000B^2   -0.5000+-0.8660i      1.0000       0.3333
    # 1-0.0000B+1.0000B^2    0.0000+-1.0000i      1.0000       0.2500
    # 1+1.7321B+1.0000B^2   -0.8660+-0.5000i      1.0000       0.4167
    # 1+1.0000B             -1.0000               1.0000       0.5000
# Root associated with the (1-B) factor in the realization  = 1/(0.9700) = 1.0310


#----Section 10.10.1 Seasonal Model Forecasting ---- Slide 89- 92
# How to identify a seasonal Model with ARMA terms
# (1-B^12)(1 - 1.47B + 0.76B^2)(X_t - 49.78) = a_t  var_ahat = 1.04
# Forecast 36 steps ahead

x = gen.aruma.wge(n=200,s=12,phi = c(1.5,-0.8),sn=87)
x = x+50
fore.aruma.wge(x,s=12,phi = c(1.47,-0.76),n.ahead = 36,lastn = FALSE)

x.pred = fore.aruma.wge(x,s=12,phi = c(1.47,-0.76),n.ahead = 36,lastn = TRUE)
n = 36
ASE = mean((x[(length(x)-n+1):(length(x))]-x.pred$f)^2)
print(paste0('ASE: ',round(ASE,3)))
    # [1] "ASE: 26.673"


#----Section 10.10.2 Seasonal Model Examples ---- Slide 93- 102
# Airline Dataset (Log)

data('airlog')
plotts.sample.wge(airlog)
est.ar.wge(airline,p=15,type = 'burg') #overfit to find seasonality
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.7226B+0.9960B^2    0.8648+-0.5062i      0.9980       0.0843
    # 1-1.0036B+0.9960B^2    0.5038+-0.8662i      0.9980       0.1662
    # 1-0.0218B+0.9861B^2    0.0111+-1.0070i      0.9930       0.2482
    # 1+0.9801B+0.9816B^2   -0.4993+-0.8772i      0.9907       0.3323
    # 1-1.9619B+0.9626B^2    1.0190+-0.0201i      0.9811       0.0031
    # 1+1.7068B+0.9580B^2   -0.8908+-0.5003i      0.9788       0.4186
    # 1+0.8120B             -1.2316               0.8120       0.5000
    # 1+0.6027B             -1.6591               0.6027       0.5000
    # 1-0.1765B              5.6650               0.1765       0.0000
# Checking (1-B^12) factor table
factor.wge(phi = c(rep(0,11),1))
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.0000B+1.0000B^2    0.5000+-0.8660i      1.0000       0.1667
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1-1.7321B+1.0000B^2    0.8660+-0.5000i      1.0000       0.0833
    # 1+1.0000B+1.0000B^2   -0.5000+-0.8660i      1.0000       0.3333
    # 1-0.0000B+1.0000B^2    0.0000+-1.0000i      1.0000       0.2500
    # 1+1.7321B+1.0000B^2   -0.8660+-0.5000i      1.0000       0.4167
    # 1+1.0000B             -1.0000               1.0000       0.5000
# All factors line up with the exception of (1-B) in the (1-B^12) 
# seasonal factor.
# Did notice that there was a 1-1.9619B+0.9626B^2 factor in the est.
# This factors into (1-B)(1-B), accounting for the missing (1-B)
# This would indicate the we have 2 factors in the data (1-B^12)
# and an additional (1-B)
# Y_t = (1-B)(1-B^12)

# Difference the data
d1 = artrans.wge(airlog,phi.tr = 1)
plotts.sample.wge(d1)
# Trend part appears to have been removed

# Difference the data again to remove the seasonality
d1.12 = artrans.wge(d1,phi.tr = c(rep(0,11),1))
plotts.sample.wge(d1.12)
#Appears to stationary, No trending/seasonality present

# Find a model for the transformed/differenced data
aic5.wge(d1.12,p=0:13,q=0:3)
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 50   12    1  -6.423649
    # 53   13    0  -6.413487
    # 54   13    1  -6.411660
    # 51   12    2  -6.410749
    # 49   12    0  -6.404858
# AIC picks a ARMA(12,1)

# Try using the 'bic' to see what model it picks
aic5.wge(d1.12,p=0:13,q=0:3,type = 'bic')
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 2    0    1  -6.231113
    # 5    1    0  -6.223197
    # 6    1    1  -6.196229
    # 4    0    3  -6.195271
    # 3    0    2  -6.194583
# Picks a much smaller ARMA(0,1)  or a ARMA(1,0)
# Doesn't really match the ACF from the differenced data, therefore,
# we will proceed with the ARMA(12,1) model

# Estimate the parameters
lair.est = est.arma.wge(d1.12,p=12,q=1)
    # $phi
    # [1]  0.00846435  0.07967002 -0.10710183 -0.02069279  0.08039524  0.04050674
    # [7] -0.05516681  0.03629598  0.13277817 -0.05297698 -0.12340975 -0.40343184
    # $theta
    # [1] 0.4536856
    # $avar
    # [1] 0.001310444
mean(airlog)
    # [1] 5.542176

#Final Model
# (1-B)(1-B^12)phi_12(B)(X_t - 5.54) = (1-0.45B)a_t  wnv = 0.0013
#phi_12 = 1 - 0.008B-0.080B^2 + 0.107B^3 + 0.021B^4 - 0.080B5 - 0.041B^6
#         + 0.055B^7 - 0.036B^8 - 0.133B^9 + 0.053B^10 + 0.0123B^11 + 0.403B^12



#----Section 10.11.1 Seasonal Model Examples ---- Slide 104- 112
# Pennsylvania Montly Temp Dataset
data('patemp')
plotts.sample.wge(patemp)
est.ar.wge(patemp,p=15,type = 'burg') #overfit to find seasonality
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.7268B+0.9973B^2    0.8657+-0.5032i      0.9987       0.0838
    # 1-0.8743B              1.1438               0.8743       0.0000
    # 1-0.8987B+0.7623B^2    0.5894+-0.9820i      0.8731       0.1640
    # 1+0.2200B+0.7559B^2   -0.1455+-1.1410i      0.8694       0.2702
    # 1+1.0663B+0.7499B^2   -0.7109+-0.9100i      0.8660       0.3556
    # 1+1.6060B+0.7278B^2   -1.1034+-0.3957i      0.8531       0.4452
    # 1+0.7830B             -1.2772               0.7830       0.5000
    # 1+0.0141B+0.4368B^2   -0.0161+-1.5130i      0.6609       0.2517
    # 1-0.6487B              1.5415               0.6487       0.0000

# Do not see the factors for (1-B^12) present.  

# Stationarizing the data using the only factor that is close 
# to the unit circle (1 - 1.727B + B2). Then reevaluate the model.
# Y_t = (1 - 1.727B + B^2)X_t
y.tr = artrans.wge(patemp,phi.tr = c(1.727,-1))
plotts.sample.wge(y.tr)
# Transformed and no looks stationary

# Use aic5 to identify ARMA model (AIC)
aic5.wge(y.tr,p=0:13,q=0:3)
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 37    9    0   2.358745
    # 41   10    0   2.368625
    # 38    9    1   2.368973
    # 46   11    1   2.370346
    # 45   11    0   2.378032
#AIC Selected AR(9)

# Use aic5 to identify ARMA model (BIC)
aic5.wge(y.tr,p=0:13,q=0:3,type = 'bic')
    # Five Smallest Values of  bic 
    # p    q        bic
    # 13    3    0   2.499181
    # 17    4    0   2.520588
    # 3     0    2   2.522096
    # 14    3    1   2.522932
    # 22    5    1   2.531133
#BIC Selected AR(3)

#Jusgement call but we are using the simplier model AR(3)

#Build our final model with AR(3) model
mod = est.ar.wge(y.tr,p=3)
    # Coefficients of Original polynomial:  
    #   -1.1371 -0.8341 -0.4081 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+0.7503B             -1.3328               0.7503       0.5000
    # 1+0.3868B+0.5439B^2   -0.3556+-1.3085i      0.7375       0.2922
    # $phi
    # [1] -1.1371311 -0.8341139 -0.4080797
    # $avar
    # [1] 10.83451
mean(patemp)
    # [1] 52.62667


# Final Model - Considered an ARUMA model (Seasonal but not unit roots)
#(1-1.727B + B^2)(1 + 1.14B + 0.83B^2 + 0.41B^3)(X_t - 52.63) = a_t var_ahat = 10.83



#----Section 10.11.1 Signal + Noise Model Examples (OLS)---- Slide 114- 121
# We would like to test the type I error rate of the OLS 
# (ordinary least squares) estimates of the slope when the residuals
# are correlated. To do this we will generate a realization from a 
# model with no trend (b1 = 0) and see how often Ho: β_1=0 is 
# rejected.

# Generate realization with no trend (X_t = 0 + 0t + z_t)
# Where z_t is generated from an AR(1) model 
# with phi = 0.95 (1-0.95B)Z_t = a_t

x = gen.sigplusnoise.wge(100,b0=0,b1=0,phi = c(0.95),sn=28)
t = seq(1,100,1)
df = data.frame(x = x, t= t)
fit = lm(x~t,data = df)
summary(fit)
#What is the p-value for the test Ho: β_1=0 Ha: β_1≠0 ? 
#(Much smaller than 0.05)

#Does this test suggest that there is a trend (nonzero slope)?
# Yes

#Was this a type I error?
#(Yes) We rejected when Ho: β_1=0 was true

iterations = 100
obs = 100
phi = 0.99
alpha = 0.05
counter = 0
for ( i in seq(1:iterations)){
  x = gen.sigplusnoise.wge(n=obs,b0=0,b1=0,phi = c(phi))
  t = seq(1,obs,1)
  df = data.frame(x = x, t= t)
  fit = lm(x~t,data = df)
  sum = summary(fit)
  #sum$coefficients[2,4]
  if (sum$coefficients[2,4] <= alpha) {
    #print(sum$coefficients[2,4])
    counter = counter + 1
  }
}
Type_I_ErrorRate = counter / iterations

print(paste0('Type I Error Rate: ',Type_I_ErrorRate*100,'%'))



#----Section 10.13.2 Cochrane-Orcutt Test---- Slide 138 - 144
# Learning how to implement Cochrane-Orcutt Methods 
#To do this we will generate a realization from a 
# model with no trend (b1 = 0) and see how often Ho: β_1=0 is 
# rejected.

# Generate realization with no trend (X_t = 0 + 0t + z_t)
# Where z_t is generated from an AR(1) model 
# with phi = 0.95 (1-0.95B)Z_t = a_t

library(orcutt)
x = gen.sigplusnoise.wge(100,b0=0,b1=0,phi = 0.95,sn=21)

t = seq(1,100,1)
df = data.frame(x = x, t= t)

# Using OLS methods
fit = lm(x~t, data = df)
summary(fit)
    # Coefficients:
    #               Estimate    Std. Error  t value   Pr(>|t|)    
    # (Intercept)   -3.078211   0.450925    -6.826    7.31e-10 ***
    #   t            0.030428   0.007752    3.925     0.000161 ***
# Detects a statitically significant slope (b1) in the realization
# Type I error

# Using the Cochrane-Orcutt method
cfit = cochrane.orcutt(fit)
summary(cfit)

    #               Estimate    Std. Error  t value   Pr(>|t|)
    # (Intercept)   -3.360341   2.811634    -1.195    0.2349
    # t             0.041244    0.040656    1.014     0.3129
# Correctly Rejects the null hypothesis of having b1 not equal to 0,
# with a p.value = 0.3129.

# Mini-Simulation: Using Cochrane-Orcutt methods
# Detect the number of time we incorrectly reject the
# Null hypothesis b1=0  : A type I error?
#(Yes) We rejected when Ho: β_1=0 was true

iterations = 100
obs = 1000
phi = 0.90
alpha = 0.05
counter = 0
for ( i in seq(1:iterations)){
  x = gen.sigplusnoise.wge(n=obs,b0=0,b1=0,phi = c(phi),plot = FALSE)
  t = seq(1,obs,1)
  df = data.frame(x = x, t= t)
  fit = lm(x~t,data = df)
  sum = summary(fit)
  cfit = cochrane.orcutt(fit)
  sum_cfit = summary(cfit)
  #sum$coefficients[2,4]
  if (sum_cfit$coefficients[2,4] <= alpha) {
    print(sum_cfit$coefficients[2,4])
    counter = counter + 1
  }
}
Type_I_ErrorRate = counter / iterations
print(paste0('Type I Error Rate: ',Type_I_ErrorRate*100,'%'))


#----Section 10.13.3,4 Concept Check
# Use SWA flight delay data (SWADelay.csv).
# Management wants to know if there is evidence that the mean delay
# is increasing over time, or if the recent increase is a 
# “random trend” and can be expected to revert back to some lesser
# mean number of delays in the future.

# Fit a simple linear regression to the data using arr_delay as the
# response and time as the explanatory variable (you will have to
# create this variable). What is the p-value?
SWA = read.csv(file.choose(),header=TRUE)
SWADelay = SWA$arr_delay
t = seq(1,length(SWADelay),1)
SWADelaydf = data.frame(x = SWADelay, t= t)

# Using OLS methods
fit = lm(x~t,data = SWADelaydf)
sum = summary(fit)
    # Coefficients:
    #               Estimate    Std. Error  t value   Pr(>|t|)    
    # (Intercept)   20680.32    2231.07     9.269     < 2e-16 ***
    #   t             160.16    21.74       7.367     6.6e-12 ***

# Concept Check 10.13.5 Now use the Orcutt package and the 
# Cochrane.Orcutt function to adjust the p-value for AR(1) errors.
# What is the adjusted p-value?
cfit = cochrane.orcutt(fit)
sum_cfit = summary(cfit)
    #               Estimate    Std. Error  t value   Pr(>|t|)    
    # (Intercept)   21077.921   3844.600    5.482     1.455e-07 ***
    #   t           156.428     37.067      4.220     3.925e-05 ***

