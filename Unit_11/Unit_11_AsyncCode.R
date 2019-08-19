#  MSDS 6373- Time Series Analysis - Unit 11 Async Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 07/15/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 7)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf

#----Section 11.2.5 White Noise ARMA(p,q)---- Slide 21 - 25
# Generating ARMA(2,1) data
# (1-1.6B+0.9B^2)(X_t - 10) = (1-0.8B)a_t
x = gen.arma.wge(n=100, phi=c(1.6,-0.9),theta = 0.8,sn=67)
x = x + 10
plotts.sample.wge(x)

# Looking at data, we believe we have stationary data
# Model Data; determine p & q:
aic.wge(x,p=0:8,q=0:4)
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

# AIC picks a ARMA(2,1)
x21 = est.arma.wge(x,p=2,q=1)
    # $phi
    # [1]  1.6194830 -0.9131788
    # $theta
    # [1] 0.868127

mean(x)
    # [1] 10.07557

# Final Model
#  (1-1.62B + 0.91B^2)(X_t - 10.07) = (1 - 0.87B)a_t  sigma_ahat^2 = 1.077


# Next We Examine the Residuals
# X21$res: COntains residuals from the ARMA(2,1) fit

plotts.sample.wge(x21$res)
acf(x21$res)

# Use Ljung-Box test; K=24
ljung.wge(x21$res,p=2,q=1)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 24
    # $chi.square
    # [1] 20.92251
    # $df
    # [1] 21
    # $pval
    # [1] 0.4636851
# At 95% confidence, We fail to reject the null hypothesis of residual data
# is white noise (b=0), K=24; p-value 0.464

# Use Ljung-Box test
ljung.wge(x21$res,p=2,q=1,K=48)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 48
    # $chi.square
    # [1] 44.93891
    # $df
    # [1] 45
    # $pval
    # [1] 0.4745203

###############
# Based on Checks 1 and 2 the residuals from the fitted ARMA(2,1)
# model seem to be white noise.
##############


#----Section 11.2.6 White Noise Seasonal---- Slide 27 - 31
# Generating Seasonal Model (1-B)^12 data
# (1-B^12)(1-1.5B+0.8B^2)(X_t - 50) = a_t
x = gen.aruma.wge(n=200,phi = c(1.5,-0.8),s=12,sn=87)
x = x + 50
plotts.sample.wge(x,lag.max = 60)

# Overfit Tables
aic5.wge(x,p=0:20,q=0:2)
    # Five Smallest Values of  aic 
    # p    q        aic
    # 50   16    1 0.08904419
    # 60   19    2 0.12811603
    # 54   17    2 0.13663987
    # 63   20    2 0.17945929
    # 19    6    0 2.03900584
est.arma.wge(x,p=16,q=1)
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.7269B+0.9977B^2    0.8655+-0.5033i      0.9988       0.0838
    # 1+0.9920B+0.9970B^2   -0.4975+-0.8692i      0.9985       0.3327
    # 1-1.0074B+0.9955B^2    0.5060+-0.8652i      0.9977       0.1658
    # 1+1.7247B+0.9926B^2   -0.8688+-0.5027i      0.9963       0.4165
    # 1-0.0172B+0.9913B^2    0.0087+-1.0043i      0.9957       0.2486
    # 1+1.9674B+0.9690B^2   -1.0151+-0.0387i      0.9844       0.4939
    # 1-0.9428B              1.0607               0.9428       0.0000
    # 1-1.4899B+0.7586B^2    0.9820+-0.5949i      0.8710       0.0867
    # 1+0.0564B             -17.7432               0.0564       0.5000

# Looking at fractor tables: 
# The overfit ARMA model suggest a factor (1-B^12) in the Model

# We will transform the data by (1-B^12)
y = artrans.wge(x,phi.tr = c(rep(0,11),1))

plotts.sample.wge(y)
# The transformed data appears stationary but still contains correlated data and
# additional modeling should be performed (not white noise)

aic5.wge(y, p=0:10,q=0:2,type = 'bic')
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 7     2    0  0.1192915
    # 10    3    0  0.1422426
    # 8     2    1  0.1424706
    # 9     2    2  0.1692218
    # 13    4    0  0.1700640
# Model selected by BIC is AR(2)

# Model residuals
est.y = est.ar.wge(y,p=2)
    # Coefficients of Original polynomial:  
    #   1.4653 -0.7597 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.4653B+0.7597B^2    0.9644+-0.6215i      0.8716       0.0911
    # $avar
    # [1] 1.036377

mean(x)
  # [1] 49.7786

##### Final Model:
#(1-B^12)(1 - 1.47B + 0.76B^2)(X_t - 49.78) = a_t   sigma_ahat^2 = 1.036

### How about the residuals? #######
# Check 1
# Visual and ACF
plotts.wge(est.y$res)
acf(est.y$res)
# Plot of residuals have a 'random' appears to them
# ACF shows no statically significant correlation

# Check 2
# Ljung-Box test  H0: rho1=rho2=rho3...rhok = 0
#                 Ha:  at least one rho not = 0, for 1<=k<=K
# Use stationarized Data
ljung.wge(est.y$res,p=2,K=24)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 24
    # $chi.square
    # [1] 20.95537
    # $df
    # [1] 22
    # $pval
    # [1] 0.5234977
ljung.wge(est.y$res,p=2,K=48)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 48
    # $chi.square
    # [1] 47.62509
    # $df
    # [1] 46
    # $pval
    # [1] 0.4063877

# At 95% confidence, We fail to reject the null hypothesis of residual data
# is white noise (b=0), K=24; p-value 0.523    K=48; p-value 0.406
###############
# Based on Checks 1 and 2 the residuals from the fitted seasonal
# model seem to be white noise.
##############


# Working with Log Airline Data
#----Section 11.3.1 Log Airline Data ---- Slide 33 - 38

data('airlog')
plotts.sample.wge(airlog)
# Recall Previously fit model
# (1 - B)(1 - B^12)phi_12(B)(X_t - 5.54) = (1 - 0.45B)a_t   sigma_ahat^2 = 0.0013
#phi_12 = 1 - 0.008B - 0.080B^2 + 0.107B^3 + 0.021B^4 - 0.080B5 - 0.041B^6
#         + 0.055B^7 - 0.036B^8 - 0.133B^9 + 0.053B^10 + 0.0123B^11 + 0.403B^12

# Previously; Overfit the data with p=14 and p=16 and determined the
# the need to transform to obtain (1-B)(1-B^12) = d1.12

est.ar.wge(airlog,p=15,type = 'burg') #overfit to find seasonality
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
est.12.1 = est.arma.wge(d1.12,p=12,q=1)
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

# Residuals are in est.12.1$res
# Check 1
plotts.wge(est.12.1$res)
acf(est.12.1$res)

# Plot of residuals have a 'random' appears to them
# ACF shows no statically significant correlation

# Check 2
# Ljung-Box test  H0: rho1=rho2=rho3...rhok = 0
#                 Ha:  at least one rho not = 0, for 1<=k<=K
# Use stationarized Data
ljung.wge(est.12.1$res,p=12,q=1,K=24)
    # $test
    # [1] "Ljung-Box test"
    # $K [1] 24
    # $chi.square [1] 17.30648
    # $df [1] 11
    # $pval [1] 0.09913114

ljung.wge(est.12.1$res,p=12,q=1,K=48)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 48
    # $chi.square
    # [1] 35.93309
    # $df
    # [1] 35
    # $pval
    # [1] 0.4245906
# At 95% confidence, We fail to reject the null hypothesis of residual data
# is white noise (b=0), K=24; p-value 0.099    K=48; p-value 0.425
###############
# Based on Checks 1 and 2 the residuals from the fitted seasonal
# model seem to be white noise.
##############



#----Section 11.3.2 Concept Check
#Recall Professor Woodward’s Model for the log airline data:
#Step 1: Use the code below to fit the model.
#Step 2: Obtain and plot the residuals, and plot the ACF of the residuals.
phi1=c(-.36,-.05,-.14,-.11,.04,.09,-.02, .02,.17,.03,-.10,-.38)
for.Wood = gen.aruma.wge(n=144,s=12,d=1,phi = phi1,sn=47)







#----Section 11.4.2 Global Temperature Data ---- Slide 46 - 51
# Modeling Global Temperature Data ( Stationary Model)

data(hadley)
mean(hadley) # -0.1684937
plotts.sample.wge(hadley)

# Model Stationary Data
aic5.wge(hadley,p=0:6,q=0:1)
  # Five Smallest Values of  aic 
  #       p    q        aic
  # 8     3    1  -4.471114
  # 9     4    0  -4.470358
  # 10    4    1  -4.461045
  # 12    5    1  -4.460449
  # 13    6    0  -4.459879
# AIC picks an ARMA(3,1) stationary model

had.est=est.arma.wge(hadley,p=3,q=1)
# $phi: 1.2700171 -0.4685313 0.1911988
# $theta: 0.6322319
# $avar: 0.01074178

#Fitted ARMA(3,1) model
# (1 - 1.27B + 0.47B^2 - 0.19B^3)(X_t + 0.17) = (1 - 0.63B)a_t where sigma_ahat^2 = 0.0107

# Factored Form
#(1 - 0.99B)(1 - 0.28B + 0.19B^2)(X_t + 0.17) = (1 - 0.63B)a_t
# This is nearly 'non-stationary' model

# Check residuals
plotts.sample.wge(had.est$res,arlimits=TRUE)
# The residual sample autocorrelations stay sufficiently within the 95% limit lines

ljung.wge(had.est$res,p=3,q=1)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 24
    # $chi.square
    # [1] 20.59104
    # $df
    # [1] 20
    # $pval
    # [1] 0.4215446
ljung.wge(had.est$res,p=3,q=1,K=48)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 48
    # $chi.square
    # [1] 45.4633
    # $df
    # [1] 44
    # $pval
    # [1] 0.4108957
# At 95% confidence (alpha=0.05), We fail to reject the null hypothesis of residual data
# is white noise (b=0), K=24; p-value 0.422    K=48; p-value 0.411
###############
# Based on Checks 1 and 2 the residuals from the fitted seasonal
# model seem to be white noise.
# Special Note: The factor table shows a root close to the unit circle (0.99)
# which is nearly non-stationary.
##############

# Generated realizations of ARMA(3,1); to see if realizations are indicative of the 
# Temperture data

temp.real = gen.arma.wge(n=150,phi = had.est$phi,theta = had.est$theta,vara = had.est$avar)


#----Section 11.4.4 Global Temperature Data ---- Slide 52 - 64
# Modeling Global Temperature Data ( Non-Stationary Model)
# We have seen some indications that the dataset may not be stationary
# with roots (1-0.99B) very near the unit circle.
# The wandering behavior and fairly slowly damping sample autocorrelations
# The overfit tables with p=8 and p=12 (not shown) suggest the possibility of 
# a single unit root of +1
# The Dickey-Fuller test of Ho: the model has a unit root, is not
# rejected (p-value=.5611)

data(hadley)
mean(hadley) # -0.1684937
plotts.sample.wge(hadley)

est.ar.wge(hadley,p=8,type = 'burg')
    # Coefficients of Original polynomial:  
    #   0.6355 -0.0554 0.0794 0.2159 -0.0689 0.0894 0.0033 0.0637 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9849B              1.0153               0.9849       0.0000
    # 1+0.7467B             -1.3393               0.7467       0.5000
    # 1-0.0679B+0.5216B^2    0.0651+-1.3831i      0.7222       0.2425
    # 1-1.0019B+0.4801B^2    1.0434+-0.9971i      0.6929       0.1214
    # 1+0.6725B+0.3458B^2   -0.9724+-1.3951i      0.5881       0.3469
    # $phi
    # [1]  0.635541409 -0.055405056  0.079395896  0.215941758 -0.068881102  0.089411988
    # [7]  0.003283417  0.063685192

#Dickey Fuller test
library(tseries)
adf.test(hadley)
    # Augmented Dickey-Fuller Test
    # data:  hadley
    # Dickey-Fuller = -2.0365, Lag order = 5, p-value = 0.5611
    # alternative hypothesis: stationary

# Fit a non-stationary (1-B) transformation
# Diffference the Data
d1.temp=artrans.wge(hadley,phi.tr=1)
plotts.sample.wge(d1.temp,arlimits=TRUE)

# Model Stationary
aic5.wge(d1.temp,p=0:6,q=0:1)
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 6     2    1  -4.497695
    # 4     1    1  -4.487947
    # 8     3    1  -4.485866
    # 7     3    0  -4.484722
    # 10    4    1  -4.480622
# Chose an ARMA(2,1)

d1.temp.est = est.arma.wge(d1.temp,p=2,q=1)
    # Coefficients of Original polynomial:  
    #   0.3274 -0.1787 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.3274B+0.1787B^2    0.9162+-2.1811i      0.4227       0.1867
d1.temp.est$phi
    # [1]  0.3274341 -0.1786827
d1.temp.est$theta
    # [1] 0.704618
d1.temp.est$avar
    # [1] 0.01058826

# FINAL Fitted ARIMA(2,1,1) Model
# (1-B)(1-0.33B+0.18B^2)(X_t + 0.17) = (1- 0.70B)a_t  sigma_ahat^2 = 0.0106

# Check for White Noise
plotts.sample.wge(d1.temp.est$res,arlimits = TRUE)
ljung.wge(d1.temp.est$res,p=2,q=1,K=24)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 24
    # $chi.square
    # [1] 19.20029
    # $df
    # [1] 21
    # $pval
    # [1] 0.5722933
ljung.wge(d1.temp.est$res,p=2,q=1,K=48)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 48
    # $chi.square
    # [1] 44.86728
    # $df
    # [1] 45
    # $pval
    # [1] 0.4775275
# At 95% confidence (alpha=0.05), We fail to reject the null hypothesis of residual data
# is white noise (b=0), K=24; p-value 0.572    K=48; p-value 0.477
###############
# Based on Checks 1 and 2 the residuals from the fitted seasonal
# model appears to be white noise.
##############


# Generated realizations of ARIMA(2,1,1); to see if realizations are indicative of the 
# Temperture data

temp.real = gen.arima.wge(n=150,phi = d1.temp.est$phi,theta = d1.temp.est$theta,d=1,vara = d1.temp.est$avar)


#Forecasts using stationary model
data(hadley)
fore.arma.wge(hadley,phi=c(1.27,-.47,.19),
              theta=.63,n.ahead=50,limits=FALSE)


#Forecasts using nonstationary model
fore.aruma.wge(hadley,d=1,phi=c(.33,-.18),
               theta=.7,n.ahead=50,limits=FALSE)


#----Section 11.5.1 Global Temperature Data ---- Slide 65 - 74
# Modeling Global Temperature Data ( Signal Plus Noise Model)
#  X_t = a + bt + Z_t
#  Testing: Ho : b = 0 vs Ha : b <> 0

x=hadley
n=length(x)
t=1:n
# Fitting Model
d=lm(x~t)
summary(d)
    # Coefficients:
    #               Estimate    Std. Error    t value   Pr(>|t|)    
    # (Intercept)   -0.5257370  0.0251087     -20.94    <2e-16 ***
    #   t            0.0044378  0.0002705     16.40     <2e-16 ***
# Residuals
x.z=x-d$coefficients[1]-d$coefficients[2]*t
#x.z are the residuals from the regression line
plotts.wge(hadley)
plotts.wge(x.z)


#Signal-plus-noise model fit to temperature data:
#  using Cochrane-Orcutt

# Step 2: Fit AR(p) model phi_zhat(B) to the residuals and find
# Y_t = phi_zhat(B)X_t   (Y_t = y.trans in the code below)
ar.z=aic.wge(x.z,p=0:6)
    # $p
    # [1] 4
    # $q
    # [1] 0
    # $phi
    # [1]  0.61395779 -0.04395915  0.07771946  0.20634156
    # $theta
    # [1] 0
    # $vara
    # [1] 0.01033484
ar.z$p #is the order p (aic selects p = 4 here) - this is on the residuals
ar.z$phi #is vector of ar.z$p (4) estimated AR coefficients

# Transform the original data(response), using phi's found from the residuals
y.trans=artrans.wge(hadley,phi.tr=ar.z$phi)


#Step 3: Transform the independent variable (time)
# T_that = phi_zhat(B)T_t,   T_1 = 1, T_2 = 2, ....    T_that = t.trans
#ar.z$phi is vector of ar.z$p estimated AR coefficients
t.trans=artrans.wge(t,phi.tr=ar.z$phi)

# Step 4:  Regress Y_that on T_that using OLS
fitco = lm(y.trans~t.trans)
summary(fitco)
    # Coefficients:
    #               Estimate    Std. Error  t value   Pr(>|t|)    
    # (Intercept)   -0.094089   0.018883    -4.983    1.67e-06 ***
    #   t.trans      0.005700   0.001248    4.566     1.01e-05 ***
# After accounting for the serial correlation (AR(4)), there is strong evidence to
# suggest that the slope is significantly different from zero (pvalue < .0001).

# Evaluate the residuals (after Cochrane-Orcutt)
plotts.wge(fitco$residuals)
acf(fitco$residuals)
ljung.wge(fitco$residuals, K=24)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 24
    # $chi.square
    # [1] 17.95885
    # $df
    # [1] 24
    # $pval
    # [1] 0.8049998
ljung.wge(fitco$residuals, K=48)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 48
    # $chi.square
    # [1] 45.49179
    # $df
    # [1] 48
    # $pval
    # [1] 0.5762013

# Sample autocorrelations tend to stay within limit lines.
# At 95% confidence (alpha=0.05), We fail to reject the null hypothesis of residual data
# is white noise (b=0), K=24; p-value 0.804    K=48; p-value 0.576

# Final Model:
# X_t = -0.5257 + 0.0044t + Z_t  where sigma_ahat^2 = 0.0103, and Z_t is AR(4)
#      (Values come from hadley = a + bt (summary(d)))

#(1- 0.614B + 0.044B^2 - 0.078B^3 - 0.026B^4)Z_t = a_t
#       (values come from ar.z residuals of Hadley)

# Note 1: The above is the signal-plus-noise fit to the temperature
# data. Cochrane-Orcutt is a procedure to assess the significance of the
# slope (adjusting for the correlated errors).
# Note 2: We had to code the Cochrane-Orcutt Procedure manually
# since the function cochrane.orcutt() is only for AR(1) correlation.


# Generate some realizations from this model
gen.sigplusnoise.wge(160, b0 = -.5257, b1 = .0044, phi = ar.z$phi, vara = .0103)

#Realizations have the appearance of temperature data
#All tend to increase because of the line with positive slope

# Forecast some models
fore.sigplusnoise.wge(hadley,max.p=4,n.ahead
                      =50,limits=FALSE)


#----Section 11.6.1,2 Sunspot Data-Model Appropriateness ---- Slide 89 - 91
# Box-Jenkins Model
data("sunspot.classic")
#Plot the data
plotts.sample.wge(sunspot.classic)
dev.off()
# Box-Jenkins Model
# Plot ACF and PACF to look for patterns
acf(sunspot.classic)
pacf(sunspot.classic)
#The two large partial autocorrelations strongly suggest an AR(2)

#Estimate AR(2) parameters
s2 = est.ar.wge(sunspot.classic,p=2)
    # Coefficients of Original polynomial:  
    #   1.3347 -0.6474 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.3347B+0.6474B^2    1.0308+-0.6943i      0.8046       0.0943
s2$phi
    # [1]  1.3347282 -0.6474423
s2$avar
    # [1] 235.993
mean(sunspot.classic)
    #[1] 44.78409

# Final Model
# (1 - 0.33B + 0.65B^2)(X_t - 44.78) = a_t where sigma_ahat^2 = 236

plotts.sample.wge(s2$res,arlimits = TRUE)
ljung.wge(s2$res,K=24)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 24
    # $chi.square
    # [1] 26.93038
    # $df
    # [1] 24
    # $pval
    # [1] 0.307709
ljung.wge(s2$res,K=48)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 48
    # $chi.square
    # [1] 62.83871
    # $df
    # [1] 48
    # $pval
    # [1] 0.07378717
# At 95% confidence (alpha=0.05), We fail to reject the null hypothesis of residual data
# is white noise (b=0), K=24; p-value 0.308    K=48; p-value 0.074
###############
# Based on Checks 1 and 2 the residuals from the model appears to be white noise.
##############


#----Section 11.6.4 Sunspot Data-Model Appropriateness ---- Slide 93 - 96
# AIC Model
# Plot Realization
plotts.sample.wge(sunspot.classic)
dev.off()
aic5.wge(sunspot.classic,p=0:10,q=0:0)
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 9     8    0   5.461687
    # 10    9    0   5.468713
    # 11   10    0   5.475731
    # 8     7    0   5.494449
    # 4     3    0   5.495997
# AIC picks an AR(8)
# FYI BIC selects an AR(2)

s8=est.ar.wge(sunspot.classic,p=8)
# s8$phi: 1.22872595 -0.47331327 -0.13807811 0.15688938 -0.14030802 0.07050449 -0.12841889 0.20692558
# s8$avar:212.6003
mean(sunspot.classic) # 44.78409

#AIC AR(8) Model
#(1- 1,23B + 0.47B^2 + 0.14B^3 - 0.16B^4 + 0.14B^5 - 0.07B^6 + 0.13B^7 - 0.21B^8)(X_t - 44.78) = a_t
# sigma_ahat^2 = 212.6

factor.wge(phi=s8$phi)
    # Coefficients of Original polynomial:  
    #   1.2287 -0.4733 -0.1381 0.1569 -0.1403 0.0705 -0.1284 0.2069 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.5565B+0.8970B^2    0.8676+-0.6017i      0.9471       0.0965
    # 1-0.8771B              1.1402               0.8771       0.0000
    # 1-0.4147B+0.6550B^2    0.3166+-1.1944i      0.8093       0.2088
    # 1+0.7964B             -1.2557               0.7964       0.5000
    # 1+0.8231B+0.5043B^2   -0.8161+-1.1476i      0.7101       0.3484

# Look at residuals of fit
plotts.sample.wge(s8$res, arlimits = TRUE)

ljung.wge(s8$res,K=24)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 24
    # $chi.square
    # [1] 17.9297
    # $df
    # [1] 24
    # $pval
    # [1] 0.8064054
ljung.wge(s8$res,K=48)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 48
    # $chi.square
    # [1] 45.8185
    # $df
    # [1] 48
    # $pval
    # [1] 0.5626712


# Again, the residuals look reasonably white
# Sample autocorrelation of the residuals stay within the 95% limit lines
# Ljung-Box did not reject the null of white noise at K=24 or at K=48
# with p-values of .81 and .56, respectively


#----Section 11.7.1,2 Sunspot Data-Model Comparing Realizations ---- Slide 98 - 101

s2.1= gen.arma.wge(n=150,phi = s2$phi,vara = s2$avar)
plotts.sample.wge(s2.1)

s3.real[3]
# Compare ACFs
acf(sunspot.classic)
#AR(2) model

factor.wge(phi = s2$phi)

for (i in 1:5) {
  assign(paste0("Variable", i), gen.arma.wge(n=150,phi = s2$phi,vara = s2$avar))
} 
Variable1



