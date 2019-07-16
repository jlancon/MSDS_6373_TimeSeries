#  MSDS 6373- Time Series Analysis - Unit 10 Homework Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 07/08/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 7)


library(tswge)

# Select a data set, and complete one of each model ID below:
#   
# ARIMA
# Seasonal
# Signal Plus Noise with Cochrane-Orcutt
# Take your time series, and analyze it with these methods
#
# Chose freeze dataset from tswge

data("freeze")
frz = freeze
plotts.sample.wge(frz)
# Since the data is recorded 10 days, a period of 365/10 (36.5) would
# be expected. Looking at spectral density confirms this observations 
# with a peak at frequency approx. 0.03.
# Data appears stationary with positive, slowly dampening autocorrelation
# with a possible sinusodal pattern.

# First overfit model to identify any seasonality and/or non-stationarity
frz.s =est.ar.wge(frz,p=15,type = 'burg')
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.9549B+0.9858B^2    0.9915+-0.1769i      0.9929       0.0281
    # 1-1.4607B+0.8547B^2    0.8545+-0.6632i      0.9245       0.1050
    # 1-0.8439B+0.7887B^2    0.5350+-0.9908i      0.8881       0.1712
    # 1-0.0263B+0.7679B^2    0.0171+-1.1410i      0.8763       0.2476
    # 1+1.1717B+0.7124B^2   -0.8223+-0.8529i      0.8441       0.3721
    # 1+0.5794B+0.7071B^2   -0.4097+-1.1164i      0.8409       0.3060
    # 1+1.5313B+0.6665B^2   -1.1487+-0.4251i      0.8164       0.4436
    # 1+0.8105B             -1.2338               0.8105       0.5000
# Looking at the overfit factors, it appears that we may have 
# non-stationarity. 1-1.9549B+0.9858B^2 can be roughly factored
# to (1-B)^2. We will take 2nd difference and fit the differenced
# data

# Fit and ARIMA model to dataset
# Take the first difference (1-B)
frz.d1 = artrans.wge(x=frz, phi.tr = c(1))
plotts.sample.wge(frz.d1)
# Take the second difference (1-B)
frz.d2 = artrans.wge(x=frz.d1, phi.tr = c(1))
plotts.sample.wge(frz.d2)

# Use aic5 to determine model of 2nd diff data-- 'BIC'
aic5.wge(frz.d2,p=0:15,q=0:2, type = 'bic')
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 3     0    2   2.596612
    # 25    8    0   2.660657
    # 22    7    0   2.665951
    # 24    7    2   2.669708
    # 28    9    0   2.670815
# Use aic5 to determine model of 2nd diff data-- 'AIC'
aic5.wge(frz.d2,p=0:20,q=0:2, type = 'aic')
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 35   11    1   2.565002
    # 3     0    2   2.571247
    # 25    8    0   2.584562
    # 24    7    2   2.585158
    # 47   15    1   2.586167

# AIC is suggesting either very complicated models (p>7) or
# simple models ARMA(0,2).  ARMA(0,2) was chosen by both metrics
# Looking at the sample ACF, the simplier models match the 
# data more closely.
acf(frz.d2)

est.arma.wge(frz.d2,p=0,q=2)
    # $phi
    # [1] 0
    # $theta
    # [1]  1.451106 -0.571752
    # $avar
    # [1] 12.92545
mean(frz)
    # [1] 4.6246
frz.pred = fore.aruma.wge(frz,s=0,d=2,phi = c(0),theta=c(1.451,-0.571),
                        n.ahead = 36,lastn = TRUE,limits = FALSE)
n = 36
ASE = mean((frz[(length(frz)-n+1):(length(frz))]-frz.pred$f)^2)
print(paste0('ASE: ',round(ASE,3)))
    # [1] "ASE: 381.578"
# Model is not ideal for forecasting since the terms in the model are
# (1-B)^2, which predicts the trend of the last 2 values in the dataset
# versus matching the overall pattern of the dataset.

##--------------- Fit a seasonal Model ------------##
plotts.sample.wge(frz)
frz.s =est.ar.wge(frz,p=15,type = 'burg')
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.9549B+0.9858B^2    0.9915+-0.1769i      0.9929       0.0281
    # 1-1.4607B+0.8547B^2    0.8545+-0.6632i      0.9245       0.1050
    # 1-0.8439B+0.7887B^2    0.5350+-0.9908i      0.8881       0.1712
    # 1-0.0263B+0.7679B^2    0.0171+-1.1410i      0.8763       0.2476
    # 1+1.1717B+0.7124B^2   -0.8223+-0.8529i      0.8441       0.3721
    # 1+0.5794B+0.7071B^2   -0.4097+-1.1164i      0.8409       0.3060
    # 1+1.5313B+0.6665B^2   -1.1487+-0.4251i      0.8164       0.4436
    # 1+0.8105B             -1.2338               0.8105       0.5000

# After investigating the factor tables, it appears that there is no
# seasonal trend in the data that correlates

# ---- Fit a model with trend, using Cochrane-Orcutt
t = seq(1,length(frz),1)
df = data.frame(x = frz, t= t)
fit = lm(x~t,data = df)
summary(fit)

library(orcutt)
# Using the Cochrane-Orcutt method
cfit = cochrane.orcutt(fit)
summary(cfit)
    #               Estimate    Std. Error  t value  Pr(>|t|)    
    # (Intercept)   4.2678368   1.2396481   3.443   0.0006244 ***
    #   t           0.0013584   0.0042488   0.320   0.7493090 

# At 95% confidence interval, we fail to reject the 
# null hypothesis of b=0 (p.value = 0.749).


