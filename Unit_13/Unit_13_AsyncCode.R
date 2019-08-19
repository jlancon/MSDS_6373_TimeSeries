#  MSDS 6373- Time Series Analysis - Unit 13 Async Code 
# Neural Networks

#  Team Member:  Jeffery Lancon
#  
#  Date: 07/29/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 7)

library(nnfor)
library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf

#----Section 13.3.2 ---- Slide 19 - 24
# Multiple Regression with Correlated Errors
# Dataset: swadelay.csv
SWA = read.csv(file.choose(),header=TRUE)
# Divide dataset into training and test set. The last
# 36 months in the test set.
SWATrain = ts(SWA$arr_delay[1:141],start = c(2004,1),frequency = 12)
SWATest = ts(SWA$arr_delay[142:177],start = c(2015,10),frequency = 12)
set.seed(2)
fit.mlp= mlp(SWATrain, reps=50,comb='mean')
fit.mlp
    # MLP fit with 5 hidden nodes and 50 repetitions.
    # Series modelled in differences: D1.
    # Univariate lags: (1,2,3,4,5)
    # Deterministic seasonal dummies included.
    # Forecast combined using the mean operator.
    # MSE: 5043757.8409

# Visualize the Neural Network
# Note: Pink nodes are for seasonal dummies
# and grey are for lagged inputs
plot(fit.mlp)

# Create a forecast for 36 segments ahead
fore.mlp = forecast(fit.mlp,h=36)
plot(fore.mlp)

# Forecasted Model grading using ASE
ASE = mean((SWATest-fore.mlp$mean)^2)
ASE
    # [1] 317604252

##### Fitting model with specified lags #####
set.seed(2)
fit.mlp.l = mlp(SWATrain,lags = c(seq(1,12,1)),
                                  allow.det.season=FALSE)
fit.mlp.l
    # MLP fit with 5 hidden nodes and 20 repetitions.
    # Series modelled in differences: D1.
    # Univariate lags: (1,2,3,4,5,6,7,8,9,11,12)
    # Forecast combined using the median operator.
    # MSE: 5598324.2681.                

# Visualize the Neural Network
plot(fit.mlp.l)

# Create a forecast for 36 segments ahead
fore.mlp.l = forecast(fit.mlp.l,h=36)
plot(fore.mlp.l)

# Forecasted Model grading using ASE
ASE = mean((SWATest-fore.mlp.l$mean)^2)
ASE
    # [1] 456929879

##### Fitting model with specified diff order #####

set.seed(2)
fit.mlp.d = mlp(SWATrain,difforder = c(12),
                allow.det.season=FALSE)
fit.mlp.d
    # MLP fit with 5 hidden nodes and 20 repetitions.
    # Series modelled in differences: D12.
    # Univariate lags: (1,2,3,6,9,12)
    # Forecast combined using the median operator.
    # MSE: 32068872.9302.                

# Visualize the Neural Network
plot(fit.mlp.d)

# Create a forecast for 36 segments ahead
fore.mlp.d = forecast(fit.mlp.d,h=36)
plot(fore.mlp.d)

# Forecasted Model grading using ASE
ASE = mean((SWATest-fore.mlp.d$mean)^2)
ASE
    # [1] 221327082


#### Previous AR(12) model on SWA dataset
# Model (1 - 0.44B - 0.02B^2 + 0.12B^3 - 0.08B^4 + 0.00B^5
# - 0.02B^6 - 0.06B^7 + 0.09B^8 - 0.06B^9 - 0.07B^10 
# - 0.02B^11 - 0.37B^12)(X_t - 34934) = a_t
set.seed(2)
f = fore.arma.wge(SWA$arr_delay,phi = c(0.44,0.02,-0.12,0.08,
                                        0.00,0.02,0.06,-0.09,
                                        0.06,0.07,0.02,0.37),
                                      n.ahead = 30, lastn = TRUE,
                                      limits = FALSE)
ASE = mean((SWA$arr_delay[148:177]-f$f)^2)
ASE
    # [1] 309880104


#### Seasonal Model with AR(p) model on SWA dataset
# Fit a seasonal s=12 and use an AR(13) on difference
# data
#Factor Table suggested s = 12
SWA.12 = artrans.wge(SWA$arr_delay[1:141],c(rep(0,11),1))
p = aic5.wge(SWA.12,p = 0:18, q = 0:2)
#AIC picks p = 13 q = 0
AR13 = est.arma.wge(SWA.12,p = 13)
f.S12.AR13 = fore.aruma.wge(SWA$arr_delay,phi = AR13$phi,
                            s = 12,n.ahead = 36, lastn = TRUE)
ASE = mean((SWA$arr_delay[142:177] - f.S12.AR13$f)^2)
ASE
    # [1] 215312956


### Concept Check 13.3.3,4,5,6
#Inspect the spectral density estimate of the 
#Southwest Airlines arrival delay data from the previous 
#example.
# There appears to be some evidence of a peak at 0, around
# .08, and around .16. This would correspond to wandering
# behavior, annual period, and biannual behavior.  

# Which R call below fits 100 neural networks that includes 
# a (1-B), (1-B6), and (1-B12) seasonal factor and also allows 
# for AR fitting of the residuals after the differencing



##--- Concept Check 13.3.4 ---
# Given that the R code below will fit 100 neural networks that 
# include a (1-B), (1-B6), and (1-B12) seasonal factor and also 
# allows for AR fitting of the residuals after the differencing, 
# use this code to obtain forecasts of the number of delays in the
# last 36 months (from October 2015, to September 2018).
set.seed(5)
fit.mlp.s1.6.12 = mlp(SWATrain, difforder = c(1,6,12), allow.det.season = FALSE, reps = 100)
fit.mlp.s1.6.12
    # MLP fit with 5 hidden nodes and 100 repetitions.
    # Series modelled in differences: D1D6D12.
    # Univariate lags: (1,2,3,4,6,12)
    # Forecast combined using the median operator.
    # MSE: 41387780.5526.

# Create a forecast for 36 segments ahead
fore.mlp.s1.6.12 = forecast(fit.mlp.s1.6.12,h=36)
plot(fore.mlp.s1.6.12)

# Forecasted Model grading using ASE
ASE = mean((SWATest-fore.mlp.s1.6.12$mean)^2)
ASE
    # [1] 845232182





#----Section 13.4.1 ---- Slide 30 - 32
# Neural Networks - Airline Data
# Dataset: swadelay.csv

# First 108 months in the Training Set.
data("airlog")
set.seed(2)
lairTrain = ts(airlog[1:108], frequency = 12, start = c(1949, 1))
# Last 36 months in the Test set.
lairTest = ts(airlog[109:144], frequency = 12, start = c(1958, 1))
fit.mlp = mlp(lairTrain)
fit.mlp
    # MLP fit with 5 hidden nodes and 20 repetitions.
    # Series modelled in differences: D1.
    # Univariate lags: (1,3,4,7,8)
    # Deterministic seasonal dummies included.
    # Forecast combined using the median operator.
    # MSE: 1e-04.

# Visualize the Neural Network
plot(fit.mlp)

# Create a forecast for 36 segments ahead
fore.mlp = forecast(fit.mlp, h = 36)
plot(fore.mlp)

ASE = mean((lairTest-fore.mlp$mean)^2)
ASE
    # [1] 0.01826498
set.seed(2)
data("airlog")
lairTrain = ts(airlog[1:108], frequency = 12, start = c(1949, 1))
# Last 36 months in the Test set.
lairTest = ts(airlog[109:144], frequency = 12, start = c(1958, 1))
fit.mlp = mlp(lairTrain,hd.auto.type = "cv")
fit.mlp
plot(fit.mlp)
fore.mlp = forecast(fit.mlp, h = 36)
ASE = mean((lairTest-fore.mlp$mean)^2)
ASE


?mlp()
# mlp(y, m = frequency(y), hd = NULL, reps = 20, comb = c("median",
#                                                         "mean", "mode"), lags = NULL, keep = NULL, difforder = NULL,
#     outplot = c(FALSE, TRUE), sel.lag = c(TRUE, FALSE),
#     allow.det.season = c(TRUE, FALSE), det.type = c("auto", "bin",
#                                                     "trg"), xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
#     hd.auto.type = c("set", "valid", "cv", "elm"), hd.max = NULL,
#     model = NULL, retrain = c(FALSE, TRUE), ...)


#BS is the Business data
# Only Time as a regressor
BS = read.csv(file.choose(),header=TRUE)
tBS80 = ts(BS$sales[1:80])
set.seed(2)
fit3 = mlp(tBS80)
    # MLP fit with 5 hidden nodes and 20 repetitions.
    # Univariate lags: (1,2,4)
    # Forecast combined using the median operator.
    # MSE: 1.311.
f = forecast(fit3, h = 20)
plot(BS$sales[81:100],type = "l")
lines(seq(1,20),f$mean, col = "blue")
ASE = mean((BS$sales[81:100]-f$mean)^2)
ASE
    # [1] 11.46737

#With additional Regressors
set.seed(2)
tBS80 = ts(BS$sales[1:80])
tBSx = data.frame(ad_tv = ts(BS$ad_tv), ad_online = ts(BS$ad_online, frequency = 7),discount = ts(BS$discount))
fit3 = mlp(tBS80,xreg = tBSx)
    # MLP fit with 5 hidden nodes and 20 repetitions.
    # Univariate lags: (1,2,3)
    # 3 regressors included.
    # - Regressor 1 lags: (1)
    # - Regressor 2 lags: (1,2)
    # - Regressor 3 lags: (2,3,4)
    # Forecast combined using the median operator.
    # MSE: 0.2157.

f = forecast(fit3, h = 20, xreg = tBSx)
plot(BS$sales[81:100],type = "l")
lines(seq(1,20),f$mean, col = "blue")
ASE = mean((BS$sales[81:100]-f$mean)^2)
ASE
    # [1] 1.004042



#ARIMA Model with Regressors
ad_tv1 = c(NA,BS$ad_tv[1:(length(BS$ad_tv)-1)])
ad_online1 = c(NA,BS$ad_online[1:(length(BS$ad_online)-1)])
BS$ad_tv1= ad_tv1
BS$ad_online1 = ad_online1
ksfit=lm(sales~ad_tv1+ad_online1+discount, data = BS)
aic.wge(ksfit$residuals,p=0:8,q=0:0) # AIC picks p=7
fit=arima(BS$sales,order=c(7,0,0),xreg=cbind(BS$ad_tv1, BS$ad_online1,
                                             BS$discount))
preds = forecast(fit,h = 20,xreg = cbind(BS$ad_tv1[81:100],BS$ad_online1[81:100],BS$discount[81:100]))
plot(BS$sales[81:100],type = "l")
lines(seq(1,20),preds$mean, col = "blue")
ASE = mean((BS$sales[81:100]-preds$mean)^2)
ASE



#NN With additional Regressors
set.seed(2)
tBS80 = ts(BS$sales[1:80])
tBSx = data.frame(ad_tv = ts(BS$ad_tv), ad_online = ts(BS$ad_online,
                                                       frequency = 7),discount = ts(BS$discount))
fit3 = mlp(tBS80,xreg = tBSx)
f = forecast(fit3, h = 20, xreg = tBSx)
plot(BS$sales[81:100],type = "l")
lines(seq(1,20),f$mean, col = "blue")
ASE = mean((BS$sales[81:100]-f$mean)^2)
ASE
    # [1] 1.004042

#NN With additional Regressors
set.seed(2)
tBS80 = ts(BS$sales[1:80])
tBSx = data.frame(ad_tv = ts(BS$ad_tv), ad_online = ts(BS$ad_online,
                                                       frequency = 7),discount = ts(BS$discount))
fit3 = mlp(tBS80,xreg = tBSx,hd.auto.type = 'cv')
f = forecast(fit3, h = 20, xreg = tBSx)
plot(BS$sales[81:100],type = "l")
lines(seq(1,20),f$mean, col = "blue")
ASE = mean((BS$sales[81:100]-f$mean)^2)
ASE
# [1] 0.757798



?forecast()
par(mfrow=c(2,2))
for(i in 2:length(BSales)){ 
  heading = paste(" ",colnames(BSales[i])) 
  plot(BSales$X, BSales[,i], type="l",main=heading,
       xlab="Weeks", ylab=paste(colnames(BSales[i]),'x1000')) 
}
dev.off()

ksfit=lm(sales~ad_tv+ad_online+discount, data = BSales)
    # Coefficients:
    #   (Intercept)        ad_tv    ad_online     discount  
    #     20.1738       4.6762       2.3237      -0.1033  
plotts.sample.wge(ksfit$residuals,arlimits = TRUE)

aic.wge(ksfit$residuals,p=0:8, q=0) # AIC picks p=7
    # $type
    # [1] "aic"
    # $value
    # [1] 1.101655
    # $p
    # [1] 7
    # $q
    # [1] 0
    # $phi
    # [1]   0.3914830668  0.1263257053 -0.3764190275 -0.0005409533  
    #       0.1006855558  0.2148095958  0.1476114131
    # $theta
    # [1] 0
    # $vara
    # [1] 2.564221

fit=arima(BSales$sales,order=c(7,0,0),xreg=BSales[,3:5])
fit
    # Coefficients:
    #       ar1      ar2     ar3     ar4     ar5     ar6      ar7     intercept
    #       1.4734  -0.8921  0.0749  0.0919  0.0438  0.1865  -0.1287    54.5513
    # s.e.  0.1107   0.1963  0.2167  0.2057  0.2049  0.1852   0.1080     2.2040
    #       ad_tv  ad_online  discount
    #       0.0703    -0.0934   -0.1514
    # s.e.  0.3434     0.2075    0.1315
# Note: ad_tv, ad_online, and discount are not much different than zero
# since the se is large and coeff are small (approximating a low t-statistic)

acf(fit$residuals)
plotts.sample.wge(fit$residuals,arlimits = TRUE)

ltest = ljung.wge(fit$residuals)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 24
    # $chi.square
    # [1] 66.44817
    # $df
    # [1] 24
    # $pval
    # [1] 7.445469e-06
# At 95% confidence (alpha=0.05), We reject the null hypothesis of residual data
# is white noise (b=0), K=24; p-value 7.45e-06
# Not a good model for this dataset

########################################
# Adding a time trend to the model (adding variable t to model)#
t=1:100 #we cound us BSales$X as well
ksfit2=lm(sales~t+ad_tv+ad_online+discount, data = BSales)
    # Coefficients:
    #   (Intercept)            t        ad_tv    ad_online     discount  
    #     22.10728      0.02603      4.35596      1.90313     -0.05664 

plotts.sample.wge(ksfit2$residuals,arlimits = TRUE)

aic.wge(ksfit2$residuals,p=0:8, q=0) # AIC picks p=6
    # $type
    # [1] "aic"
    # $value
    # [1] 0.9835036
    # $p
    # [1] 6
    # $q
    # [1] 0
    # $phi
    # [1]  0.44043768  0.06888471 -0.42345549 -0.05290844  0.08724326  0.21222750
    # $theta
    # [1] 0
    # $vara
    # [1] 2.324497

fit2=arima(BSales$sales,order=c(6,0,0),xreg=cbind(t,BSales[,3:5]))
fit2
    # Coefficients:
    #         ar1      ar2     ar3     ar4     ar5      ar6  intercept
    #       1.4090  -0.8778  0.0180  0.0562  0.1676  -0.0364    51.9224
    # s.e.  0.1131   0.1978  0.2182  0.2057  0.1785   0.1050     2.2242
    #         t     ad_tv     ad_online  discount
    #       0.0465  0.1123    -0.0508   -0.1701
    # s.e.  0.0148  0.3549     0.1939    0.1052
    # 
    # sigma^2 estimated as 1.363:  log likelihood = -159.14,  aic = 342.29

ljung.wge(fit2$residuals)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 24
    # $chi.square
    # [1] 64.06313
    # $df
    # [1] 24
    # $pval
    # [1] 1.66855e-05
# At 95% confidence (alpha=0.05), We reject the null hypothesis that the
# residual data is white noise (b=0), K=24; p-value 1.67e-05. There is 
# strong evidence that the residuals are serially correlated.
# Not a good model for this dataset

###############################
#----Section 12.2.4 ---- Slide 18 - 20
# Multiple Regression with Lagged Variables

#Example:
#With dplyr lag function
df = data.frame(Y = c(1,1,2,3,4,4,5,8),X1 = c(5,6,6,7,7,8,8,9))
df$X1_L1 = dplyr::lag(df$X1,1)
df$X1_L2 = dplyr::lag(df$X1,2)
df

# Creating lag in R base
df = data.frame(Y = c(1,1,2,3,4,4,5,8),X1 = c(5,6,6,7,7,8,8,9))
X1_L1 = numeric(length(df$X1))
X1_L1 = c(NA,df$X1[1:(length(df$X1)-1)]) #X1 lagged once
X1_L2 = c(NA,NA,df$X1[1:(length(df$X1)-2)]) #X1 lagged twice
df$X1_L1=X1_L1
df$X1_L2=X1_L2
df
#####################






head(BSales)
###############################
#----Section 12.2.5 ---- Slide 22 - 28
# Multiple Regression with Lagged Variables
# Dataset: businesssales.csv

ad_tv1 = dplyr::lag(BSales$ad_tv,1)
ad_online1 = dplyr::lag(BSales$ad_online,1)
discount = BSales$discount
BSales$ad_tv1= ad_tv1
BSales$ad_online1 = ad_online1
ksfit=lm(sales~ad_tv1+ad_online1+discount, data = BSales)
  # Coefficients:
  #   (Intercept)       ad_tv1   ad_online1     discount  
  #     10.08872      5.42230      3.88479     -0.07733 
aic.wge(ksfit$residuals,p=0:8,q=0:0) # AIC picks p=7
    # $type
    # [1] "aic"
    # $value
    # [1] 0.7481867
    # $p
    # [1] 7
    # $q
    # [1] 0
    # $phi
    # [1] -0.06461833  0.19212542 -0.08348717 -0.01488629 -0.13475021
    # [6]  0.23864863  0.18607832
    # $theta
    # [1] 0
    # $vara
    # [1] 1.79781

fit=arima(BSales$sales,order=c(7,0,0),xreg=cbind(ad_tv1, ad_online1, discount))
fit
    # Coefficients:
    #           ar1     ar2      ar3      ar4      ar5      ar6     ar7
    #         -0.5197  0.1348  -0.0787  -0.1155  -0.1954  -0.0004  0.0485
    # s.e.   0.1839  0.1689   0.1233   0.1256   0.1309   0.1884  0.1574
    #         intercept  ad_tv1  ad_online1  discount
    #           4.8382  3.4341      8.1152   -0.0573
    # s.e.     2.8270  0.6166      1.2447    0.0281
    # 
    # sigma^2 estimated as 1.642:  log likelihood = -165.43,  aic = 354.86

#####################
# NOW going to add time as a variable with lagged variables
t=1:100
ad_tv1 = dplyr::lag(BSales$ad_tv,1)
ad_online1 = dplyr::lag(BSales$ad_online,1)
BSales$ad_tv1= ad_tv1
BSales$ad_online1 = ad_online1
ksfit=lm(sales~t + ad_tv1+ad_online1+discount, data = BSales)
    # Coefficients:
    #   (Intercept)            t       ad_tv1   ad_online1     discount  
    #     11.20096      0.01270      5.18679      3.74697     -0.05338
aic.wge(ksfit$residuals,p=0:8,q=0:0) # AIC picks p=7
    # $type
    # [1] "aic"
    # $value
    # [1] 0.7031507
    # $p
    # [1] 7
    # $q
    # [1] 0
    # $phi
    # [1] -0.05644526  0.15653543 -0.12479280 -0.05005342 -0.14801128
    # [6]  0.23402135  0.16170821
    # $theta
    # [1] 0
    # $vara
    # [1] 1.718642


fit=arima(BSales$sales,order=c(7,0,0),xreg=cbind(t, ad_tv1, ad_online1, discount))
fit
    # Coefficients:
    #         ar1     ar2      ar3      ar4      ar5     ar6     ar7
    #       -0.5062  0.1265  -0.1053  -0.1388  -0.1985  0.0109  0.0493
    # s.e.   0.1883  0.1537   0.1138   0.1160   0.1206  0.1805  0.1535
    #         intercept       t   ad_tv1  ad_online1  discount
    #           6.2215    0.0065  3.3180      7.8248   -0.0453
    # s.e.     2.7820     0.0038  0.6288      1.3020    0.0276
    # 
    # sigma^2 estimated as 1.577:  log likelihood = -163.43,  aic = 352.87
ljung.wge(fit$residuals)
    # $test
    # [1] "Ljung-Box test"
    # $K
    # [1] 24
    # $chi.square
    # [1] 20.36429
    # $df
    # [1] 24
    # $pval
    # [1] 0.6758827


#----Section 12.3.4 Concept Check
# Consider the attached data set in WhatIsTheLag.csv. 
# Use the cross-correlation function (ccf()) to find evidence of 
# the lag in which X1 is related to Y. That is, which k has the 
# most evidence of X_t-k being related to Y_t?

WhatLag = read.csv(file.choose(),header=TRUE)
#?ccf()
dev.off()
ccf(WhatLag$Y,WhatLag$X1)



###############################
#----Section 12.4.3 ---- Slide 51 - 53
# Multivariate Time Series Forecast
x1.25=c( -1.03, 0.11, -0.18, 0.20, -0.99, -1.63, 1.07, 2.26, -0.49, -1.54, 0.45, 0.92,
         -0.05, -1.18, 0.90, 1.17, 0.31, 1.19, 0.27, -0.09, 0.23, -1.91, 0.46, 3.61, -0.03)
x2.25=c( -0.82, 0.54, 1.13, -0.24, -0.77, 0.22, 0.46, -0.03, -0.59, 0.45, 0.59, 0.15,
         0.60, 0.13, -0.04, 0.12, -0.96, 0.23, 1.81, -0.01, -0.95, -0.55, -0.15, 0.71, 0.90)
x1=x1.25[1:20]
x2=x2.25[1:20]
par(mfrow=c(1,2))
plotts.wge(x1)
plotts.wge(x2)

p1=aic.wge(x1,p=0:8,q=0:0)# aic picks p=2
    # $type[1] "aic"
    # $value[1] -0.2311079
    # $p[1] 2
    # $q[1] 0
    # $phi[1]  0.1768573 -0.5844679
    # $theta[1] 0
    # $vara[1] 0.5879532

x1.est=est.ar.wge(x1,p=p1$p)
    # Coefficients of Original polynomial:  
    #   0.1769 -0.5845 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.1769B+0.5845B^2    0.1513+-1.2993i      0.7645       0.2315

fore.arma.wge(x1,phi=x1.est$phi,n.ahead=5,lastn=FALSE,limits=FALSE)

# Now we will forecast x2 ###
p2=aic.wge(x2,p=0:8,q=0:0)# aic picks p=2
    # $type[1] "aic"
    # $value[1] -1.397817
    # $p[1] 2
    # $q[1] 0
    # $phi[1]  0.01319337 -0.81432047
    # $theta[1] 0
    # $vara[1] 0.1830828

x2.est=est.ar.wge(x1,p=p2$p)
    # Coefficients of Original polynomial:  
    #   0.1769 -0.5845 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.1769B+0.5845B^2    0.1513+-1.2993i      0.7645       0.2315

fore.arma.wge(x2,phi=x2.est$phi,n.ahead=5,lastn=FALSE,limits=FALSE)

######################################
# VAR and VARselect are from CRAN package vars
X=cbind(x1,x2)
library(vars)
VARselect(X, lag.max = 6, type = "const",season = NULL, exogen = NULL)
#VARselect picks p=5 (using AIC)
lsfit=VAR(X,p=5,type="const")
preds=predict(lsfit,n.ahead=5)
    # $x1
    # fcst       lower       upper         CI
    # [1,]  0.230860065  0.20606481  0.25565532 0.02479525
    # [2,] -1.905108807 -1.93006759 -1.88015002 0.02495879
    # [3,]  0.451961902  0.42556537  0.47835843 0.02639653
    # [4,]  3.612474098  3.58488506  3.64006313 0.02758904
    # [5,] -0.009114912 -0.03670624  0.01847642 0.02759133
    # $x2
    # fcst      lower      upper       CI
    # [1,]  0.6113475 -0.5217832 1.74447831 1.133131
    # [2,]  0.8242393 -0.3118112 1.96028976 1.136050
    # [3,] -1.4053274 -2.8895560 0.07890118 1.484229
    # [4,]  0.1388190 -1.3597092 1.63734721 1.498528
    # [5,] -0.2372017 -1.7759917 1.30158827 1.538790
preds$fcst$x1[1:5,1] #are the VAR forecasts for x1. Similar for x2
    # [1]  0.230860065 -1.905108807  0.451961902  3.612474098
    # [5] -0.009114912

summary(lsfit)

library(RColorBrewer)
fanchart(preds, colors = brewer.pal(n = 8, name = "Blues")) # Change color pallet to make distinguishable.



###############################
#----Section 12.5.1 ---- Slide 63 - 74
# Multivariate Time Series Forecast
# melanoma incidence and sunspot numbers 1936-1972
melanoma=c(1.0, 0.9, 0.8, 1.4, 1.2, 1.0, 1.5, 1.9, 1.5, 1.5, 1.5, 1.6, 1.8, 2.8, 2.5, 2.5, 2.4, 2.1, 1.9, 2.4, 2.4, 2.6, 2.6,
           4.4, 4.2, 3.8, 3.4, 3.6, 4.1, 3.7, 4.2, 4.1, 4.1, 4.0, 5.2, 5.3, 5.3)
sunspot=c(40, 115, 100, 80, 60, 40, 23, 10, 10, 25, 75, 145, 130, 130, 80, 65, 20, 10, 5, 10, 60, 190, 180, 175,
          120, 50, 35, 20, 10, 15, 30, 60, 105, 105, 105, 80, 65)
plotts.wge(melanoma)
plotts.wge(sunspot)
# Look at cross correlation function
ccf(sunspot,melanoma,ylim = c(-1,1))


mel.67=melanoma[1:32]
sun.67=sunspot[1:32]
#### Univariate Model ####
# Estimate of AR model of Melanoma
p.mel=aic.wge(mel.67,p=0:8,q=0:0)
p.mel$p #[1] 1 Selected a p=1

mel.est=est.ar.wge(mel.67,p=p.mel$p)
    # Coefficients of Original polynomial:  
    #   0.9270 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9270B              1.0788               0.9270       0.0000

# Forecast Next 5 observations
fore.mel = fore.arma.wge(mel.67,phi=mel.est$phi,n.ahead=5,lastn=FALSE,limits=FALSE)
fore.mel$f
    # [1] 3.977020 3.863019 3.757342 3.659380 3.568571

##
# Estimate of AR model of Sunspots
p.sun=aic.wge(sun.67,p=0:8,q=0:0)
p.sun$p #[1] 3   Selected a p=3
sun.est=est.ar.wge(sun.67,p=p.sun$p)
    # Coefficients of Original polynomial:  
    #   0.9039 -0.0469 -0.4317 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.4361B+0.8112B^2    0.8851+-0.6702i      0.9007       0.1031
    # 1+0.5322B             -1.8790               0.5322       0.5000

fore.sun =fore.arma.wge(sun.67,phi=sun.est$phi,n.ahead=5,lastn=FALSE,limits=FALSE)
fore.sun$f
    # [1] 84.39242 98.55759 97.26509 84.90076 67.66916


#  Using Multivariate Analysis
# VAR and VARselect are from CRAN package vars
X=cbind(mel.67,sun.67)
VARselect(X, lag.max = 6, type = "const",season = NULL, exogen = NULL) #AIC = 5.04
    # $selection
    # AIC(n)  HQ(n)  SC(n) FPE(n) 
    # 4      3      3      3
#VARselect picks p=4 (using AIC)

# Fit model to dataset
lsfit=VAR(X,p=4,type='const')
summary(lsfit)

preds=predict(lsfit,n.ahead=5)
    # $mel.67
    # fcst    lower    upper        CI
    # [1,] 4.364052 3.762310 4.965793 0.6017418
    # [2,] 4.635511 3.930144 5.340878 0.7053674
    # [3,] 5.241517 4.349345 6.133689 0.8921718
    # [4,] 5.412168 4.440600 6.383736 0.9715679
    # [5,] 5.544624 4.501463 6.587785 1.0431610
    # 
    # $sun.67
    # fcst    lower    upper       CI
    # [1,] 111.9755 54.73742 169.2135 57.23806
    # [2,] 134.1595 56.87783 211.4411 77.28166
    # [3,] 143.6726 58.01010 229.3352 85.66253
    # [4,] 127.2407 41.06890 213.4125 86.17181
    # [5,]  99.9504 12.01179 187.8890 87.93861
preds$fcst$mel.67[1:5,1] #[1] 4.364052 4.635511 5.241517 5.412168 5.544624
preds$fcst$sun.67[1:5,1] #[1] 111.9755 134.1595 143.6726 127.2407  99.9504

plot(seq(1,37,1),melanoma, type = "b", ylim = c(0,6))
points(seq(33,37,1),preds$fcst$mel.67[1:5,1],type = "b", pch = 15)
fanchart(preds)

####ASE - VAR Model
x = melanoma
x.pred = preds$fcst$mel.67[1:5,1]
n = 5
ASE = mean((x[(length(x)-n+1):(length(x))]-x.pred)^2)
print(paste0('ASE: ',round(ASE,3)))
# [1] "ASE: 0.11"






############
# Multiple Regression Analysis -Melanoma-Sunspots
t=1:37
ksfit=lm(melanoma~sunspot+t)
    # Coefficients:
    #   (Intercept)      sunspot            t  
    #       0.4162       0.0015       0.1165

phi=aic.wge(ksfit$residuals,p=0:8,q=0:0) # AIC picks p=2
    # $type
    # [1] "aic"
    # $value
    # [1] -1.789043
    # $p
    # [1] 2
    # $q
    # [1] 0
    # $phi
    # [1]  0.4731624 -0.2276296
    # $theta
    # [1] 0
    # $vara
    # [1] 0.1421028

fit=arima(melanoma,order=c(phi$p,0,0),xreg=cbind(t,sunspot))
fit
    # Coefficients:
    #         ar1      ar2  intercept       t  sunspot
    #       0.4795  -0.2461     0.3957  0.1157   0.0021
    # s.e.  0.1627   0.1713     0.1951  0.0077   0.0017
    # 
    # sigma^2 estimated as 0.1424:  log likelihood = -16.59,  aic = 45.17

############
# Multiple Regression with correlated errors Analysis
# Melanoma-Sunspots
t=1:32
ksfit=lm(melanoma[1:32]~sunspot[1:32]+t)
    # Coefficients:
    #   (Intercept)  sunspot[1:32]          t  
    #     0.49088        0.00151        0.11059 

phi=aic.wge(ksfit$residuals,p=0:8,q=0:0) # AIC picks p=2
    # $type[1] "aic"
    # $value[1] -1.835114
    # $p[1] 1
    # $q[1] 0
    # $phi[1] 0.3612045

fit=arima(melanoma[1:32],order=c(phi$p,0,0),xreg=cbind(t,sunspot[1:32]))
    # Coefficients:
    #         ar1  intercept       t        
    #       0.3625     0.5240  0.1094  0.0014
    # s.e.  0.1631     0.2316  0.0107  0.0016
    # 
    # sigma^2 estimated as 0.1412:  log likelihood = -14.15,  aic = 38.3

preds = predict(fit,newxreg = data.frame(t = c(33,34,35,36,37), sunspot = sunspot[33:37]))
    # $pred
    # Time Series:
    #   Start = 33 
    # End = 37 
    # Frequency = 1 
    # [1] 4.277084 4.386511 4.495939 4.570814 4.659511

plot(seq(1,37,1),melanoma, type = "b")
points(seq(33,37,1),preds$pred,type = "b", pch = 15)

#### ASE - Melanoma
x = melanoma
x.pred = preds$pred
n = 5
ASE = mean((x[(length(x)-n+1):(length(x))]-x.pred)^2)
print(paste0('ASE: ',round(ASE,3)))
  # [1] "ASE: 0.324"

######## Multiple Regression - Lagged Data
# Look at cross correlation function
ccf(sunspot,melanoma,ylim = c(-1,1)) #Decided to look at Lag2
t=1:35
sun2=sunspot[1:35]
mel2 = c(rep(NA,35))
for(i in 1:35){mel2[i]=melanoma[i+2]}
ksfit=lm(mel2~t+sun2)
phi=aic.wge(ksfit$residuals,p=0:8,q=0:0) # AIC picks p=1
fit=arima(mel2,order=c(phi$p,0,0),xreg=cbind(t,sun2))
fit
    # Coefficients:
    #         ar1  intercept       t    sun2
    #       0.3124     0.2746  0.1177  0.0064
    # s.e.  0.1732     0.1608  0.0068  0.0013
    # 
    # sigma^2 estimated as 0.08449:  log likelihood = -6.47,  aic = 22.94

preds = predict(fit,newxreg = data.frame(t = c(33,34,35,36,37), sunspot = sunspot[33:37]))
plot(seq(1,37,1),melanoma, type = "b")
points(seq(33,37,1),preds$pred,type = "b", pch = 15)

#### ASE - Melanoma - Lagged Data
x = melanoma
x.pred = preds$pred
n = 5
ASE = mean((x[(length(x)-n+1):(length(x))]-x.pred)^2)
print(paste0('ASE: ',round(ASE,3)))
# [1] "ASE: 0.356"


####   Concept Check 12.5.2-5
# If you recall, the model with the time trend and the TV and online advertising 
# variables at lag 1 had an AIC of 352. We would like to compare a VAR model to the 
# previous model based on the AIC.
# Step 1: Reload the data in BusinessSales.csv.  (*Start with a fresh data set. 
# You don’t need to have the lagged variables in the data set when doing VAR. 
#It will search for those automatically.)

#Step 2: Use VARselect() to estimate the order of the VAR model. Use the BIC, which 
#is noted as “SC(n).”
# Dataset: businesssales.csv
BSales = read.csv(file.choose(),header=TRUE)

par(mfrow=c(2,2))
for(i in 2:length(BSales)){ 
  heading = paste(" ",colnames(BSales[i])) 
  plot(BSales$X, BSales[,i], type="l",main=heading,
       xlab="Weeks", ylab=paste(colnames(BSales[i]),'x1000')) 
}
dev.off()

ad_tv1 = dplyr::lag(BSales$ad_tv,1)
ad_online1 = dplyr::lag(BSales$ad_online,1)
discount = BSales$discount
BSales$ad_tv1= ad_tv1
BSales$ad_online1 = ad_online1
ksfit=lm(sales~ad_tv1+ad_online1+discount, data = BSales)
# Coefficients:
#   (Intercept)       ad_tv1   ad_online1     discount  
#     10.08872      5.42230      3.88479     -0.07733 
aic.wge(ksfit$residuals,p=0:8,q=0:0) # AIC picks p=7
# $type
# [1] "aic"
# $value
# [1] 0.7481867
# $p
# [1] 7
# $q
# [1] 0
# $phi
# [1] -0.06461833  0.19212542 -0.08348717 -0.01488629 -0.13475021
# [6]  0.23864863  0.18607832
# $theta
# [1] 0
# $vara
# [1] 1.79781

fit=arima(BSales$sales,order=c(7,0,0),xreg=cbind(ad_tv1, ad_online1, discount))
fit
    # Coefficients:
    #         ar1     ar2      ar3      ar4      ar5      ar6
    #       -0.5197  0.1348  -0.0787  -0.1155  -0.1954  -0.0004
    # s.e.   0.1839  0.1689   0.1233   0.1256   0.1309   0.1884
    #         ar7  intercept  ad_tv1  ad_online1  discount
    #       0.0485     4.8382  3.4341      8.1152   -0.0573
    # s.e.  0.1574     2.8270  0.6166      1.2447    0.0281
    # 
    # sigma^2 estimated as 1.642:  log likelihood = -165.43,  aic = 354.86

BSales = read.csv(file.choose(),header=TRUE)
VARselect(BSales, lag.max = 6, type = "const",season = NULL, exogen = NULL) 
    # $selection
    # AIC(n)  HQ(n)  SC(n) FPE(n) 
    # 1      1      1      1 
#VARselect picks p=1 (using BIC - SC(n))

#Concept Check 12.5.3
#Use VAR() and the order estimated in the last step to fit the VAR model.
# Fit it with a trend and without a trend.
# Is there enough evidence to suggest the trend term is appropriate?

lsfit=VAR(BSales,p=1,type="trend")
summary(lsfit)
?VAR()

#Concept Check 12.5.4
#Using a VAR model without the trend and the estimated order, is there strong 
#evidence that sales is related to TV advertising at a lag of 1?
lsfit_l1=VAR(BSales,p=1,type="const")
summary(lsfit_l1$varresult$sales)
