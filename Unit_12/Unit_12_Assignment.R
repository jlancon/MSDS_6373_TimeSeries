library(vars)
library(tswge)

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
BSales95 = BSales[0:95,]
par(mfrow=c(2,2))
for(i in 2:length(BSales)){ 
  heading = paste(" ",colnames(BSales[i])) 
  plot(BSales$X, BSales[,i], type="l",main=heading,
       xlab="Weeks", ylab=paste(colnames(BSales[i]),'x1000')) 
}
dev.off()

### Fitting an  ARIMA model ###
# Determining if a 'lag in variables would be warranted
# Using Cross-Correlation Function
par(mfrow=c(3,1))
ccf(BSales95$sales,BSales$ad_tv)
ccf(BSales95$sales,BSales$ad_online)
ccf(BSales95$sales,BSales$discount)

# Lagging data and creating new variables
# Also adding time as a variable
t=1:95
ad_tv1 = dplyr::lag(BSales$ad_tv[1:95],1)
ad_online1 = dplyr::lag(BSales$ad_online[1:95],1)
discount3 = dplyr::lag(BSales$discount[1:95],3)
BSales95$ad_tv1= ad_tv1
BSales95$ad_online1 = ad_online1
BSales95$discount3 = discount3

# Fitting linear Model, using lagged variables and trend
ksfit=lm(sales~t+ad_tv1+ad_online1+discount3, data = BSales95)
plot(ksfit$residuals,type='l',main="Linear Model Residuals",font.main=4,
     xlab="Weeks",
     ylab="Sales (x1000)")

# Using lm residuals for determine ARMA(p) model
aic5.wge(ksfit$residuals,p=0:10,q=0:2,type = 'aic') # AIC picks p=7; q=0
    # Five Smallest Values of  aic 
    # p    q        aic
    # 22    7    0  0.6254352
    # 20    6    1  0.6380274
    # 25    8    0  0.6382546
    # 26    8    1  0.6410901
    # 23    7    1  0.6425060

# Fit Multivariate ARIMA model (7,0,0) with trend
fit=arima(BSales95$sales,order=c(7,0,0),xreg=cbind(t,ad_tv1, ad_online1, discount3))
fit
    # Coefficients:
    #         ar1     ar2      ar3      ar4      ar5     ar6
    #       0.0321  0.2854  -0.2124  -0.0601  -0.1097  0.2865
    # s.e.  0.2534  0.1157   0.1269   0.1108   0.1106  0.1118
    #       ar7     intercept       t   ad_tv1  ad_online1  discount3
    #       0.230    17.3239    0.0219  3.6001      4.3265    -0.1004
    # s.e.  0.117     5.5817    0.0114  0.5796      1.3652     0.0473
    # 
    # sigma^2 estimated as 1.476:  log likelihood = -149.22,  aic = 324.44

plotts.sample.wge(fit$residual[!is.na(fit$residual)],arlimits = TRUE)
aic5.wge(fit$residual[!is.na(fit$residual)],p=0:10,q=0:3)
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 1    0    0  0.4105123
    # 5    1    0  0.4316862
    # 2    0    1  0.4317152
    # 3    0    2  0.4526119
    # 9    2    0  0.4527799

# Predicting Sales values for weeks 96-100
preds = predict(fit,newxreg = data.frame(t = c(96,97,98,99,100), 
                                         ad_tv1 = BSales$ad_tv[96:100],
                                         ad_online1 = BSales$ad_online[96:100],
                                         discount = BSales$discount[96:100]))
dev.off()
plot(seq(80,100,1),BSales$sales[80:100], type = "b")
points(seq(96,100,1),preds$pred,type='b', col = 'red',pch = 15)

# Prediction Matrix
PredMatrix = cbind(BSales$sales[96:100],preds$pred)
PredMatrix
    #         BSales$sales[96:100]  preds$pred
    # 96             61.31576       61.24130
    # 97             59.94334       55.17678
    # 98             55.57380       53.69783
    # 99             50.21189       50.57694
    # 100             48.10178      52.31508

#### ASE - Sales - Lagged Data and time trend
x = BSales$sales
x.pred = preds$pred
n = 5
ASE = mean((x[(length(x)-n+1):(length(x))]-x.pred)^2)
print(paste0('ASE: ',round(ASE,3)))
# [1] "ASE: 8.826"



#######################################################
###   VAR Model
######################################################
BSales = read.csv(file.choose(),header=TRUE)
BSalesVAR = BSales[,2:5] # Removing time variable 'X'
BSalesVAR95 = BSalesVAR[1:95,]
# Fit VAR Model without a trend 
VARselect(BSalesVAR95, lag.max = 6, type = "const",season = NULL, exogen = NULL) 
    # $selection
    # AIC(n)  HQ(n)  SC(n) FPE(n) 
    # 3      2      2      3 
#VARselect (no-trend) picks: #BIC/SC(2) -6.595 / AIC(3) -7.762

# Fit VAR Model with a trend 
VARselect(BSalesVAR95, lag.max = 6, type = "trend",season = NULL, exogen = NULL) 
    # $selection
    # AIC(n)  HQ(n)  SC(n) FPE(n) 
    # 3      3      2      3 
#VARselect (with-trend) picks: #BIC/SC(2) -6.177 / AIC(3) -7.627


# Fit VAR(p) model without trend, using p=2
lsfit=VAR(BSalesVAR95,p=2,type="const")
summary(lsfit)
    # Estimation results for equation sales: 
    #   ====================================== 
    #   sales = sales.l1 + ad_tv.l1 + ad_online.l1 + 
    #           discount.l1 + sales.l2 + ad_tv.l2 + 
    #           ad_online.l2 + discount.l2 + const 
    # 
    #   Estimate    Std. Error    t value     Pr(>|t|)    
    #   sales.l1      0.31842    0.13228   2.407 0.018270 *  
    #   ad_tv.l1      2.18509    0.48783   4.479 2.34e-05 ***
    #   ad_online.l1  4.36774    0.72791   6.000 4.81e-08 ***
    #   discount.l1   0.08165    0.10919   0.748 0.456700    
    #   sales.l2     -0.32226    0.06449  -4.997 3.13e-06 ***
    #   ad_tv.l2      0.49881    0.53206   0.938 0.351185    
    #   ad_online.l2  3.21157    0.79850   4.022 0.000126 ***
    #   discount.l2  -0.18004    0.10942  -1.645 0.103622    
    #   const        11.27747    2.17815   5.178 1.51e-06 ***
    #   ---
    #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Predicting Sales values for weeks 96-100; VAR(2)
VAR_preds=predict(lsfit,n.ahead=5)
VAR_preds$fcst$sales[,1]
    #[1] 59.87337 58.96541 56.50778 54.19601 52.96994

plot(seq(80,100,1),BSales$sales[80:100], type = "b")
points(seq(96,100,1),VAR_preds$fcst$sales[,1],type='b',
       col = 'red',pch = 15)

# Prediction Matrix - VAR
VAR_PredMatrix = cbind(BSales$sales[96:100],
                       VAR_preds$fcst$sales[,1])
colnames(VAR_PredMatrix) = c('BSales$sales','Pred_sales')
rownames(VAR_PredMatrix) <- 96:100
VAR_PredMatrix
    #     BSales$sales Pred_sales
    # 96      61.31576   59.87337
    # 97      59.94334   58.96541
    # 98      55.57380   56.50778
    # 99      50.21189   54.19601
    # 100     48.10178   52.96994


#### VAR(p) - Sales Prediction - ASE
x = BSales$sales
x.pred = VAR_preds$fcst$sales[,1]
n = 5
VAR_ASE = mean((x[(length(x)-n+1):(length(x))]-x.pred)^2)
print(paste0('ASE: ',round(VAR_ASE,3)))
# [1] "ASE: 8.696"




#######################################################
###   Multivariate Model - Air QUality LA_Cmort_Study.csv
######################################################
AQ = read.csv(file.choose(),header=TRUE)
head(AQ)
#     Week  temp    part    cmort
# 1    1    72.38   72.72   97.85
# 2    2    67.19   49.60   104.64
# 3    3    62.94   55.68   94.36
# 4    4    72.49   55.16   98.05
# 5    5    74.25   66.02   95.85
# 6    6    67.88   44.01   95.98


# Visualization of Realizations
# Dataset: LA_CMort_Study.csv
par(mfrow=c(2,2))
for(i in 2:length(AQ)){ 
  heading = paste(" ",colnames(AQ[i])) 
  plot(AQ$Week, AQ[,i], type="l",main=heading,
       xlab="Weeks", ylab=paste(colnames(AQ[i]),' ')) 
}
dev.off()

### Fitting an  ARIMA model ###
# Determining if a 'lag in variables would be warranted
# Using Cross-Correlation Function
par(mfrow=c(2,1))
ccf(AQ$cmort,AQ$part) #Appears to have a lag of 7
ccf(AQ$cmort,AQ$temp) #Appears to have a lag of 1

# Using 230 observations for training and 20 for testing
AQAR = AQ[1:230,]

# Lagging data and creating new variables
# Also adding time as a variable
t=1:230
part7 = dplyr::lag(AQAR$part,7)
temp1 = dplyr::lag(AQAR$temp,1)

AQAR$part7= part7
AQAR$temp1 = temp1
head(AQAR,10)
    #     Week  temp    part    cmort   part7   temp1
    # 1     1   72.38   72.72   97.85      NA      NA
    # 2     2   67.19   49.60   104.64     NA   72.38
    # 3     3   62.94   55.68   94.36      NA   67.19
    # 4     4   72.49   55.16   98.05      NA   62.94
    # 5     5   74.25   66.02   95.85      NA   72.49
    # 6     6   67.88   44.01   95.98      NA   74.25
    # 7     7   74.20   47.83   88.63      NA   67.88
    # 8     8   74.88   43.60   90.85   72.72   74.20
    # 9     9   64.17   24.99   92.06   49.60   74.88
    # 10   10   67.09   40.41   88.75   55.68   64.17

# Fitting linear Model, using lagged variables and trend
ksfit=lm(cmort~t+part7+temp1, data = AQAR)
dev.off()
plot(ksfit$residuals,type='l',main="Linear Model Residuals",font.main=4,
     xlab="Weeks",
     ylab="Mortality")

# Using lm residuals for determine ARMA(p) model
aic5.wge(ksfit$residuals,p=0:15,q=0:2,type = 'bic') # AIC5 picks p=2; q=0
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 7     2    0   3.723562
    # 8     2    1   3.746356
    # 10    3    0   3.747016
    # 5     1    1   3.750141
    # 6     1    2   3.750989

# Fit Multivariate ARIMA model (2,0,0) with trend
fit=arima(AQAR$cmort,order=c(2,0,0),xreg=cbind(t,part7, temp1))
fit
    # Coefficients:
    #         ar1     ar2  intercept        t   part7    temp1
    #       0.3402  0.4244   103.7295  -0.0130  0.0932  -0.1834
    # s.e.  0.0624  0.0678     6.1402   0.0244  0.0375   0.0651
    # 
    # sigma^2 estimated as 34.73:  log likelihood = -712.4,  aic = 1438.79

# Plotting residuals of fit model
plotts.sample.wge(fit$residual[!is.na(fit$residual)],arlimits = TRUE)
aic5.wge(fit$residual[!is.na(fit$residual)],p=0:10,q=0:3)
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 1     0    0   3.556553
    # 5     1    0   3.565394
    # 2     0    1   3.565397
    # 16    3    3   3.573303
    # 9     2    0   3.574223

# Predicting Sales values for weeks 231-250
AQpreds = predict(fit,newxreg = data.frame(t = c(seq(231,250,1)), 
                                           part7 = AQ$part[231:250],
                                           temp1 = AQ$temp[231:250]))
dev.off()
plot(seq(231,250,1),AQ$cmort[231:250], type = "b",
     main='Predictions-Mortality',xlab="Weeks",ylab="Mortality")
points(seq(231,250,1),AQpreds$pred,type='b', col = 'red',pch = 15)

# Prediction Matrix
AQPredMatrix = cbind(AQ$cmort[231:250],AQpreds$pred)
colnames(AQPredMatrix) = c('AQ$cmort','Pred_Mort')
rownames(AQPredMatrix) <- 231:250
head(AQPredMatrix)
    #       AQ$cmort Pred_Mort
    # 231    84.68  83.63805
    # 232    81.53  83.94565
    # 233    89.20  83.62570
    # 234    74.51  83.79173
    # 235    81.85  84.67281
    # 236    81.97  84.50624

#### ASE - cmort - Lagged Data and time trend
x = AQ$cmort
x.pred = AQpreds$pred
n = 5
ASE = mean((x[(length(x)-n+1):(length(x))]-x.pred)^2)
print(paste0('ASE: ',round(ASE,3)))
# [1] "ASE: 41.578"


#######################################################
###   VAR Model - Air QUality LA_Cmort_Study.csv
######################################################


AQVAR = AQ[,2:4] # Removing week variable 'Week'

# Using 230 observations for training and 20 for testing
AQVAR = AQVAR[1:230,]

# Fit VAR Model without a trend 
VARselect(AQVAR, lag.max = 8, type = "const",season = NULL, 
          exogen = NULL) 
    # $selection
    # AIC(n)  HQ(n)  SC(n) FPE(n) 
    # 6      2      2      6 
#VARselect (no-trend) picks: #BIC/SC(2) 11.757 / AIC(6) 11.328

# Fit VAR Model with both constant and a trend 
VARselect(AQVAR, lag.max = 8, type = "both",season = NULL, exogen = NULL) 
    # $selection
    # AIC(n)  HQ(n)  SC(n) FPE(n) 
    # 6      2      2      6 
#VARselect (with-trend) picks: #BIC/SC(2) 11.808 / AIC(6) 11.337

# Fit VAR(p) model without trend, using p=2
AQ_lsfit=VAR(AQVAR,p=2,type="const")
summary(AQ_lsfit)
    # 
    # Estimation results for equation cmort: 
    #   ====================================== 
    #   cmort = temp.l1 + part.l1 + cmort.l1 + temp.l2 +
    #           part.l2 + cmort.l2 + const 
    # 
    #           Estimate    Std. Error t value  Pr(>|t|)    
    #   temp.l1  -0.19099    0.06904  -2.766    0.006150 ** 
    #   part.l1   0.05734    0.04419   1.297    0.195857    
    #   cmort.l1  0.24110    0.06421   3.755    0.000222 ***
    #   temp.l2  -0.02844    0.07060  -0.403    0.687487    
    #   part.l2   0.08707    0.04599   1.893    0.059652 .  
    #   cmort.l2  0.34575    0.06281   5.505    1.02e-07 ***
    #   const    47.66142    8.68841   5.486  1.12e-07 ***
    #   ---
    #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    # 
    # 
    # Residual standard error: 5.716 on 221 degrees of freedom
    # Multiple R-Squared: 0.6499,	Adjusted R-squared: 0.6404 
    # F-statistic: 68.39 on 6 and 221 DF,  p-value: < 2.2e-16 

# Predicting Sales values for weeks 231-250; VAR(2)
AQVAR_preds=predict(AQ_lsfit,n.ahead=20)
AQVAR_preds$fcst$cmort[,1]
    # [1] 82.31314 84.08373 83.26914 84.01505 84.01362 84.50331 84.77040
    # [8] 85.19568 85.54671 85.94417 86.30913 86.68004 87.02962 87.37002
    # [15] 87.69189 87.99916 88.28890 88.56264 88.81976 89.06120

# Plot realization and predicted values of cmort
dev.off()
plot(seq(231,250,1),AQ$cmort[231:250], type = "b",
     main='Predictions-Mortality VAR',xlab="Weeks",ylab="Mortality")
points(seq(231,250,1),AQVAR_preds$fcst$cmort[,1],type='b',
       col = 'red',pch = 15)

# Prediction Matrix - VAR
AQVAR_PredMatrix = cbind(AQ$cmort[231:250],
                         AQVAR_preds$fcst$cmort[,1])
colnames(AQVAR_PredMatrix) = c('AQ$cmort','Pred_Mort')
rownames(AQVAR_PredMatrix) <- 231:250
head(AQVAR_PredMatrix)
    #       AQ$cmort Pred_Mort
    # 231    84.68  82.31314
    # 232    81.53  84.08373
    # 233    89.20  83.26914
    # 234    74.51  84.01505
    # 235    81.85  84.01362
    # 236    81.97  84.50331


#### VAR(p) - Sales Prediction - ASE
x = AQ$cmort
x.pred = AQVAR_preds$fcst$cmort[,1]
n = 20
AQVAR_ASE = mean((x[(length(x)-n+1):(length(x))]-x.pred)^2)
print(paste0('ASE: ',round(AQVAR_ASE,3)))
    # [1] "ASE: 71.865"

dev.off()
plot(seq(220,250,1),AQ$cmort[220:250], type = "b",
     main='Predictions-Mortality VAR & Multivariate'
     ,xlab="Weeks"
     ,ylab="Mortality",
     sub='Green - Multivariate  Red-VAR')
points(seq(231,250,1),AQVAR_preds$fcst$cmort[,1],type='b',
       col = 'red',pch = 17)
points(seq(231,250,1),AQpreds$pred,type='b', col = 'green',pch = 15)
legend(240, 82, legend=c("Multivariate AR(2)", "VAR(2)"),
       col=c("green", "red"), lty=1:1,pch=15:17, cex=0.95)
