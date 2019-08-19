# Repiratory Mortality LA Pollution Study

#EDA
library(tidyverse)
library(GGally)
library(astsa)
library(tswge)
library(ggplot2)
library(nnfor)

  
??astsa
# Load Dataset
data("lap")
RM = lap

RM[1:5,]

# Plottting original Data
par(mfrow=c(3,2))
plot(RM[,2],type='l',main="Repratory Mortality",font.main=4,
     col='red',
     xlab="Weeks",
     ylab="Resp Motality")
plot(RM[,4],type='l',main="Temperature",font.main=4,
     col='Blue',
     xlab="Weeks",
     ylab="Temperature °C")
plot(RM[,5],type='l',main="Relative Humidity",font.main=4,
     col='green',
     xlab="Weeks",
     ylab="Rel Humidity")
plot(RM[,6],type='l',main="Carbon Monixide",font.main=4,
     col='blue',
     xlab="Weeks",
     ylab="Carbon Monoxide")
plot(RM[,7],type='l',main="Sulfur Dioxide",font.main=4,
     col='blue',
     xlab="Weeks",
     ylab="Sulfur Dioxide")
plot(RM[,8],type='l',main="Nitrogen Dioxide",font.main=4,
     col='green',
     xlab="Weeks",
     ylab="Nitrogen Dioxide")




par(mfrow=c(2,2))
plot(RM[,2],type='l',main="Repratory Mortality",font.main=4,
     col='red',
     xlab="Weeks",
     ylab="Resp Motality")
plot(RM[,9],type='l',main="Hydrocarbons",font.main=4,
     col='blue',
     xlab="Weeks",
     ylab="Hydrocarbons")
plot(RM[,10],type='l',main="Ozone",font.main=4,
     col='blue',
     xlab="Weeks",
     ylab="Ozone")
plot(RM[,11],type='l',main="Particulates",font.main=4,
     col='green',
     xlab="Weeks",
     ylab="Ozone")
?plotts.sample.wge()

# Plot of RM data
RMplot = plotts.sample.wge(RM[,2],lag.max = 60)
# Appears to have a significant frequency at 0.4 approx 25 or 26 periods.

RM_52 = artrans.wge(RM[,2], c(rep(0,51),1))
plotts.sample.wge(RM_52)

#Univariate Response
aic5.wge(RM[,2], p=0:10,q=0:3, type = 'bic')  # Picked a p=3:q=1
aic5.wge(RM[,2], p=0:15,q=0:3, type = 'aic')  # Also Picked a p=3:q=1
aic5.wge(RM_52, p=0:10,q=0:3, type = 'bic')  # Picked a p=4:q=0
aic5.wge(RM_52, p=0:15,q=0:3, type = 'aic')  # Picked a p=4:q=0
est = est.arma.wge(RM_52, p = 4, q = 0)
RM_52_AR4_MA0 = artrans.wge(RM_52,phi = est$phi)
ljung.wge(RM_52_AR4_MA0)
ljung.wge(RM_52_AR4_MA0,K=48)
acf(RM_52_AR4_MA0)

?plotts.sample.wge()


?artrans.wge()
?est.arma.wge()
?fore.aruma.wge()

RM_52_AR4_MA0.fore=fore.aruma.wge(RM[,2],phi = est$phi, s = 52, n.ahead = 26,lastn = TRUE)


ASE_ARMA = mean((RM[,2][483:508] - RM_52_AR4_MA0.fore$f)^2)
ASE_ARMA #[1] 2.010904


# Train NN model on first 482 observations and predict last 26
set.seed(244)
RM_small = window(RM, start = c(1970, 1),end = c(1979,14))  
RM.fit.mlp= mlp(RM_small[,2], reps=50,comb='mean')
RM.fit.mlp
    # MLP fit with 5 hidden nodes and 50 repetitions.
    # Series modelled in differences: D1.
    # Univariate lags: (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,27,30,42,47,50)
    # Deterministic seasonal dummies included.
    # Forecast combined using the mean operator.
    # MSE: 0.3154.

# Visualize the Neural Network
plot(RM.fit.mlp)

# Create a forecast for 26 segments ahead
?forecast()
RM.fore.mlp = forecast(RM.fit.mlp,h=26)
plot(RM.fore.mlp)

ASE_MLP = mean((RM[,2][483:508] - RM.fore.mlp$mean)^2)
ASE_MLP #[1] 5.19172

#Creating an ensemble method
#ensemble

ensemble_uni = (RM_52_AR4_MA0.fore$f + RM.fore.mlp$mean)/2

plot(SM$Melanoma, type = "l")
lines(seq(30,37,1),ensemble,col = "green")

ASE_ens = mean((RM[,2][483:508] - ensemble_uni)^2)
ASE_ens #[1] 2.776065

dev.off()
plot(seq(470,508,1),RM[470:508,2], type = "l",
     main='Predictions-Repiratory Mortality Univariate ARUMA, Neural Network, Ensemble'
     ,xlab="Weeks"
     ,ylab="Mortality",
     xlim=c(470, 510),
     ylim=c(5.5,14),lwd=3)
points(seq(482,508,1),c(RM[482,2],RM_52_AR4_MA0.fore$f),type='l',
       col = 'red',lwd=2,lty=2)
points(seq(482,508,1),c(RM[482,2],RM.fore.mlp$mean),type='l',
       col = 'green',pch = 17,lwd=2,lty=3)
points(seq(482,508,1),c(RM[482,2],ensemble_uni),type='l', 
       col = 'blue',pch = 17,lwd=2,lty=6)
abline(v=482, col="grey",lwd=2)
legend('topleft', legend=c("Actual","ARMA", "NN","Ensemble"),
        col=c("black","red", "green","blue"), lty=c(1,2,3,6), lwd=c(3,2,2,2),y.intersp=0.5)






#############################################################################
##################  Multivariate Analysis  ##################################

############ VAR MODELS ##########################

#VAR Model 1 Forecasts Seasonally Differenced Data 

#Difference all series to make them stationary (assumptoin of VAR)
# Doesn't have to be white... just stationary
library(vars)

RM_small = window(RM, start = c(1970, 1),end = c(1979,14))  
RMS_52 = artrans.wge(RM_small[,2], c(rep(0,51),1))
RMS_52_tmp = artrans.wge(RM_small[,4], c(rep(0,51),1))
RMS_52_rh = artrans.wge(RM_small[,5], c(rep(0,51),1))
RMS_52_co = artrans.wge(RM_small[,6], c(rep(0,51),1))
RMS_52_so2 = artrans.wge(RM_small[,7], c(rep(0,51),1))
RMS_52_no2 = artrans.wge(RM_small[,8], c(rep(0,51),1))
RMS_52_hyd = artrans.wge(RM_small[,9], c(rep(0,51),1))
RMS_52_o3 = artrans.wge(RM_small[,10], c(rep(0,51),1))
RMS_52_part = artrans.wge(RM_small[,11], c(rep(0,51),1))

#VARSelect on Differenced Data chooses 42
VARselect(cbind(RMS_52,RMS_52_tmp, RMS_52_part),lag.max = 50, type = "both")
    # AIC(n)  HQ(n)  SC(n) FPE(n) 
    # 5      4      1      5 

#VAR with p = 2
RMortDiffVAR = VAR(cbind(RMS_52,RMS_52_tmp, RMS_52_part),type = "both",p = 2)
preds=predict(RMortDiffVAR,n.ahead=26)
#?predict()

#We have predicted differences .... calculate actual respiratory mortalities 
startingPoints = RM[,2][483:508]
RMortForcasts = preds$fcst$RMS_52[,1:3] + startingPoints

ASE_VAR = mean((RM[,2][483:508] - RMortForcasts[,1])^2)
ASE_VAR #[1] 0.06722774


###### MLP model ###################


RM_small = window(RM, start = c(1970, 1),end = c(1979,14))

RM_smallDF = data.frame(week=seq(1,482),RM_small[,c(4:11)])
fit.mlp = mlp(ts(RM_small[,2]),reps = 50,comb = "mean",xreg = RM_smallDF)
fit.mlp
    # MLP fit with 5 hidden nodes and 50 repetitions.
    # Univariate lags: (1,2,4)
    # 2 regressors included.
    # - Regressor 1 lags: (1,2,4)
    # - Regressor 2 lags: (2)
    # Forecast combined using the mean operator.
    # MSE: 1.6597.
plot(fit.mlp)

RMDF = data.frame(week=seq(1,508),RM[,c(4:11)])
fore.xreg.mlp = forecast(fit.mlp, h = 26, xreg = RMDF)
plot(fore.xreg.mlp)
ASE.xreg.mlp = mean((RM[,2][483:508] - fore.xreg.mlp$mean)^2)
ASE.xreg.mlp #[1] 1.908502

#Creating an ensemble method
#ensemble

ensemble_multi = (RMortForcasts[,1] + fore.xreg.mlp$mean)/2

dev.off()
plot(seq(470,508,1),RM[470:508,2], type = "l",
     main='Predictions-Repiratory Mortality Univariate VAR, Neural Network, Ensemble'
     ,xlab="Weeks"
     ,ylab="Mortality",
     xlim=c(470, 510),
     ylim=c(5.5,14),lwd=3)
points(seq(482,508,1),c(RM[482,2],RMortForcasts[,1]),type='l',
       col = 'red',lwd=2,lty=2)
points(seq(482,508,1),c(RM[482,2],fore.xreg.mlp$mean),type='l',
       col = 'green',pch = 17,lwd=2,lty=3)
points(seq(482,508,1),c(RM[482,2],ensemble_multi),type='l', 
       col = 'blue',pch = 17,lwd=2,lty=6)
abline(v=482, col="grey",lwd=2)
legend('topleft', legend=c("Actual","VAR", "NN","Ensemble"),
       col=c("black","red", "green","blue"), lty=c(1,2,3,6), lwd=c(3,2,2,2),y.intersp=0.5)


RMortVAR_full = VAR(cbind(Mortality = RM[,2],Temp = RM[,4],Part = RM[,11]),season = 52, type = "both",p = 2)
preds=predict(RMortVAR_full,n.ahead=5)
print(paste0('VAR Model Predictions:',preds))



preds$fcst$RM...2.[,1]












