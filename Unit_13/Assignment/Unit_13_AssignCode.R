#  MSDS 6373- Time Series Analysis - Unit 13 Assignment Code 
# Neural Networks

#  Team Member:  Jeffery Lancon
#  
#  Date: 08/02/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 7)

library(nnfor)
library(tswge)
library(forecast)
library(vars)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf

# Model Sunspot/melanoma data with a MLP. Use the model to provide eight-
# step-ahead forecasts.
# Multiple Regression with Correlated Errors
# Dataset: Melanoma_Sunspot.csv
SunMel = read.csv(file.choose(),header=TRUE)

MelTrain = ts(SunMel$Melanoma[1:29],start = c(1936,1),frequency = 1)
MelTrain_DF = data.frame(MelTrain)
MelTest = ts(SunMel$Melanoma[30:37],start = c(1965,1),frequency = 1)
MelTest_DF = data.frame(MelTest)
SunTrain = ts(SunMel$Sunspot[1:29],start = c(1936,1),frequency = 1)
SunTrain_DF = data.frame(SunTrain)
SunTest = ts(SunMel$Sunspot[30:37],start = c(1965,1),frequency = 1)
SunTest_DF = data.frame(SunTest)

plotts.sample.wge(MelTrain)
par(mfrow=c(2,1))
plot(MelTrain,
     main='Melanoma Rates'
     ,xlab="Year"
     ,ylab="Cancer Rate x100K")
plot(SunTrain,
     main='Sunspot activity'
     ,xlab="Year"
     ,ylab="Sunspots")
#?mlp()

set.seed(255)
fit.mlp= mlp(MelTrain, reps=50,allow.det.season = TRUE,
             comb='mean',xreg = SunTrain_DF,hd.auto.type = 'cv')
fit.mlp
    # MLP fit with 3 hidden nodes and 50 repetitions.
    # Series modelled in differences: D1.
    # Univariate lags: (1,2,3)
    # 1 regressor included.
    # - Regressor 1 lags: (1,2,3)
    # Forecast combined using the mean operator.
    # MSE: 0.0079.

# Visualize the Neural Network
# Note: Cyan nodes are for regressor lags
# and grey are for univariate lagged inputs
dev.off()
plot(fit.mlp)


# Create a forecast for 8 segments ahead
fore.mlp = forecast(fit.mlp, h = 8, 
                    xreg = data.frame(SunMel$Sunspot))
plot(fore.mlp)

# Forecasted Model grading using ASE
ASE.mlp = mean((MelTest-fore.mlp$mean)^2)
ASE.mlp
    # [1] 0.08215025


##### VAR Model ####
#?VARselect()
SunMelTrain = SunMel[1:29,2:3]
VARselect(SunMelTrain, lag.max = 10, type = "both")
#VARselect picks p=4 (using BIC)

SunMel.VAR = VAR(SunMelTrain, p = 4, type = "both")
    # VAR Estimation Results:
    #   ======================= 
    #   
    #   Estimated coefficients for equation Melanoma: 
    #   ============================================= 
    #   Call:
    #   Melanoma = Melanoma.l1 + Sunspot.l1 + Melanoma.l2 
    #   + Sunspot.l2 + Melanoma.l3 + Sunspot.l3 + Melanoma.l4 
    #   + Sunspot.l4 + const + trend 
    # 
    # Melanoma.l1   Sunspot.l1  Melanoma.l2   Sunspot.l2  Melanoma.l3 
    # 0.543048927 -0.003809321 -0.426117347  0.014012032  0.204913594 
    # Sunspot.l3  Melanoma.l4   Sunspot.l4        const        trend 
    # -0.008980316  0.085930715  0.001936931  0.194936006  0.064855312 


# Create a forecast for 8 segments ahead
SunMel.VAR.pred = predict(SunMel.VAR,n.ahead = 8)

# Forecasted Model grading using ASE
ASE.VAR = mean((SunMel$Melanoma[30:37] - 
                  SunMel.VAR.pred$fcst$Melanoma[,1])^2)
ASE.VAR
    # [1] 0.3867062

# Plotting Forecast vs actual for both VAR and MLP
plot(SunMel$Melanoma, type = "l",
     main='Melanoma Rates',
     xlab="Year"
     ,ylab="Cancer Rate x100K")
lines(seq(30,37,1),SunMel.VAR.pred$fcst$Melanoma[,1],col = "red")
lines(seq(30,37,1),fore.mlp$mean,col = "green")
#axis(1, at=1:37, labels=seq(1936,1972,1),las=2)
legend(26, 3, legend=c("MLP Forecast", "VAR Forecast",'Actual'),
       col=c("green", "red",'black'), lty=1, cex=0.9)

###################################################
###################################################

# Model Schumway data with a MLP. Use the model to provide 10-
# step-ahead forecasts.
# Dataset: la_cmort_study.csv
MOR = read.csv(file.choose(),header=TRUE)

MORTrain = MOR[1:498,2:4]
MORTest = MOR[499:(length(MOR$temp)),2:4]

par(mfrow=c(3,1))
plot(MORTrain$cmort,type = "l",
     main='Cardiac Mortality Rates'
     ,xlab="Week"
     ,ylab="Mortality Rate")
plot(MORTrain$part,type = "l",
     main='Particulate Levels'
     ,xlab="Week"
     ,ylab="Particulates")
plot(MORTrain$temp,type = "l",
     main='Temperature'
     ,xlab="Week"
     ,ylab="Temperature")

set.seed(255)
MORfit.mlp= mlp(ts(MORTrain$cmort), reps=30,allow.det.season = TRUE,
             comb='mean',xreg = data.frame(MORTrain[,2:3],Week=seq(1,498,1)))
MORfit.mlp
    # MLP fit with 5 hidden nodes and 30 repetitions.
    # Univariate lags: (1,2)
    # 2 regressors included.
    # - Regressor 1 lags: (1,2,4)
    # - Regressor 2 lags: (1)
    # Forecast combined using the mean operator.
    # MSE: 21.4336.

# Visualize the Neural Network
# Note: Cyan nodes are for (2) regressor (3x1)lags
# and grey are for univariate (2) lagged inputs
dev.off()
plot(MORfit.mlp)


# Create a forecast for 10 segments ahead
MORfore.mlp = forecast(MORfit.mlp, h = 10, 
                    xreg = data.frame(MOR[,2:3],Week=seq(1,508,1)))
plot(MORfore.mlp)

# Forecasted Model grading using ASE
MORASE.mlp = mean((MORTest[,3]-MORfore.mlp$mean)^2)
MORASE.mlp
    # [1] 78.96093


##### VAR Model ####
#?VARselect()
VARselect(MORTrain, lag.max = 10, type = "both", season = 52)
#VARselect picks p=5 (using AIC)

MOR.VAR = VAR(MORTrain, p = 5, type = "both",season = 52)
MOR.VAR
  # Estimated coefficients for equation cmort: 
  #   ========================================== 
  #   Call:
  #   cmort = temp.l1 + part.l1 + cmort.l1 + temp.l2 + part.l2 + cmort.l2 + temp.l3 
  #         + part.l3 + cmort.l3 + temp.l4 + part.l4 + cmort.l4 + temp.l5 + part.l5 
  #         + cmort.l5 + const + trend + sd1 + sd2 + sd3 + sd4 + sd5 + sd6 + sd7 + sd8 
  #         + sd9 + sd10 + sd11 + sd12 + sd13 + sd14 + sd15 + sd16 + sd17 + sd18 + sd19
  #         + sd20 + sd21 + sd22 + sd23 + sd24 + sd25 + sd26 + sd27 + sd28 + sd29 + sd30
  #         + sd31 + sd32 + sd33 + sd34 + sd35 + sd36 + sd37 + sd38 + sd39 + sd40 + sd41
  #         + sd42 + sd43 + sd44 + sd45 + sd46 + sd47 + sd48 + sd49 + sd50 + sd51 
  # 
  # temp.l1     part.l1    cmort.l1     temp.l2     part.l2    cmort.l2     temp.l3 
  # -0.14547886 -0.04305943  0.28505181 -0.05599289 -0.02362104  0.33722081  0.03750208 
  # part.l3    cmort.l3     temp.l4     part.l4    cmort.l4     temp.l5     part.l5 
  # -0.05130228  0.03161504  0.04164536  0.05498033 -0.01804954 -0.08793782  0.04831086 
  # cmort.l5       const       trend         sd1         sd2         sd3         sd4 
  # -0.03992639 55.36808758 -0.01254698 -4.67703160 -3.04175945 -6.65098215 -4.95486807 
  # sd5         sd6         sd7         sd8         sd9        sd10        sd11 
  # -4.27836934 -6.71148787 -5.81253814 -5.00093042 -7.07416439 -5.23836398 -6.76306641 
  # sd12        sd13        sd14        sd15        sd16        sd17        sd18 
  # -8.14131973 -4.92713258 -8.87134375 -2.93944680 -7.73875192 -9.03311279 -7.72220774 
  # sd19        sd20        sd21        sd22        sd23        sd24        sd25 
  # -3.96921985 -1.45744098 -6.33796337 -5.85317778 -5.34202427 -6.27480008 -0.26480198 
  # sd26        sd27        sd28        sd29        sd30        sd31        sd32 
  # -5.28920096 -6.48582834 -6.85458296 -5.48504771 -3.13940511 -4.57074342 -3.94723622 
  # sd33        sd34        sd35        sd36        sd37        sd38        sd39 
  # -4.40699103 -2.02778950 -5.18819474 -3.50208100 -2.62759054  0.64569619 -4.28921323 
  # sd40        sd41        sd42        sd43        sd44        sd45        sd46 
  # -1.40012778 -3.27724672 -2.45630900 -0.05649595 -1.07864061  0.81418382  7.19760654 
  # sd47        sd48        sd49        sd50        sd51 
  # 6.05422161 -3.75637464 -2.58477118 -2.43423978 -1.32537563 



# Create a forecast for 10 segments ahead
MOR.VAR.pred = predict(MOR.VAR,n.ahead = 10)

# Forecasted Model grading using ASE
MORASE.VAR = mean((MORTest$cmort - 
                     MOR.VAR.pred$fcst$cmort[,1])^2)
MORASE.VAR
# [1] 26.52133

# Plotting Forecast vs actual for both VAR and MLP
plot(MOR$cmort[460:508], type = "l",
     main='Cardiac Death Rates',
     xlab="Week"
     ,ylab="Cardiac Deaths")
lines(seq(39,48,1),MOR.VAR.pred$fcst$cmort[,1],col = "red")
lines(seq(39,48,1),MORfore.mlp$mean,col = "green")
legend(20, 105, legend=c("MLP Forecast", "VAR Forecast",'Actual'),
       col=c("green", "red",'black'), lty=1, cex=0.9)







library(dplyr)
library(tidyr)
library(stats)

# Read a txt file
my_data <-read.delim(file.choose(), 
                     sep =";", header = TRUE, dec =".",stringsAsFactors=FALSE)

# Create a new variable 'datetime', combining Date and Time variables
my_data$datetime <- as.POSIXct(paste(my_data$Date, my_data$Time), format="%d/%m/%Y %H:%M:%S")

# Reducing dataset to variables we will use in the study
df <- my_data[,c('datetime','Global_active_power')]

# Removing data with missing observations 
df<-df[!(df$Global_active_power=="?"),] # 25979 missing observations

# Converting power consumption to numeric variable
df$Global_active_power <- as.numeric(df$Global_active_power)
summary(df)

plot(df[1:262080,'Global_active_power'],type='l',
     xlim=c(0,262080),
     ylim = c(min(df$Global_active_power),10),
     xlab = 'Time(min) Realization',col='blue',
     ylab = 'Power Consumption kW.min',
     main = 'Household Electric Power Consumption (kW.min)',
     cex.main=0.8)

#### Extra code for consolidating values by date-time  ####

library(dplyr)
library(plyr)
df$Month <- strftime(df$datetime, format="%Y/%m")
df$Year <- strftime(df$datetime, format="%Y")
df$Day <- strftime(df$datetime, format = "%Y/%m/%d")
df$hour <- strftime(df$datetime, format = "%Y/%m/%d %H")

# Reducing granularity of data to monthly values and summing the power consumption
df_monthly <- ddply(df, .(Month), summarize, monthly_sum=sum(Global_active_power))

plotts.wge(df_monthly$monthly_sum)
parzen.wge(df_monthly$monthly_sum, trunc = 14)

# Reducing granularity of data to daily values and summing the power consumption
df_daily <- ddply(df, .(Day), summarize, daily_sum=sum(Global_active_power))

# Removing any observations with NA values
sum(is.na(df_daily$date_num))
df_daily <- na.omit(df_daily)

# Plotting Daily Realization
plot(df_daily$daily_sum,type='l',
     xlim=c(0,length(df_daily$daily_sum)),
     ylim = c(min(df_daily$daily_sum),max(df_daily$daily_sum)),
     xlab = 'Time(Day) Realization',col='blue',
     ylab = 'Power Consumption kW.day',
     main = 'Household Electric Power Consumption (kW.day)',
     cex.main=0.8)

# Using first 46 months of data as training data ans using last
# 30 days data as test set
# Adding a counter variable for possible trend
df_daily$date_num = seq(1,length(df_daily$Day))
df_dailyTrain = df_daily[1:1403,]
df_dailyTest = df_daily[1404:1433,]
# Fitting MLP model to Daily data

set.seed(255)
Engfit.mlp= mlp(ts(df_daily$daily_sum[1:1403]), reps=10,allow.det.season = TRUE,
                comb='mean',xreg = data.frame(df_daily$date_num[1:1403]))
Engfit.mlp
    # MLP fit with 5 hidden nodes and 1 repetition.
    # Univariate lags: (1,3,4)
    # MSE: 184735.5048.

# Visualize the Neural Network
# Note: Grey input nodes are for univariate (3) lagged inputs
# 5 Hidden Nodes
dev.off()
plot(Engfit.mlp)


# Create a forecast for 10 segments ahead
ENGfore.mlp = forecast(Engfit.mlp, h = 30, 
                       xreg = data.frame(df_daily$date_num))
plot(ENGfore.mlp)

# Forecasted Model grading using ASE
ENGASE.mlp = mean((df_daily$daily_sum[1404:1433]-ENGfore.mlp$mean)^2)
ENGASE.mlp
# [1] 171550.9


# Plotting Forecast vs actual for MLP
plot(df_daily$daily_sum[1300:1433], type = "l",
     main='Household Energy Consumption Per Hour',
     xlab="Hour"
     ,ylab="Energy kw.hr")
lines(seq(104,133,1),ENGfore.mlp$mean,col = "red")
legend(20, 2750, legend=c("MLP Forecast", 'Actual'),
       col=c("red",'black'), lty=1, cex=0.9)




