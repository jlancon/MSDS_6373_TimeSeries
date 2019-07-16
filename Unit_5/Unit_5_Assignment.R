#  MSDS 6373- Time Series Analysis - Unit 5 Assignment Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 06/04/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 3)


library(tswge)
library(dplyr)
library(tidyr)
library(stats)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))


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

# AIC5 on power consumption data - minute ganularity
#plotts.wge(df$Global_active_power)
#plotts.sample.wge(df$Global_active_power)
aic5.wge(df$Global_active_power)
    # ---------WORKING... PLEASE WAIT... 
    # 
    # 
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 12    3    2  -2.677994
    # 17    5    1  -2.677597
    # 16    5    0  -2.677395
    # 18    5    2  -2.677279
    # 14    4    1  -2.677269


#Walmart Store 9 Item 50 Filtering / spectral analysis / AR(3)
# Read in the data
Walmart = read.csv(file.choose(),header = TRUE)

# Load the Data
Stor9Item50 = Walmart %>% filter(item == 50 & store == 9) #1826 obs 5 variables

#Look at and Visualize the data
head(Stor9Item50)
plotts.sample.wge(Stor9Item50$sales)

# AIC5 Model for Walmart data Store 9 Item 50
aic5.wge(Stor9Item50$sales)
    # ---------WORKING... PLEASE WAIT... 
    # 
    # 
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 15    4    2   4.991650
    # 17    5    1   5.009865
    # 18    5    2   5.021469
    # 14    4    1   5.073902
    # 12    3    2   5.114417

# Generate the realization from an ARMA model
# You pick the p & q
# Equation: X_t - 0.90X_t-1 + 0.20X_t-2 - 0.70X_t-3 + 0.30X_t-4 
#           + 0.20X_t-5 = a_t + 0.53a_t-1 - 0.38a_t-2 - 0.60_t-3
plotts.true.wge(phi=c(0.90, -0.20, 0.70, -0.30,-.2),theta=c(-0.53, 0.38, 0.6))

factor.wge(phi=c(0.90, -0.20, 0.70, -0.30,-.2))
factor.wge(phi=c(-0.53, 0.38, 0.6))


# Use AIC 5 to identify the top five quality models with respect to AIC
# for the airline cancellation data from the attached data set. 
# Comment on which are AR, MA, and ARMA.
SWA = read.csv(file.choose(),header=TRUE)
plotts.wge(SWA$arr_cancelled)
plotts.sample.wge(SWA$arr_cancelled)
aic5.wge(SWA$arr_cancelled)






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



## Weekly summary of data ####
library(tidyverse)
library(magrittr)
library(lubridate)
df_wk <- df %>% group_by(Year,week = isoweek(datetime)) %>% summarise(weekly_sum = sum(Global_active_power))


?isoweek()

df %>%
  group_by(Year, Month) %>%
  summarise(Rain = sum(Rain))






















# Section / Async Video 5.3.1 Generating MA(1) Data Slides 17,18
# Equation: Xt = at-0.9at_1 + 0.4at_2
gen.arma.wge(n=100,theta=-0.99)
gen.arma.wge(n=100,theta=0.99)
gen.arma.wge(n=100,theta=-0.99,sn=5)
gen.arma.wge(n=100,theta=0.99,sn=5)
plotts.true.wge(theta = c(.99))
plotts.true.wge(theta = c(-.99))

# Generating MA(2) data from Example 5.2.3
# Equation: Xt = at-0.9at_1 + 0.4at_2
gen.arma.wge(n=100,theta = c(0.9,-0.4))
plotts.true.wge(theta = c(0.9,-0.4))

# Section / Async Video 5.3.4 Generating AR(2) & MA(2) Data Slides 20,21
# AR(2): X_t - 1.1X_t-1 + 0.9X_t-2 = a_t
plotts.true.wge(phi=c(1.1,-0.9))

# MA(2): X_t = a_t- 1.1X_t-1 + 0.9X_t-2
plotts.true.wge(theta=c(1.1,-0.9))

plotts.true.wge(theta=c(-.1,0.3))


# Section / Async Video 5.4.3 Invertibility Data Slides 31,32,33
 # NOTE: we are still using phi when using factor.wge.  This is not
 #100% correct but all we are doing is using the factor formulas to
 # find the roots of a theta variable.

# Equation: MA(2) Xt = a_t- 1.6a_t-1 + 0.9a_t-2
factor.wge(phi=c(1.6,-0.9))
    # Coefficients of Original polynomial:  
    #   1.6000 -0.9000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.6000B+0.9000B^2    0.8889+-0.5666i      0.9487       0.0903


# Equation: MA(2) Xt = a_t- 1.6a_t-1 - 0.9a_t-2
factor.wge(phi=c(1.6,0.9))
  # Coefficients of Original polynomial:  
  #   1.6000 0.9000 
  # 
  # Factor                 Roots                Abs Recip    System Freq 
  # 1-2.0410B              0.4900               2.0410       0.0000
  # 1+0.4410B             -2.2677               0.4410       0.5000

# Section / Async Video 5.4.4 Concept Check
# Equation: MA(2) Xt = a_t + 0.1a_t-1 - 0.3a_t-2
factor.wge(phi=c(-0.1,0.3))

# Section / Async Video 5.5.4 Concept Check
# Equation: ARMA(3,1) X_t + 0.1X_t-1 + 0.82X_t-2 - 0.16X_t-3= a_t + 0.2a_t-1
factor.wge(phi=c(-0.1,-0.82,0.16))
factor.wge(phi=c(-0.2))


# Section / Async Video 5.5.8 ARMA(p,q) Properties Slides 52,53

# AR factors
plotts.true.wge(phi=c(0.3, 0.9, 0.1, -0.8075))
factor.wge(phi=c(0.3, 0.9, 0.1, -0.8075))

# MA factors
plotts.true.wge(theta=c(-0.9, -0.8, -0.72))
factor.wge(phi=c(-0.9, -0.8, -0.72))

# ARMA
plotts.true.wge(phi=c(0.3, 0.9, 0.1, -0.8075),theta=c(-0.9, -0.8, -0.72))


# Section / Async Video 5.5.10 Concept Check
# Equation: MA(2) X_t - 0.1X_t-1 + 0.5X_t-2 - 0.7X_t-3 = a_t - 0.72_t-1 + 0.8_t-2
plotts.true.wge(phi=c(0.1, -0.5, 0.7),theta=c(0.72, -0.8))

# Section / Async Video 5.6.2 ARMA(p,q) Examples-Canadian Lynx Slides 59-61
data("llynx")
plotts.sample.wge(llynx)

aic5.wge(llynx)
  # Five Smallest Values of  aic 
  # p    q        aic
  # 14    4    1  -2.951518
  # 16    5    0  -2.951301
  # 13    4    0  -2.949515
  # 12    3    2  -2.942965
  # 17    5    1  -2.936873

# Section / Async Video 5.6.3 ARMA(p,q) Examples-SW Airline Delays Slides 63-64
SWA = read.csv(file.choose(),header=TRUE)
plotts.wge(SWA$arr_delay)
plotts.sample.wge(SWA$arr_delay)
aic5.wge(SWA$arr_delay)
    # Error in aic calculation at 4 2 
    # Five Smallest Values of  aic 
    # p    q        aic
    # 14    4    1   19.00136
    # 17    5    1   19.01266
    # 18    5    2   19.02307
    # 4     1    0   19.03069
    # 8     2    1   19.04178

# Section / Async Video 5.6.4 Concept Check SW Airline Weather - Which model is best
#SWA = read.csv(file.choose(),header=TRUE)
plotts.wge(SWA$weather_delay)
plotts.sample.wge(SWA$weather_delay)
aic5.wge(SWA$weather_delay)
    # Five Smallest Values of  aic 
    # p    q        aic
    # 14    4    1   14.90084
    # 13    4    0   14.90143
    # 15    4    2   14.90365
    # 4     1    0   14.90685
    # 11    3    1   14.90944

# Section / Async Video 5.7.2 ARMA(p,q) psi weights Slides 69-71

# psi-weights for simple MA(1) model X(t)=(1-.8B)a(t)
psi.weights.wge(theta=.8,lag.max=5)

# psi-weights for simple AR(1) model (1-.8B)X(t)=a(t)
psi.weights.wge(phi=.8,lag.max=5) #note that psi(j)=.8j

# psi-weights for ARMA(2,1) model (1-1.2B+.6B2)X(t)=(1-.5B)a(t)
psi.weights.wge(phi=c(1.2,-.6),theta=c(.5),lag.max=5)


# Section / Async Video 5.7.3 Concept Check psi weight
#X_t - 1.95X_t-1 + 1.9X_t-2 = a_t
psi.weights.wge(phi=c(1.95,-1.9),lag.max=5)