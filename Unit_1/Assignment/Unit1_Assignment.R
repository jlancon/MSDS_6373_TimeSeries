#  MSDS 6373- Time Series Analysis - Section 1

#  Team Member:  Jeffery Lancon
#  
#  Date: 04/28/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 1)


library(tswge)
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


par(mfrow=c(2,1))
acf(df$Global_active_power[1:1000],lag.max = 20, plot=TRUE, ylim = c(-1,1),col="blue")
acf(df$Global_active_power[90001:91000],lag.max = 20, plot=TRUE, ylim = c(-1,1),col="red")


acf(df$Global_active_power,lag.max = 2000000,plot=TRUE, ylim = c(-1,1),col="blue")

z <- acf(df$Global_active_power,lag.max = 2000000,plot=FALSE)

z$acf[1:13]
# Get rid of the first element (i.e. lag 0)
z$acf[2:13]
# Plot the autocorrelation function without lag 0
par(xaxp  = c(2,2000000, 10))



plot(z$acf[2:2000000], 
     type="h", 
     main="Autocorrelation Function", 
     xlab="Lag",
     col='blue',
     ylab="ACF", 
     ylim=c(-0.2,0.45), # this sets the y scale to -0.2 to 0.2
     las=1,
     xaxp  = c(2,2000000, 10),
     xaxt="n")
abline(h=0)

# Save an object to a file
saveRDS(z, file = "C:/Users/Prodigy/Documents/Personal Info/SMU/MSDS 6373 - Time Series/Unit 1/Async Materials/my_acf_data.rds")
# Restore the object
readRDS(file = "my_data.rds")




cc <-data("fig1.5")
plotts.wge(fig1.5)


# Add labels to the x-axis
x <- c(1:12)
y <- c(1:12)
axis(1, at=x, labels=y)

?acf()
par(mfrow=c(1,1))
plotts.wge(df$Global_active_power[1:525600])


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