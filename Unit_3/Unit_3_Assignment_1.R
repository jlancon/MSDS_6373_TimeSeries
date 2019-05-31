#  MSDS 6373- Time Series Analysis - Section 3

#  Team Member:  Jeffery Lancon
#  
#  Date: 05/21/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 1)
#  Take the Walmart data and do a five-point moving average and 51-point moving average.
#  Show the spectral density for each. Create a slide in your PowerPoint document for the
#  live session. 

library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf
library(dplyr)
library(tidyr)
library(stats)

# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

#Walmart Store 8Item 1 Filtering / spectral analysis / AR(3)
# Read in the data
Walmart = read.csv(file.choose(),header = TRUE)

# Load the Data
Stor8Item1 = Walmart %>% filter(item == 1 & store == 8) #1826 obs 5 variables

#Look at and Visualize the data
head(Stor8Item1)
plotts.wge(Stor8Item1$sales)


# Create a 5-point moving average
ma_5 = stats::filter(Stor8Item1$sales, rep(1,5))/5
plotts.wge(ma_5)

#head(Stor8Item1$sales)
#head(ma_5)

# Removing NA values from variable ma_5
ma_5 <- na.omit(ma_5, inplace=TRUE)
#Create spectral density for 5-pt moving average data set.
parzen.wge(ma_5, trunc = 300)

###### ------------- ######
# Create a 51-point moving average
ma_51 = stats::filter(Stor8Item1$sales, rep(1,51))/51
plotts.wge(ma_51)

#head(Stor8Item1$sales)
#head(ma_51)

# Removing NA values from variable ma_51
ma_51 <- na.omit(ma_51, inplace=TRUE)
#Create spectral density for 51-pt moving average data set.
parzen.wge(ma_51, trunc = 150)


butterworth.wge(x,order=10,type='low',cutoff=0.2,plot=TRUE)