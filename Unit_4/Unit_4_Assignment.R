#  MSDS 6373- Time Series Analysis - Section 4

#  Team Member:  Jeffery Lancon
#  
#  Date: 05/28/2019
#  Case Study: 
#  For Live Session Consider the WalMart Data Given here. This
# is data for Store 9 and Item 50.
# Create 1 or 2 slides in a PP presentation to indicate which model
# you feel is the most appropriate in modeling this time series and why.
# Not only indicate why you chose the model you did, but why you didn't choose
# the others.
# Four Models:
# (a) X_t - 0.967Xt_1 = a_t
# (b) X_t - 1.452Xt_1 + 0.453Xt_2 + 0.294Xt_3 - 0.175Xt_4 - 0.237Xt_5 + 0.154Xt_6 = a_t
# (c) X_t - 1.445Xt_1 + 0.411Xt_2 + 0.038Xt_3 - 0.170Xt_4 - 0.362Xt_5 + 0.245Xt_6 + 0.177Xt_7 - 0.213Xt_8 = a_t
# (d) X_t - 1.384Xt_1 + 0.359Xt_2 + 0.309Xt_3 - 0.063Xt_4 - 0.317Xt_5 + 0.140Xt_6 + 0.0587Xt_7 + 0.199Xt_8 - 0.2877Xt_9 = a_t
# 


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf
library(dplyr)
library(tidyr)
library(stats)

# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

#Walmart Store 9 Item 50 Filtering / spectral analysis / AR(3)
# Read in the data
Walmart = read.csv(file.choose(),header = TRUE)

# Load the Data
Stor9Item50 = Walmart %>% filter(item == 50 & store == 9) #1826 obs 5 variables

#Look at and Visualize the data
head(Stor9Item50)
plotts.sample.wge(Stor9Item50$sales)

# Zero Mean Data Visualization
Stor9Item50$ZM_sales = Stor9Item50$sales-mean(Stor9Item50$sales)
plotts.sample.wge(Stor9Item50$ZM_sales)

# Look at first Model - Model (a)
# (a) X_t - 0.967Xt_1 = a_t

factor.wge(phi=c(0.967))
    # Coefficients of Original polynomial:  
    #   0.9670 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9670B              1.0341               0.9670       0.0000

plotts.true.wge(n=length(Stor9Item50$sales),phi=c(0.967))

#############################################################################
# Look at second Model - Model (b)
# (b) X_t - 1.452Xt_1 + 0.453Xt_2 + 0.294Xt_3 - 0.175Xt_4 - 0.237Xt_5 + 0.154Xt_6 = a_t

factor.wge(phi=c(1.452,-0.453,-0.294,0.175,0.237,-0.154))
    # Coefficients of Original polynomial:  
    #   1.4520 -0.4530 -0.2940 0.1750 0.2370 -0.1540 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9428B              1.0607               0.9428       0.0000
    # 1-0.9484B+0.6428B^2    0.7377+-1.0057i      0.8017       0.1493
    # 1+1.0620B+0.4080B^2   -1.3014+-0.8702i      0.6388       0.4062
    # 1-0.6228B              1.6057               0.6228       0.0000

plotts.true.wge(n=length(Stor9Item50$sales),phi=c(1.452,-0.453,-0.294,0.175,0.237,-0.154))

#############################################################################
# Look at second Model - Model (c)
# (c) X_t - 1.445Xt_1 + 0.411Xt_2 + 0.038Xt_3 - 0.170Xt_4 - 0.362Xt_5 + 0.245Xt_6 + 0.177Xt_7 - 0.213Xt_8 = a_t

factor.wge(phi=c(1.445,-0.411,-0.038,0.170,0.362,-0.245,-0.177,0.213))
    # Coefficients of Original polynomial:  
    #   1.4450 -0.4110 -0.0380 0.1700 0.3620 -0.2450 -0.1770 0.2130 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.2456B              0.8028               1.2456       0.0000
    # 1-0.6653B+0.8160B^2    0.4077+-1.0292i      0.9033       0.1900
    # 1+1.0631B+0.6139B^2   -0.8658+-0.9376i      0.7835       0.3687
    # 1-1.2609B+0.5144B^2    1.2257+-0.6647i      0.7172       0.0791
    # 1+0.6636B             -1.5069               0.6636       0.5000


plotts.true.wge(n=length(Stor9Item50$sales),phi=c(1.445,-0.411,-0.038,0.170,0.362,-0.245,-0.177,0.213))
# Non-stationary

#############################################################################
# Look at second Model - Model (d)
# (d) X_t - 1.384Xt_1 + 0.359Xt_2 + 0.309Xt_3 - 0.063Xt_4 - 0.317Xt_5 + 0.140Xt_6 + 0.0587Xt_7 + 0.199Xt_8 - 0.2877Xt_9 = a_t

factor.wge(phi=c(1.384,-0.359,-0.309,0.063,0.317,-0.140,-0.0587,-0.199,0.2877))
    # Coefficients of Original polynomial:  
    #   1.3840 -0.3590 -0.3090 0.0630 0.3170 -0.1400 -0.0587 -0.1990 0.2877 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9898B              1.0103               0.9898       0.0000
    # 1-0.8851B+0.8545B^2    0.5179+-0.9497i      0.9244       0.1705
    # 1-1.6496B+0.8498B^2    0.9706+-0.4845i      0.9218       0.0737
    # 1+1.4991B+0.6791B^2   -1.1037+-0.5044i      0.8241       0.4318
    # 1+0.6415B+0.5894B^2   -0.5442+-1.1834i      0.7677       0.3186


plotts.true.wge(n=length(Stor9Item50$sales),phi=c(1.384,-0.359,-0.309,0.063,0.317,-0.140,-0.0587,-0.199,0.2877))


##############################
# Factoring Characteristic Equation (by hand model)

# x_t + 0.5X_t-1 + 0.6X_t-2 = a_t

a = 0.6 # Squared term - note signs
b = 0.5 # Single term - note signs
c = 1  #no term - note signs

if ((b^2-(4*a*c)) < 0){
  r1a = -b/(2*a) 
  r1b = ((sqrt(abs(b^2-(4*a*c))))/(2*a))
  print(paste0('complex root: ',round(r1a,4),' ± ', round(r1b,4),'i'))
  Modulus = sqrt(r1a^2+r1b^2)
  print(paste0('modulus |r| = ',round(Modulus,4),' Stationary if greater than 1'))
  print(paste0('Absolute reciprical: ',1/Modulus))

}else{
  r1a = (-b+(sqrt(b^2-(4*a*c))))/(2*a)
  r1b = (-b-(sqrt(b^2-(4*a*c))))/(2*a)
  print(paste0('NOT-complex root: ',round((r1a),4),' and ',round((r1b),4)))
}


# Calculating the Frequency
phi1 = -0.5 # Note the signs of the variables
phi2 = -0.6 # Note the signs of the variables

f0 = (1/(2*pi))*acos((phi1/(2*sqrt(-phi2))))
print(paste0('Frequency of complex conjugate root: ',round(f0,4)))