#  MSDS 6373- Time Series Analysis - Unit 5 Async Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 06/04/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 3)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

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