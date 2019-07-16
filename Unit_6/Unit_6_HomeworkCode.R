#  MSDS 6373- Time Series Analysis - Unit 6 Homework Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 06/27/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 5)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

# This problem uses the global temperature data shown in Figure 1.24a and given in 
# handley
# a)	Plot the data
# b)	Using Least Squares method, fit a regression line Xt = a + bt + Zt.  
#     What are ahat and bhat for the fitted model? (You may find the base R function
#     lm to be helpful)
# c)	Find and plot Zt, the residuals from the least squares fit.
# d)	Does Zt have the appearance of stationary data? Discuss.
# e)	Find and plot the sample autocorrelations for Xt and Zt. Compare and contrast 
#     these two sets of sample autocorrelations.

# a)	Plot the data
data('hadley')
plotts.wge(hadley)

# b)	Using Least Squares method, fit a regression line Xt = a + bt + Zt.  
#     What are ahat and bhat for the fitted model? 
t = 1:length(hadley)
hadleylm = lm(hadley ~ t)
hadleylm$coefficients
    # (Intercept)            t 
    # -0.525737028  0.004437805

# c)	Find and plot Zt, the residuals from the least squares fit.
plotts.wge(hadleylm$residuals)

# e)	Find and plot the sample autocorrelations for Xt and Zt. Compare and contrast 
#     these two sets of sample autocorrelations.
acf_hadley = acf(hadley)

acf_resid = acf(hadleylm$residuals)


# Problem 5.2
# a)	Generate and plot four realizations of length n=150 from the AR(4) model in 
#     Equation 5.3.  Do any of these realizations have a ‘linear-trending’ behavior?
# b)	Generate and plot four realizations of length n=150 and with white noise
#     variance 0.01 from the model
#   Xt = -0.526 + 0.0044t + Zt where (1 – 0.614B + 0.044B2 – 0.077B3 – 0.206B4)Zt = 0
#  

# a)	Generate and plot four realizations of length n=150 from the AR(4) model in 
#     Equation 5.3.
phi5.3 = c(0.66,-0.02,0.10,0.24)
x1 = gen.arma.wge(n=150,phi = phi5.3)
x2 = gen.arma.wge(n=150,phi = phi5.3)
x3 = gen.arma.wge(n=150,phi = phi5.3)
x4 = gen.arma.wge(n=150,phi = phi5.3)


# b)	Generate and plot four realizations of length n=150 and with white noise
#     variance 0.01 from the model
#   Xt = -0.526 + 0.0044t + Zt where (1 – 0.614B + 0.044B2 – 0.077B3 – 0.206B4)Zt = 0
#  
phi52b = c(0.614, -0.44, 0.077,0.206)
x52_1 = gen.sigplusnoise.wge(n=150,b0=-0.526,b1=0.0044,phi = phi52b,vara = 0.01)
x52_2 = gen.sigplusnoise.wge(n=150,b0=-0.526,b1=0.0044,phi = phi52b,vara = 0.01)
x52_3 = gen.sigplusnoise.wge(n=150,b0=-0.526,b1=0.0044,phi = phi52b,vara = 0.01)
x52_4 = gen.sigplusnoise.wge(n=150,b0=-0.526,b1=0.0044,phi = phi52b,vara = 0.01)

factor.comp.wge(x52_1,p=4,ncomp = 4)
    # Coefficients of Original polynomial:  
    #   0.7485 -0.4313 0.1938 0.4091 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9650B              1.0363               0.9650       0.0000
    # 1-0.3109B+0.8041B^2    0.1933+-1.0983i      0.8967       0.2223
    # 1+0.5273B             -1.8965               0.5273       0.5000


# Problem 5.3
# Generate realizations of length n=200 from the following models:
#   a)	(1-1.2B + 0.8B2)(1-B)Xt = at
#   b)	(1 – 1.2B + 0.8B2)(1-B)2Xt = at
#   d) (1 – 1.2B + 0.8B2)(1 - B)2(1 - B4)(1 + B + B2)Xt = at

#   a)	(1-1.2B + 0.8B2)(1-B)Xt = at
xa = gen.arima.wge(n=200,phi = c(1.2,-0.8),d=1)
factor.wge(phi = c(1.2,-0.8))
  # Coefficients of Original polynomial:  
  #   1.2000 -0.8000 
  # Factor                 Roots                Abs Recip    System Freq 
  # 1-1.2000B+0.8000B^2    0.7500+-0.8292i      0.8944       0.1330


#   b)	(1 – 1.2B + 0.8B2)(1-B)2Xt = at
xb = gen.arima.wge(n=200,phi = c(1.2,-0.8),d=2)
factor.wge(phi = c(1.2,-0.8))
    # Coefficients of Original polynomial:  
    #   1.2000 -0.8000 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.2000B+0.8000B^2    0.7500+-0.8292i      0.8944       0.1330


#   d) (1 – 1.2B + 0.8B2)(1 - B)2(1 - B4)(1 + B + B2)Xt = at
xd = gen.aruma.wge(n=200,phi = c(1.2,-0.8),d=2,s=4,lambda = c(-1,-1))


# Problem 5.5
# For each of these four ARUMA(p,d,q) models, find p,d, and q.
# a)	(1 – 3B + 4.5B2 – 5B3 + 4B4 – 2B5 + 0.5B6)Xt = (1 – 1.7B + 0.8B2)at
# b)	(1 – 0.5B + 0.3B2 – 0.95B3 + 0.3B4 – 0.35B5 + 0.2B6)Xt = at
# c)	(1 – 1.5B + 1.3B2 + 0.35B3 – B4 + 1.35B5 – 0.7B6 + 0.4B7)Xt = (1 + 0.9B2)at
# d)	(1 – 0.5B – 0.5B2 – 0.5B4 + 0.5B6)Xt = (1- 0.81B2)at

# A)	(1 – 3B + 4.5B2 – 5B3 + 4B4 – 2B5 + 0.5B6)Xt = (1 – 1.7B + 0.8B2)at
factor.wge(phi = c(3,-4.5,5,-4,2,-0.5))
    # Coefficients of Original polynomial:  
    #   3.0000 -4.5000 5.0000 -4.0000 2.0000 -0.5000 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1+0.0000B+1.0000B^2    0.0000+-1.0000i      1.0000       0.2500
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1-1.0000B+0.5000B^2    1.0000+-1.0000i      0.7071       0.1250
factor.wge(phi = c(1.7,-0.8))
    # Coefficients of Original polynomial:  
    #   1.7000 -0.8000 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.7000B+0.8000B^2    1.0625+-0.3480i      0.8944       0.0504

# B)	(1 – 0.5B + 0.3B2 – 0.95B3 + 0.3B4 – 0.35B5 + 0.2B6)Xt = at
factor.wge(phi = c(0.5,-0.3,0.95,-0.3,0.35,-0.2))
    # Coefficients of Original polynomial:  
    #   0.5000 -0.3000 0.9500 -0.3000 0.3500 -0.2000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1+1.0000B+0.8000B^2   -0.6250+-0.9270i      0.8944       0.3444
    # 1+0.0000B+0.5000B^2    0.0000+-1.4142i      0.7071       0.2500
    # 1-0.5000B              2.0000               0.5000       0.0000

# D)	(1 – 0.5B – 0.5B2 – B4 + 0.5B5 + 0.5B6)Xt = (1- 0.81B2)at
factor.wge(phi = c(0.5,0.5,0,1,-0.5,-0.5))
    # Coefficients of Original polynomial:  
    #   0.5000 0.5000 0.0000 1.0000 -0.5000 -0.5000 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1+1.0000B             -1.0000               1.0000       0.5000
    # 1+0.0000B+1.0000B^2    0.0000+-1.0000i      1.0000       0.2500
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1+0.5000B             -2.0000               0.5000       0.5000
factor.wge(phi = c(0,0.81))
    # Coefficients of Original polynomial:  
    #   0.0000 0.8100 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+0.9000B             -1.1111               0.9000       0.5000
    # 1-0.9000B              1.1111               0.9000       0.0000


# Problem 5.7
# a)	Find and print the factor table for (1-B6)Xt = at.  What are the six roots
#     of unity?
# b)	Generate a realization of length n=120 from the seasonal model in (a).  
#     Describe any seasonal behavior you see in the realization.
# c)	Plot the realization in (b) along with the true autocorrelations and
#     true spectral density (use plots.true.wge()). Describe the behavior seen 
#     in the autocorrelations and spectral density.
# d)	Plot the realization in (b) along with the sample autocorrelations,
#     periodogram, and Parzen window spectral density estimates.  How do these 
#     plots compare with those from (c)?
#   

factor.wge(phi = c(0,0,0,0,0,1))
    # Coefficients of Original polynomial:  
    #   0.0000 0.0000 0.0000 0.0000 0.0000 1.0000 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+1.0000B             -1.0000               1.0000       0.5000
    # 1+1.0000B+1.0000B^2   -0.5000+-0.8660i      1.0000       0.3333
    # 1-1.0000B+1.0000B^2    0.5000+-0.8660i      1.0000       0.1667
    # 1-1.0000B              1.0000               1.0000       0.0000

s6 = plotts.true.wge(n=120,phi = c(0,0,0,0,0,0.99999)) #Alternate gen.aruma.wge(n=120,s=6)

plotts.sample.wge(s6$data)