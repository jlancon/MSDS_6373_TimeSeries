#  MSDS 6373- Time Series Analysis - Unit 7 Homework Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 06/28/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 6)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))


# Problem 6.1
# A realization of length n=10 from the ARMA(4,2) model
# (Xt-µ) + 1.1(Xt-1 - µ) + 1.78(Xt-2 - µ) + 0.88(Xt-3 - µ) + 0.64(Xt-4 - µ) = at – 0.2at-1 + 0.9at-2
# Where µ = 40 is given in the following table.
# t	Xt	t	Xt
# 1	40.0	6	48.7
# 2	30.5	7	39.2
# 3	49.8	8	31.7
# 4	38.3	9	46.1
# 5	29.3	10	42.4
# a.	By hand (calculator)
# i)	Obtain forecasts Xhat10(l), l = 1,2,3,4 using Equation 6.24
# ii)	Find 95% prediction intervals for forecasts in (a)



Pro61 = c(40,30.5,49.8,38.3,29.3,48.7,39.2,31.7,46.1,42.4)
p = c(-1.1,-1.78,-0.88,-0.64)
q = c(0.2,-0.9)

Pro61_Predict = fore.arma.wge(Pro61,phi = p,
                              theta = q, n.ahead = 4,lastn = FALSE,plot = TRUE,
                              limits = TRUE)
    # > Pro61_Predict
    # $f
    # [1] 32.15921 44.30143 41.04902 34.39342
    # 
    # $ll
    # [1] 28.78304 38.76410 35.20864 27.91702
    # 
    # $ul
    # [1] 35.53539 49.83877 46.88941 40.86981
    # 
    # $resid
    # [1]  0.0000000  0.0000000  0.0000000  0.0000000 -1.3260000 -1.6572000
    # [7] -2.4780400 -1.0421280  0.5358104  2.4050773
    # 
    # $wnv
    # [1] 2.967132
    # 
    # $se
    # [1] 1.722536 2.825170 2.979789 3.304282
    # 
    # $psi
    # [1] -1.3000  0.5500  0.8290 -1.3869

# Find the first 5 psi weights for the model
psi.weights.wge(phi = c(-1.1,-1.78,-0.88,-0.64),theta = c(0.2,-0.9),lag.max = 5)

n = length(Pro61)
orderp = length(p)
orderq = length(q)


sigma_aHat = sqrt((sum((Pro61_Predict$resid)^2))/(n-orderp))
print(paste0('sigma_aHat =',sigma_aHat))



# Problem 6.3.c
# Using gen.arma.wge(), after calling mult.wge, generate a 
#realization of length n=100 from the ARMA(3,1) model.
# (1-0.8B)(1-B+0.9B2)(Xt-50) = (1+0.5)at
# c.	Forecast the last 10 values of the series 
# (using t0 = 90 as the forecast origin) and discuss the behavior
# of the forecasts and how well forecasts perform.

mod = mult.wge(fac1 = c(0.8),fac2 = c(1,-0.9))
    # $char.poly
    # 1 - 1.8*x + 1.7*x^2 - 0.72*x^3 
    # 
    # $model.coef
    # [1]  1.80 -1.70  0.72
model63 = gen.arma.wge(n=100,phi = mod$model.coef,theta = c(-0.5))
model63 = model63 + 50

model63_fore = fore.arma.wge(model63,phi = mod$model.coef,theta = c(-0.5),n.ahead = 10,
              lastn = TRUE)
    # $f
    # [1] 50.92279 50.36148 48.79577 47.95388 48.69604 50.33585 51.41964
    # [8] 51.11718 49.91094 49.03423
    # 
    # $ll
    # [1] 48.75411 44.92246 41.20737 39.93023 40.64085 42.05881 43.13568
    # [8] 42.56785 40.84735 39.83389
    # 
    # $ul
    # [1] 53.09147 55.80050 56.38416 55.97752 56.75124 58.61289 59.70361
    # [8] 59.66651 58.97452 58.23458
    # $wnv
    # [1] 1.224274
    # 
    # $se
    # [1] 1.106469 2.775010 3.871630 4.093697 4.109792 4.222980 4.226514
    # [8] 4.361903 4.624279 4.694053
    # 
    # $psi
    # [1]  2.3000000  2.4400000  1.2020000 -0.3284000 -0.8777200 -0.1561760
    # [7]  0.9745592  1.3877474  0.7287479 -0.3457417
    # $resid
    # [91]  2.136445922 -1.141278829 -2.350129756 -1.378926333  1.291495306
    # [96]  1.480672155 -0.133998873 -0.673469732 -0.331561917 -0.946005680

for (z in seq(1,(10))){
  if (z == 1){print(" Forecast    Lower    Upper     Actual") }
    print(paste0(round(model63_fore$f[z],4),'   ',round(model63_fore$ll[z],4),'   ',round(model63_fore$ul[z],4),'   ',round(model63[(90+z)],4)))
}
    # [1] " Forecast    Lower   Upper   Actual"
    # [1] "50.9228   48.7541   53.0915   53.0592"
    # [1] "50.3615   44.9225   55.8005   54.134"
    # [1] "48.7958   41.2074   56.3842   49.0336"
    # [1] "47.9539   39.9302   55.9775   40.9529"
    # [1] "48.696   40.6409   56.7512   39.0083"
    # [1] "50.3358   42.0588   58.6129   47.0971"
    # [1] "51.4196   43.1357   59.7036   57.6248"
    # [1] "51.1172   42.5678   59.6665   60.0767"
    # [1] "49.9109   40.8473   58.9745   52.489"
    # [1] "49.0342   39.8339   58.2346   41.7996"




# Problem 6.4
# Generate a realization of length n=100 from the ARUMA(1,5,0) model
# (1-B)(1-B4)(1+0.5B)(Xt – 50) = at
# Do the following:
#   a)	Forecast the next 10 steps beyond the end of the series.
# b)	Forecast the last 10 values of the series (using t0 = 90 as the forecast origin) and describe the behavior of the forecasts and how well the forecasts perform.
# c)	Redo part (b) with this time showing the 95% probability limits.  How many of the actual last 10 values stay within these limits?
#   

model64 = gen.aruma.wge(n=100,phi = c(-0.5),s=4,d=1,sn=2)
model64 = model64 + 50
plotts.wge(model64)

model64_forward = fore.aruma.wge(model64,phi = c(-0.5),s=4,d=1,n.ahead = 10,
                             lastn = FALSE, limits = FALSE)

model64_last = fore.aruma.wge(model64,phi = c(-0.5),s=4,d=1,n.ahead = 10,
                                 lastn = TRUE, limits = FALSE)


for (z in seq(1,(10))){
  if (z == 1){print(" Forecast    Lower    Upper     Actual") }
  print(paste0(round(model64_last$f[z],4),'   ',round(model64_last$ll[z],4),'   ',round(model64_last$ul[z],4),'   ',round(model64[(90+z)],4)))
}

    # [1] " Forecast    Lower    Upper     Actual"
    # [1] "124.1524   122.1856   126.1193   123.1859"
    # [1] "116.5927   114.3937   118.7917   116.4932"
    # [1] "131.4651   128.8171   134.113   130.8102"
    # [1] "119.5763   116.6569   122.4957   118.8486"
    # [1] "124.6726   120.2524   129.0929   123.6143"
    # [1] "117.0889   112.1179   122.0598   116.8109"
    # [1] "131.9733   126.2696   137.6769   132.2094"
    # [1] "120.0785   113.8367   126.3203   119.1826"
    # [1] "125.1778   117.4046   132.9511   126.2771"
    # [1] "117.5926   109.0329   126.1523   116.8752"






fore.arma
x = Pro61
x0 = 5
k=1

a = x[x0] - 
sum=0
for (z in seq(1,orderp)){
  sum = sum + (p[z]*x[(orderp+1-z)])
}


  (p[k]*x[x0-1] + p[k+1]*x[x0-2] + p[k+2]*x[x0-3] + p[k+3]*x[x0-4])

# Find the 95% probability interval for the l=3, forecast Xhat(3)
LiveSession = c(5,8,9,8,7,6,4,3)
LiveSession_Predict = fore.arma.wge(LiveSession,phi = c(1.7,-0.72),
                                    n.ahead = 8,lastn = FALSE,plot = TRUE,
                                    limits = TRUE)



sigma_aHat = sqrt((sum((LiveSession_Predict$resid)^2))/6)
print(paste0('sigma_aHat =',sigma_aHat))












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