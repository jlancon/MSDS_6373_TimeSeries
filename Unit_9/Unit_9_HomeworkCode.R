#  MSDS 6373- Time Series Analysis - Unit 9 Homework Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 07/02/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 7)


library(tswge)

# Using the Texas gas price data (TexasGas.csv), we found that an 
# AR(2) was the model suggested by both the AIC and BIC. We would now
# like to compare the fit from using the maximum likelihood and Burg estimates. 

# Using at least one slide per question
# 
# Fit an AR(2) to the data using the maximum likelihood estimates like you did in the Concept Check question.
# Fit and AR(2) to the data using the Burg estimates.  Display and describe.
# Find the ASE for the maximum likelihood fit by forecasting the last 24 weeks of the series.
# Find the ASE for the Burg fit by forecasting the last 24 weeks of the series.
# Which model would you choose?


# Determine model and forecast next 8 months of data
Gas = read.csv(file.choose(),header=TRUE)
plotts.sample.wge(Gas$Price)

#Use aic5.wge() or aic.wge() to identify estimates of p and q
aic5.wge(Gas$Price) #AIC Model
# Five Smallest Values of  aic 
#       p    q        aic
# 7     2    0  -6.131525
# 8     2    1  -6.124641
# 10    3    0  -6.123937
# 9     2    2  -6.116413
# 13    4    0  -6.116182
aic5.wge(Gas$Price,type = 'bic')
# Five Smallest Values of  bic 
#       p    q        bic
# 7     2    0  -6.082895
# 8     2    1  -6.059802
# 10    3    0  -6.059098
# 5     1    1  -6.049422
# 6     1    2  -6.049014

# Fit an AR(2) to the data using the maximum likelihood estimates
estGas_mle = est.ar.wge(Gas$Price,p= 2,type='mle')
    # Coefficients of Original polynomial:  
    #   1.3812 -0.4077 
    # Factor          Roots     Abs Recip    System Freq 
    # 1-0.9536B       1.0487    0.9536       0.0000
    # 1-0.4276B       2.3386    0.4276       0.0000

# Forecasting last 24 wks of data
Gas_mle.fore = fore.arma.wge(Gas$Price,phi = estGas_mle$phi, n.ahead = 24,
              lastn = TRUE, plot = TRUE,limits = FALSE )

# Mean of Residuals (MSE)
n = length(Gas$Price)
MSE.fore_mle = mean((Gas_mle.fore$f-Gas$Price[(n-23):n])^2)
print(MSE.fore_mle)
  # [1] 0.01461187

########################################################
# Fit an AR(2) to the data using the Burg estimates
estGas_burg = est.ar.wge(Gas$Price,p= 2,type='burg')
    # Coefficients of Original polynomial:  
    #   1.3814 -0.4058 
    # Factor        Roots    Abs Recip    System Freq 
    # 1-0.9575B     1.0444   0.9575       0.0000
    # 1-0.4239B     2.3592   0.4239       0.0000


# Forecasting last 24 wks of data
Gas_burg.fore = fore.arma.wge(Gas$Price,phi = estGas_burg$phi, n.ahead = 24,
                             lastn = TRUE, plot = TRUE,limits = FALSE )

# Mean of Residuals (MSE)
n = length(Gas$Price)
MSE.fore_burg = mean((Gas_burg.fore$f-Gas$Price[(n-23):n])^2)
print(MSE.fore_burg)
# [1] 0.01309828







