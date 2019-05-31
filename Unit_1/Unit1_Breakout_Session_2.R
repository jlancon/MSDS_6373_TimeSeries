#  MSDS 6373- Time Series Analysis - Section 1

#  
#  Date: 05/07/2019
#  Breakout Session 2: Unit 1: 

# Consider the very small (assume stationary) time series:  4,6,7,5,3
# a) What is your best estimate of the mean or the series?
# b) What is your best estimate of the variance of the series: gamma hat0? 
# c) What is your best estimate of the: rho1?
# d) What is your best estimate of the variance of the sample mean?
# e) Provide a 95% confidence interval for the mean of the series.

# Enterning Timeseries data
ts <- c(4,6,7,5,3)

# a) Determining Mean of timeseries
print (paste0("Mean of Timeseries: ",mean(ts)))
  # [1] "Mean of Timeseries: 5"

# b) What is your best estimate of the variance of the series: gamma_hat0
n = length(ts)
gamma0 <- var(ts)*(n-1)/n
print (paste0("variance gamma_hat0: ",gamma0))
  # [1] "variance gamma_hat0: 2"
var(ts) #Note variance is divided by n-1

# c) What is the estimate of: rho1 
rhos <- acf(ts,plot = FALSE,lag.max = n-1)
  # Autocorrelations of series 'ts', by lag
  # 
  # 0    1    2    3    4 
  # 1.0  0.1 -0.6 -0.2  0.2
print(rhos$acf[1]) #[1] 1
print(rhos$acf[2]) #[1] 0.1

# d) What is your best estimate of the variance of the sample mean?

nlag=n-1
sum=0
for (k in 1:nlag) {
  sum=sum+(1-(k/n))*rhos$acf[k+1]*gamma0 #k+1 is due to the indexing of acf function
  }
vxbar=2*sum/n+gamma0/n 
vxbar 
print(paste0("Variance of Xbar: ",vxbar))
  # [1] "Variance of Xbar: 0.144"

# 95% Confidence Interval for variance of dataset
MOE = 1.96*sqrt(vxbar)
LL = mean(ts) - MOE
UL = mean(ts) + MOE

print(paste0("Lower Limit: ",LL)) #[1] "Lower Limit: 4.2562322943284"
print(paste0("Upper Limit: ",UL)) #[1] "Upper Limit: 5.7437677056716"

#-----------------------------
# ALternate way to derive value for vxbar -- More closely follows formula in text
sumAlt=0
for (k in 1:nlag) { # Follows the book formula layout
  sumAlt=sumAlt+(1-(k/n))*rhos$acf[k+1] # acf[k+1] due to indexing of values acf starts at 0
                                        # rhos0 = acf[1]; rhos1 = acf[2]; R indexes from 1
}
vxbar_Alt=(1+ 2*sumAlt)*(gamma0/n) 
print(paste0("vxbar_Alt value: ",vxbar_Alt))
# [1] "vxbar_Alt value: 0.144"

