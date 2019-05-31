
library(tswge)
# Chapter 1 - Problem 1.1
ts <- c(76,70,66,60,70,72,76,80)
mean(ts)
n = length(ts)
gamma0 = var(ts)*((n-1)/n)
  # [1] 34.9375

rhos <- acf(ts,plot = FALSE,lag.max = n-1)
  # Autocorrelations of series 'ts', by lag
  # 
  # 0      1      2      3      4      5      6      7 
  # 1.000  0.422 -0.043 -0.430 -0.466 -0.173  0.042  0.149 
print(rhos$acf[1]) #[1] 1
print(rhos$acf[2]) #[1] 0.4219589

# Chapter 1 - Problem 1.2
data(wtcrude)
plotts.sample.wge(wtcrude)

data(patemp)
plotts.sample.wge(patemp)

# Chapter 1 - Problem 1.6

# Time series with B: 2pi(0.025)  Period: 1/0.025 Frequency: 0.025
t= seq(1,100,length=100)
y1 = cos(2*pi*0.05*t)
y2 = cos(2*pi*0.35*t+2)
y3 = runif(100, min=0, max=1)
# Time Series with multiple periods/frequencies
ysum=3*y1+1.5*y2+y3
plotts.sample.wge(ysum)

# Alternate code from Bivin
x = gen.sigplusnoise.wge(n=100,coef=c(3,1.5),freq=c(0.05,0.35),psi=c(0,2))
plotts.sample.wge(x)
