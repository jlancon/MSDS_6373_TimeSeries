#  MSDS 6373- Time Series Analysis - Unit 7 Async Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 06/18/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 1)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

#----Section 7.4.9 AR(p) Predictions ---- Slide 39 - 41
# Generating Predictions using tswge

#AR(1) phi Positive
data("fig6.1nf")
plotts.wge(fig6.1nf)

#Forecast
fore.arma.wge(fig6.1nf,phi = 0.8,n.ahead = 20,plot = TRUE,limits = FALSE)


#AR(1) phi Negative
x1 = gen.arma.wge(100,phi = -0.8)

#Forecast
fore.arma.wge(x1,phi = -0.8,n.ahead = 20,plot = TRUE,limits = FALSE)


#AR(2) Model
x2 = gen.arma.wge(n=75,phi = c(1.6,-0.8),sn=24)
x2 = x2+25  # Since gen.arma.wge creates a model with Xbar = 0,
            # we add a mu of 25 to data
plotts.wge(x2)

fore.arma.wge(x2,phi =c(1.6,-0.8),n.ahead = 40,plot = TRUE,limits = FALSE)



#----Section 7.5.2 ARMA(p,q) Predictions ---- Slide 50 - 52
# Generating Predictions using tswge

#AR(2)
x1 = gen.arma.wge(n=75,phi = c(1.6,-0.8),sn=24)
fore.arma.wge(x1,phi =c(1.6,-0.8),n.ahead = 20,plot = TRUE,limits = FALSE)

#ARMA(2,1)
x2 = gen.arma.wge(n=75,phi = c(1.6,-0.8),theta = -0.9,sn=24)
fore.arma.wge(x2,phi =c(1.6,-0.8),theta = -0.9,n.ahead = 20,plot = TRUE,limits = FALSE)

#ARMA(1,1) Predicting an ARMA(1,1)
fore.arma.wge(x2,phi =c(0.8),theta = -0.9,n.ahead = 20,plot = TRUE,limits = FALSE)


#----Section 7.5.3 Canadian Lynx Predictions ---- Slide 53 - 55
# Generating Predictions using tswge

data("llynx")
plotts.wge(llynx)

#AR(4) Prediction
fore.arma.wge(llynx,phi =c(1.3,-0.7,0.1,-0.2),n.ahead = 20,plot = TRUE,limits = FALSE)

#ARMA(4,1) Prediction
fore.arma.wge(llynx,phi =c(1.3,-0.7,0.1,-0.2),theta = -0.6,n.ahead = 20,plot = TRUE,limits = FALSE)

#AR(11) Prediction
fore.arma.wge(llynx,phi = c(1.1676,-0.5446,0.2662,-0.3094, 0.1540, -0.1463, 0.0569, -0.0294, 0.1346, 0.2021, -0.3394),n.ahead = 20,plot = TRUE,limits = FALSE)


#----Concept Check 7.5.4 ----
# Jet Fuel Data Feb 2012, Aug 2018.
# Assume a stationary series
# (1-0.967B)(X_t-2.20) = (1+0.477B)a_t


#----Concept Check 7.5.4 ----
# Jet Fuel Data Feb 2012, Aug 2018.
# Assume a stationary series
# (1-0.967B)(X_t-2.20) = (1+0.477B)a_t

#----Concept Check 7.5.6 ----
# SW Airline delay data Feb 2004, Aug 2018.
# Assume a stationary series
# (1 -0.4411B - 0.0185B^2 + 0.1193B^3 - 0.0815B^4
# + 0.0003B^5 - 0.0152B^6 - 0.0562B^7 + 0.0940B^8
# -0.0558B^9 - 0.0660B^10 - 0.0172B^11 - 0.3685B^12)
# (X_t - 34934) = a_t
SWA = read.csv(file.choose(),header=TRUE)
plotts.wge(SWA$arr_delay)
fore.arma.wge(SWA$arr_delay,phi = c(0.4411,0.0185,-0.1193,0.0815,-0.0003,0.0152,0.0562,-0.094,0.0558,0.0660,0.0172,0.3685),n.ahead = 25,plot = TRUE,limits = FALSE)


#----Section 7.6.4 Calculating psi weights ---- Slide 67 - 68
# Generating Psi weights using tswge

# psi_0 = 1, psi_1 = 0.7, psi_2 = 0.24, psi_3 = 0.132, ....
psi.weights.wge(phi = c(1.2,-0.6), theta = c(0.5),lag.max = 5)


#----Concept Check 7.5.6 ----
# (1 - 0.4B + 0.6B^2 - 0.8B^3)(X_t - 9) = a_t
psi.weights.wge(phi = c(0.4,-0.6,0.8),lag.max = 5)
    # [1]  0.40000 -0.44000  0.38400  0.73760 -0.28736


#----Section 7.6.8 Investigating Limits ---- Slide 67 - 68
# Example: Forecasts using the AR(1) model
# tswge demo: showing forecast limits
data(fig6.1nf)
#AR(1)
fore.arma.wge(fig6.1nf,phi=.8,n.ahead=20,
              plot=TRUE,limits=FALSE)
fore.arma.wge(fig6.1nf,phi=.8,n.ahead=20,
              plot=TRUE,limits=TRUE)

data(fig6.2nf)
#ARMA(2,1)
#(1-1.2B + .6B^2)(X_t - 50)= (1 -.5B)a_t
data(fig6.2nf)
fore.arma.wge(fig6.2nf,phi=c(1.2,-.6), theta=.5,
              n.ahead=20,plot=TRUE,limits=FALSE)
fore.arma.wge(fig6.2nf,phi=c(1.2,-.6), theta=.5,
              n.ahead=20,plot=TRUE,limits=TRUE)



#----Section 7.7.1 Forecasting ASE look-back ---- Slide 84 - 90
# Example: log Lynx dataset
# tswge demo: 
# AR(4) model (1-1.3B + 0.7B^2 - 0.1B^3 + 0.2B^4)(X_t - 2.9) = a_t

# Forecast 12 from end of data set
f = fore.arma.wge(llynx,phi = c(1.3, -0.7, 0.1, -0.2), n.ahead = 12, limits = FALSE)

# Forecast last 12 of dataset, to check fit of model
f = fore.arma.wge(llynx,phi = c(1.3, -0.7, 0.1, -0.2), n.ahead = 12, lastn = TRUE, limits = FALSE)
# Check the fit using ASE between last 12 observations
# and predicted 12 values.
ASE = mean((f$f-llynx[103:114])^2)
ASE
    # [1] 0.1102716

# Next, we will try an ARMA(4,1) model on the same data set
# ARMA(4,1) 1-0.7B-0.1B^2+0.2B^3 + 0.3B^4)(X_t - 2.9) = (1+ 0.6B)a_t
f4_1 = fore.arma.wge(llynx,phi = c(0.7, 0.1,-0.2,-0.3),theta = c(-0.6), n.ahead = 12, lastn = TRUE, limits = FALSE)
ASE = mean((f4_1$f-llynx[103:114])^2)
ASE
    # [1] 0.1109845

# Now, we will try an AR(11) model on the same data set
# AR(11) 
f11 = fore.arma.wge(llynx,phi=c(1.17, -0.54, 0.27, -0.31, 0.15, -0.15,
                              0.06, -0.03,0.13, 0.20, - 0.34),n.ahead=12,limits=FALSE, lastn = TRUE)
ASE = mean((f11$f-llynx[103:114])^2)
ASE
    # [1] 0.07865787

#----Section 7.7.2 Forecasting ASE look-back 
# Example: log Lynx dataset
# tswge demo: 

# AR(4) model (1-1.3B + 0.7B^2 - 0.1B^3 + 0.2B^4)(X_t - 2.9) = a_t
# Forecast last 30 of dataset, to check fit of model
LynxF_AR4 = fore.arma.wge(llynx,phi = c(1.3, -0.7, 0.1, -0.2), n.ahead = 30, lastn = TRUE, limits = FALSE)
ASE = mean((LynxF_AR4$f-llynx[85:114])^2)
ASE #[1] 0.1440614

# Now, we will try an AR(11) model on the same data set
# AR(11) 
LynxF_AR11 = fore.arma.wge(llynx,phi=c(1.17, -0.54, 0.27, -0.31, 0.15, -0.15,
                                0.06, -0.03,0.13, 0.20, - 0.34),n.ahead=30,limits=FALSE, lastn = TRUE)
ASE = mean((LynxF_AR11$f-llynx[85:114])^2)
ASE #[1] 0.1003434


#----Concept Check 7.7.3 ----
# Comparing 2 models for SW Airlines delay data, Evaluate
# these models on ASE for the last 30 observations and determine
# which model has the lower error rates based on ASE

SWA = read.csv(file.choose(),header=TRUE)
plotts.wge(SWA$arr_delay)
# Model AR(12)

SWADelay_AR12 = fore.arma.wge(SWA$arr_delay,phi = c(0.44,0.02,-0.12,0.08,-0.00,0.02,0.06,-0.09,0.06,0.07,0.02,0.37),
              n.ahead = 30,lastn = TRUE,plot = TRUE,limits = FALSE)
ASE_AR12 = mean((SWADelay_AR12$f - SWA$arr_delay[(length(SWA$arr_delay)-length(SWADelay_AR12$f)+1):(length(SWA$arr_delay))])^2)
ASE_AR12 #[1] 309880104

# Model ARMA(12,1)
SWADelay_ARMA12_1 = fore.arma.wge(SWA$arr_delay,phi = c(0.34,0.07,-0.11,0.07,0.01,0.02,0.06,-0.09,0.05,0.07,0.02,0.39), theta = c(-0.12),
                              n.ahead = 30,lastn = TRUE,plot = TRUE,limits = FALSE)
ASE_ARMA12_1 = mean((SWADelay_ARMA12_1$f - SWA$arr_delay[(length(SWA$arr_delay)-length(SWADelay_ARMA12_1$f)+1):(length(SWA$arr_delay))])^2)
ASE_ARMA12_1 #[1] 332013290


#93354237   101522226

ASE = mean((SWADelay_AR12$f-SWA$arr_delay[148:177])^2)

sum1 = (SWADelay_AR12$f-SWA$arr_delay[148:177])^2
sum2 = sum(sum1)/30

#----Section 7.8.3 ARIMA(0,1,0) Forecasting ---- Slide 108 - 110
# Example: (1-B)(X_t-mu) = a_t
# tswge demo:
xd1=gen.aruma.wge(n=75,d=1,sn=74)
fore.aruma.wge(xd1,d=1,n.ahead=5,limits=T)

# Example: (1-B)^2(X_t-mu) = a_t
xd2=gen.aruma.wge(n=30,d=2)
fore.aruma.wge(xd2,d=2,n.ahead=5,limits=T)



#----Section 7.8.4 ARIMA(0,1,0) Forecasting ---- Slide 113 - 114
# tswge demo:
x=gen.aruma.wge(n=50,phi=.8,d=1,sn=15)

# Forecast ARIMA(0,1,0) 
#(1-B)X_t = a_t
fore.aruma.wge(x,d=1,n.ahead=20 , limits = FALSE)

#ARIMA(1,1,0) 
fore.aruma.wge(x,phi=.8,d=1,n.ahead=20 , limits = FALSE)

#ARIMA(0,2,0) 
fore.aruma.wge(x,d=2,n.ahead=20, limits = FALSE)


#----Section 7.9.2 Seasonal Forecast ---- Slide 120 - 121

# tswge demo:
# Example: ARIMA(0,0,0) s=4 Forecasting
# (1-B^4)X_t = a_t
x=gen.aruma.wge(n=20,s=4,sn=6)
fore.aruma.wge(x,s=4,n.ahead=8,lastn=FALSE, plot=TRUE, limits = FALSE)

fore.aruma.wge(x,s=4,n.ahead=8,lastn=TRUE, plot=TRUE, limits = FALSE)

# Example: ARUMA(1,0,0) s=4 Forecasting
# (1-0.8B)(1-B^4)X_t = a_t
x1=gen.aruma.wge(n=20,phi=0.8,s=4,sn=6)
fore.aruma.wge(x1,phi=.8,s=4,n.ahead=24 , limits = FALSE)


#----Section 7.9.4 Parzen,Box,Woodward Competition Forecast ---- Slide 130 - 133
# Parzen Model AR(13,0,0) s=12
data(airlog)
fore.aruma.wge(airlog, d = 0, s = 12, phi = c(.74,0,0,0,0,0,0,0,0,0,0,.38,-.2812),n.ahead =
                 36,lastn = TRUE, limits = FALSE)

# Box Model
# ARMA(0,1,13) s=12
fore.aruma.wge(airlog, d = 1, s = 12, theta = c(0.4,0,0,0,0,0,0,0,0,0,0,.6,-.24),n.ahead =
                 36,lastn = TRUE, limits = FALSE)

# Woodward Model AR(12,1,0) s=12
phi1=c(-.36,-.05,-.14,-.11,.04,.09,-.02, .02,.17,.03,-.10,-.38)
fore.aruma.wge(airlog,phi=phi1,d=1,s=12,n.ahead=36,plot=T,lastn=T,limits=F)


#----Section 7.9.5 Parzen,Box,Woodward Competition Forecast ---- Slide 135 - 142
# AIC and ASE
data(airlog)

SA1 = artrans.wge(airlog,1) # take first differences of the data

SA12 = artrans.wge(airlog,c(rep(0,11),1))# take the 12th difference of the data (1-B12)

# take the 12th difference of the first difference (1-B)(1-B12)
SA1_12 = artrans.wge(SA1,c(rep(0,11),1)) 

Parzen = aic.wge(SA12, p = 13) #Phi(B)(1-B12)(Xt-mu) = at
Box = aic.wge(SA1_12, q = 13) #(1-B)(1-B12) (Xt-mu) = Theta(B)at
WoodwardAndGray = aic.wge(SA1_12, p = 12) # #Phi(B)(1-B)(1-B12) (Xt-mu) = at
Parzen$value #[1] -6.465559
Box$value #[1] -6.499213
WoodwardAndGray$value #[1] -6.423649

# Parzen's Model
Parzen = fore.aruma.wge(airlog, d = 0, s = 12, phi = c(.74,0,0,0,0,0,0,0,0,0,0,.38,-.2812),n.ahead =
                 36,lastn = TRUE, limits = FALSE)
Parzen_ASE = mean((airlog[(144-36+1):144]-Parzen$f)^2)
Parzen_ASE #[1] 0.01252636

# Box's Model
Box = fore.aruma.wge(airlog, d = 1, s = 12, theta = c(0.4,0,0,0,0,0,0,0,0,0,0,.6,-.24),n.ahead =
                 36,lastn = TRUE, limits = FALSE)
Box_ASE = mean((airlog[(144-36+1):144]-Box$f)^2)
Box_ASE #[1] 0.006903242

# Woodward's Model
phi1=c(-.36,-.05,-.14,-.11,.04,.09,-.02, .02,.17,.03,-.10,-.38)
Wood = fore.aruma.wge(airlog,phi=phi1,d=1,s=12,n.ahead=36,plot=T,lastn=T,limits=F)
Wood_ASE = mean((airlog[(144-36+1):144]-Wood$f)^2)
Wood_ASE #[1] 0.004185726

# Signal Plus Noise Forecast
# fore.sigplusnoise.wge(x,linear=TRUE,freq=0,max.p=5,
#       n.ahead=10,lastn=FALSE,plot=TRUE,limits=TRUE)

x=gen.sigplusnoise.wge(n=50,b0=10,b1=0.2,phi=c(0.8,-0.6))
xfore = fore.sigplusnoise.wge(x,linear=TRUE,n.ahead = 20,lastn = FALSE,limits = FALSE)
    # Coefficients of Original polynomial:  
    #   0.4442 -0.6324 -0.2844 -0.1076 -0.2731 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.1271B+0.8535B^2    0.6603+-0.8577i      0.9238       0.1456
    # 1+0.0583B+0.5123B^2   -0.0569+-1.3960i      0.7158       0.2565
    # 1+0.6246B             -1.6009               0.6246       0.5000

#----Concept Check 7.10.2 ----
# Amazon Stock Price Data, Fit a signal + noise model
# to this dataset and forecast the next 30 days
#library("quantmod")
#getSymbols(c('AMZN')) #Amazon

AMZN = read.csv(file.choose(),header=TRUE)
xfore = fore.sigplusnoise.wge(AMZN$Adj.Close,linear=TRUE,n.ahead = 30,lastn = FALSE,limits = TRUE)
xfore$f[30]
xfore$ll[30]
xfore$ul[30]


#----For Live Session 7.11.2 ----
# (1-0.9B)(1-0.8B)X_t = a_t
# Find the first 5 psi weights for the model
psi.weights.wge(phi = c(1.7,-0.72),lag.max = 5)

# Find the 95% probability interval for the l=3, forecast Xhat(3)
LiveSession = c(5,8,9,8,7,6,4,3)
LiveSession_Predict = fore.arma.wge(LiveSession,phi = c(1.7,-0.72),
              n.ahead = 8,lastn = FALSE,plot = TRUE,
              limits = TRUE)

sigma_aHat = sqrt((sum((LiveSession_Predict$resid)^2))/6)
print(paste0('sigma_aHat =',sigma_aHat))


b_cast = backcast.wge(LiveSession, phi =c(1.7,-0.72),
                      theta = 0,n.back = 8)

factor.wge(phi = c(0,0,0,1))



# Book Appendix 3 - using R ---

ar = mult.wge(c(1.6,-.9),c(-0.9),c(-1,-.8))
ma = mult.wge(c(-0.5),c(0,-0.95))

trsp = true.spec.wge(phi = ar$model.coef,theta = ma$model.coef)
trsp = true.arma.spec.wge(phi = ar$model.coef,theta = ma$model.coef)

factor.wge(ar$model.coef)

plotts.true.wge(n=200,phi = ar$model.coef,theta = ma$model.coef)

plotts.true.wge(n=200,phi = c(0.66,-0.02,0.10,0.24))


x=gen.arima.wge(n=50,d=1)
mult.wge(c(-0.99),c(-0.999))

# Example 5.4 - book
factor.wge(phi = c(-0.3,-0.6,-0.3,0.4))

ar=mult.wge(c(1.2,-.8),c(0.995))
plotts.true.wge(phi = c(ar$model.coef))
factor.wge(phi = c(ar$model.coef))

# Example 5.6 - book
ar=mult.wge(c(1.34,-.995),c(0.8))
plotts.true.wge(phi = c(ar$model.coef))
factor.wge(phi = c(ar$model.coef))
?acf()

# Example 5.8 - book
ar=mult.wge(c(1.34,-1),c(0.8))
factor.wge(phi = c(ar$model.coef))
gen.aruma.wge(n=250,phi = c(ar$model.coef),s=2)

ar=mult.wge(c(1),c(1),c(0,0,0,1),c(-1,-1))

#----Example 6.1 -----#
# AR(1)
data('fig6.1nf')
b = fig6.1nf
mean(b)
plotts.sample.wge(b)
b_fore = fore.aruma.wge(b, d = 0, s = 0, phi = c(.80),
                        n.ahead = 20,lastn = FALSE, limits = FALSE)

#----Example 6.2 -----#
# ARMA(2,1)
data("fig6.2nf")
F62 = fig6.2nf
p = 2 #order of ARMA model
c_fore = fore.aruma.wge(F62, d = 0, s = 0, phi = c(1.2,-0.6),theta = c(0.5),
                        n.ahead = 20,lastn = FALSE, limits = FALSE)
sigma_aHat = sqrt((sum((c_fore$resid)^2))/(length(F62)-p))
print(paste0('sigma_aHat =',sigma_aHat))

# Prediction Intervals other than 95%
# Use 99% Prediction Interval - Using Z

z = 2.58 # Z for 99%
x_6.2 = fore.aruma.wge(fig6.2nf, d = 0, s = 0, phi = c(1.2,-0.6),theta = c(0.5),
                        n.ahead = 20,lastn = FALSE, limits = FALSE)

half_width = abs((x_6.2$f-x_6.2$ll)*(z/1.96))
x_6.2$ll_99 = x_6.2$f-half_width # Will add the new ul & ll to original data set
x_6.2$ul_99 = x_6.2$f+half_width


#----Example 6.3 -----#
# ARIMA (0,1,0) - Forecast are the last value; Prediction Intervals
# are unbounded
data("fig6.5nf")
F65 = fig6.5nf
d_fore = fore.aruma.wge(F65, d = 1, s = 0, phi = c(0),theta = c(0.0),
                        n.ahead = 20,lastn = FALSE, limits = TRUE)

#----Example 6.4 -----#
# ARIMA (1,1,0) - (1-B)(1-0.8B)X_t = a_t 
# Forecast: using difference equation, forecast tend to approach
# y = C2 + X_bar:  in this case C2 = -8.15 / X_bar = 149.1 
# y = 140.9
data("fig6.6nf")
F66 = fig6.6nf
x_bar = mean(F66)
e_fore = fore.aruma.wge(F66, d = 1, s = 0, phi = c(0.8),theta = c(0.0),
                        n.ahead = 20,lastn = FALSE, limits = TRUE)


#----Example 6.5 -----#
# ARIMA (0,2,0) - (1-B)^2X_t = a_t 
# Forecast: Uses the last 2 observations and uses that trend to forcast the
# next position (predicts immediate past trend).  The forecast errors increase without bound
data("fig6.7nf")
F67 = fig6.7nf
e_fore = fore.aruma.wge(F67, d = 2, s = 0, phi = c(0),theta = c(0.0),
                        n.ahead = 20,lastn = FALSE, limits = TRUE)

#----Example 6.6 -----#
# ARUMA (0,0,0) s = 12 (1-B^12)X_t = a_t 
# Forecast: The last data values are prepeatedly forecast into
# the future (Xl-s). X_0 = X_12 = X_24 = ..
#             x_1 = X_13 = X-25
data("fig6.8nf")
F68 = fig6.8nf
e_fore = fore.aruma.wge(F68, d = 0, s = 12, phi = c(0),theta = c(0.0),
                        n.ahead = 20,lastn = FALSE, limits = FALSE)


#----Example 6.7 -----#
# ARUMA (0,0,0) s = 12 (1-B^12)X_t = a_t 
# Forecast: The last data values are prepeatedly forecast into
# the future (Xl-s). X_0 = X_12 = X_24 = ..
#             x_1 = X_13 = X-25
F68=gen.aruma.wge(n=48,phi= c(.0873,0.0802,0.3941),s=12,sn=8)
e_fore = fore.aruma.wge(F68, d = 0, s = 12, phi= c(.0873,0.0802,0.3941),theta = c(0.0),
                        n.ahead = 24,lastn = FALSE, limits = FALSE)

