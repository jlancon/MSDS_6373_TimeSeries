#  MSDS 6373- Time Series Analysis - Unit 11 Assignment Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 07/16/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 7)


library(tswge)

# In preparation for the live session, please complete the following:
#   
# Please address each activity on at least one PowerPoint slide and submit via the online campus.
# 
# Our purpose is to update the Sunspot analysis. Go to this website.
# 
# Download the most current yearly mean sunspot data and with this data:
#   
# 1. Plot the data.
# 2. Comment on its stationarity.
# 3. Use aic5.wge to estimate the p and q of the model. You may use your choice of AIC/AICC/BIC.
# 4. Fit the model using your model identification (p and q). You may use any of the estimates you like (maximum likelihood, Yule–Walker, Burg).
# 5. Use this model to generate an ASE from forecasting the last 15 years of sunspot data. (You will use this to compare your models to your peer’s models.)
# 6. Now fit a seasonal model to the Sunspot data (you pick the value of s), and find the ASE for this model using the last 15 years of sunspot data. 
# 7. Describe which model you prefer, and why.
# 8. Use your best model (the one you choose) to forecast the next 10 years of sunspot data.

?sunspot.classic
#Load and plot the data (1700-2014)
sunspot = read.csv(file.choose(),header=FALSE,col.names = c('Year','MeanSpots','Marker'))
# 1. Plot the data.
plotts.sample.wge(sunspot$MeanSpots)
length(sunspot$MeanSpots) #[1] 315

#3. Use aic5.wge to estimate the p and q of the model.
aic5.wge(sunspot$MeanSpots,p=0:15,q=0:3,type = 'bic')
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 37    9    0   5.582947
    # 38    9    1   5.600826
    # 41   10    0   5.600874
    # 30    7    1   5.600905
    # 34    8    1   5.605228

aic5.wge(sunspot$MeanSpots,p=0:20,q=0:3,type = 'aic')
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 76   18    3   5.448291
    # 84   20    3   5.451593
    # 63   15    2   5.454327
    # 71   17    2   5.456510
    # 74   18    1   5.462587

# 4. Fit the model using your model identification (p and q). You may use 
#    any of the estimates you like (maximum likelihood, Yule–Walker, Burg).
s9mle=est.ar.wge(sunspot$MeanSpots,p=9,type = 'mle')
    # Coefficients of Original polynomial:  
    #   1.1667 -0.4008 -0.1524 0.1337 -0.0951 0.0257 0.0349 -0.0762 0.2421 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.6128B+0.9474B^2    0.8511+-0.5754i      0.9734       0.0946
    # 1-0.9506B              1.0520               0.9506       0.0000
    # 1-0.6170B+0.7245B^2    0.4258+-1.0949i      0.8512       0.1910
    # 1+1.4690B+0.6151B^2   -1.1942+-0.4469i      0.7843       0.4430
    # 1+0.5447B+0.6032B^2   -0.4515+-1.2058i      0.7767       0.3070
s9mle$phi
    # [1]  1.16669101 -0.40078794 -0.15236233  0.13372304 -0.09509482 
    # [6]  0.02566646  0.03487100 -0.07615663  0.24210060
s9mle$avar #[1] 221.4785
mean(sunspot$MeanSpots) #[1] 49.67587


s9burg=est.ar.wge(sunspot$MeanSpots,p=9,type = 'burg')
s9yw=est.ar.wge(sunspot$MeanSpots,p=9,type = 'yw')

mean(sunspot$MeanSpots) #[1] 49.67587


# 5. Use this model to generate an ASE from forecasting the last 15 years 
#    of sunspot data.
# Forecasting Last 15 observations
s9mle.for = fore.arma.wge(sunspot$MeanSpots,phi=s9mle$phi,n.ahead=15,
                          lastn=TRUE,limits=FALSE)

# Calculating ASE for Last 15 observations
n = 15
x = sunspot$MeanSpots
x.pred = s9mle.for$f
ASE = mean((x[(length(x)-n+1):(length(x))]-x.pred)^2)
print(paste0('ASE: ',round(ASE,3)))
    # [1] "ASE: 996.387"

######################################################
# 6. Fittiing a seasonal model to the Sunspot data
# Find the ASE for this model using the last 15 years of sunspot data.
# Looking at Realization, it appears that the data has
# a seasonal component of s=10.

# overfit to identify seasonality  
est.ar.wge(sunspot$MeanSpots,p=18,type = 'burg')
  # Coefficients of Original polynomial:  
  #   1.1394 -0.3822 -0.1472 0.1293 -0.1202 0.0504 0.0399 -0.0393 0.2896 -0.0394 
  #   0.0343 0.0030 -0.0853 0.0861 -0.0575 0.0920 -0.0649 -0.0988 
  # Factor                 Roots                Abs Recip    System Freq 
  # 1-1.6456B+0.9535B^2    0.8629+-0.5515i      0.9765       0.0905
  # 1-1.8766B+0.8868B^2    1.0581+-0.0900i      0.9417       0.0135
  # 1-0.7292B+0.8828B^2    0.4130+-0.9809i      0.9396       0.1866
  # 1-1.3846B+0.8457B^2    0.8187+-0.7157i      0.9196       0.1143
  # 1+1.6168B+0.7869B^2   -1.0273+-0.4642i      0.8871       0.4324
  # 1-0.1143B+0.7554B^2    0.0757+-1.1481i      0.8691       0.2395
  # 1+0.5521B+0.7536B^2   -0.3663+-1.0922i      0.8681       0.3015
  # 1+1.0255B+0.7118B^2   -0.7203+-0.9412i      0.8437       0.3540
  # 1+0.8124B             -1.2310               0.8124       0.5000
  # 1+0.6041B             -1.6553               0.6041       0.5000
factor.wge(phi = c(rep(0,9),1))
  # Coefficients of Original polynomial:  
  #   0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 1.0000 
  # 
  # Factor                 Roots                Abs Recip    System Freq 
  # 1-1.0000B              1.0000               1.0000       0.0000
  # 1+0.6180B+1.0000B^2   -0.3090+-0.9511i      1.0000       0.3000
  # 1-1.6180B+1.0000B^2    0.8090+-0.5878i      1.0000       0.1000
  # 1-0.6180B+1.0000B^2    0.3090+-0.9511i      1.0000       0.2000
  # 1+1.0000B             -1.0000               1.0000       0.5000
  # 1+1.6180B+1.0000B^2   -0.8090+-0.5878i      1.0000       0.4000

# Evidence of a seasonal trend in the factor tables is extremely
# weak for any S=10

# Transform the data set, using s=10
sunspot.s10 = artrans.wge(sunspot$MeanSpots,phi.tr = c(rep(0,9),1))
acf(sunspot.s10)
plotts.sample.wge(sunspot.s10)
aic5.wge(sunspot.s10,p=0:15,q=0:3,type = 'bic')
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 56   13    3   5.975975
    # 43   10    2   5.995000
    # 44   10    3   6.008910
    # 39    9    2   6.026533
    # 51   12    2   6.027945

#Chose ARMA(13,3) Model
# Fit the model
sunspot.s10.mod =est.arma.wge(sunspot.s10,p=13,q=3)
    # Coefficients of Original polynomial:  
    #   0.6020 0.7715 -0.1756 -0.5709 0.2807 0.0134 0.0003 0.0990 0.1173 
    #   -0.3678 0.1890 0.1455 -0.1818 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+1.7288B+0.8957B^2   -0.9650+-0.4302i      0.9464       0.4333
    # 1+1.2168B+0.8956B^2   -0.6793+-0.8094i      0.9463       0.3611
    # 1-1.8673B+0.8786B^2    1.0626+-0.0949i      0.9374       0.0142
    # 1-1.4826B+0.7666B^2    0.9670+-0.6078i      0.8755       0.0893
    # 1+0.0194B+0.7373B^2   -0.0131+-1.1646i      0.8586       0.2518
    # 1-0.9311B+0.6392B^2    0.7283+-1.0168i      0.7995       0.1511
    # 1+0.7140B             -1.4006               0.7140       0.5000
sunspot.s10.mod$phi
    # [1]   0.6019795483  0.7714577607 -0.1756096655 -0.5709492699  
    # [5]   0.2806974146  0.0133637310  0.0003220232  0.0989909870
    # [9]   0.1172918566 -0.3678277274  0.1889878508  0.1454818979
    # [13] -0.1818014008
sunspot.s10.mod$theta
    # [1] -0.5602144  0.6100634  0.9501492
sunspot.s10.mod$avar #[1] 286.3279
mean(sunspot$MeanSpots) #[1] 49.67587

# Use this model to generate an ASE from forecasting the last 15 years 
# of sunspot data.
# Forecasting Last 15 observations
sunspot.s10.mod.for = fore.aruma.wge(sunspot$MeanSpots,phi=sunspot.s10.mod$phi,s=10,
                                     theta = sunspot.s10.mod$theta,
                                     n.ahead=15,lastn=TRUE,limits=FALSE)

# Calculating ASE for Last 15 observations
n = 15
x = sunspot$MeanSpots
x.pred = sunspot.s10.mod.for$f
ASE = ((x[(length(x)-n+1):(length(x))]-x.pred)^2)
print(paste0('ASE: ',round(ASE,3)))
# [1] "ASE: 1570.098"

# 8. Use your best model (the one you choose) to forecast the next 
#    10 years of sunspot data.
# Use Model AR(9)
# Forecasting Next 10 observations
s9mle.for = fore.arma.wge(sunspot$MeanSpots,phi=s9mle$phi,n.ahead=10,
                          lastn=FALSE,limits=FALSE)


######################################################
# Acuspike dataset
#Acuspike Active users (06/01/18 - 12/19/18)
acuspike = read.csv(file.choose(),header=TRUE)
acuspike = acuspike[1:182,]
# 1. Plot the data.
plotts.sample.wge(acuspike$Active.Users)
length(acuspike$Active.Users) #[1] 182

# Use aic5.wge to estimate the p and q of the model.
aic5.wge(acuspike$Active.Users,p=0:15,q=0:3,type = 'bic')
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 11    2    2   5.826061
    # 29    7    0   5.836056
    # 26    6    1   5.855731
    # 33    8    0   5.862624
    # 30    7    1   5.863772

aic5.wge(acuspike$Active.Users,p=0:20,q=0:3,type = 'aic')
    # Five Smallest Values of  aic 
    #       p    q        aic
    # 60   14    3   5.634917
    # 44   10    3   5.652536
    # 49   12    0   5.665645
    # 61   15    0   5.666015
    # 53   13    0   5.667186

# overfit to identify seasonality  
est.ar.wge(acuspike$Active.Users,p=20,type = 'mle')
    # Coefficients of Original polynomial:  
    #   0.3929 0.0009 0.0299 0.0160 -0.1070 0.0846 0.1771 0.1121 -0.0339
    #   -0.0429 -0.1237 0.1560 0.0273 0.1124 -0.0889 -0.0741 0.0061 -0.0917
    #   -0.0051 0.1242 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.2331B+0.9739B^2    0.6331+-0.7912i      0.9869       0.1426
    # 1+0.5302B+0.8633B^2   -0.3071+-1.0315i      0.9292       0.2961
    # 1-0.9257B              1.0802               0.9257       0.0000
    # 1-0.0600B+0.8548B^2    0.0351+-1.0810i      0.9245       0.2448
    # 1-1.5496B+0.8517B^2    0.9097+-0.5887i      0.9229       0.0914
    # 1+0.9212B             -1.0856               0.9212       0.5000
    # 1+1.0548B+0.8334B^2   -0.6328+-0.8941i      0.9129       0.3480
    # 1+1.5297B+0.8192B^2   -0.9336+-0.5908i      0.9051       0.4102
    # 1-1.6722B+0.7366B^2    1.1352+-0.2628i      0.8582       0.0362
    # 1-0.5200B+0.7259B^2    0.3582+-1.1178i      0.8520       0.2006
    # 1+1.5318B+0.6521B^2   -1.1746+-0.3924i      0.8075       0.4487

factor.wge(phi = c(rep(0,6),1))
    # Coefficients of Original polynomial:  
    #   0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 1.0000 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.0000B              1.0000               1.0000       0.0000
    # 1+0.4450B+1.0000B^2   -0.2225+-0.9749i      1.0000       0.2857
    # 1-1.2470B+1.0000B^2    0.6235+-0.7818i      1.0000       0.1429
    # 1+1.8019B+1.0000B^2   -0.9010+-0.4339i      1.0000       0.4286

# A sesonality of s=7 was identified in the data set.

# Transform the data set, using s=7
spike.s7 = artrans.wge(acuspike$Active.Users,phi.tr = c(rep(0,6),1))
acf(spike.s7)
plotts.sample.wge(spike.s7)

# Use aic5.wge to estimate the p and q of the model.
aic5.wge(spike.s7,p=0:15,q=0:3,type = 'bic')
    # Five Smallest Values of  bic 
    #       p    q        bic
    # 33    8    0   5.979358
    # 37    9    0   6.008265
    # 34    8    1   6.008323
    # 31    7    2   6.018781
    # 30    7    1   6.021080

aic5.wge(spike.s7,p=0:30,q=0:4,type = 'aic')
    # Five Smallest Values of  aic 
    #         p    q        aic
    # 103   20    2   5.679543
    # 94    18    3   5.684224
    # 98    19    2   5.685382
    # 141   28    0   5.685812
    # 142   28    1   5.688539

#Chose AR(8) Model
# Fit the model
spike.s7.mod =est.arma.wge(spike.s7,p=8)
    # Coefficients of Original polynomial:  
    #   0.3606 0.0043 0.0830 0.0637 -0.1555 0.0029 -0.4620 0.3308 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+0.9471B             -1.0558               0.9471       0.5000
    # 1+1.1217B+0.8312B^2   -0.6747+-0.8647i      0.9117       0.3555
    # 1-0.2338B+0.8269B^2    0.1414+-1.0906i      0.9094       0.2295
    # 1-1.5511B+0.7883B^2    0.9839+-0.5482i      0.8879       0.0809
    # 1-0.6445B              1.5516               0.6445       0.0000
spike.s7.mod$phi
    #[1]  0.360590095  0.004255630  0.082975287  0.063717392 -0.155481073
    #[6]  0.002916824 -0.462010774  0.330755460
spike.s7.mod$avar #[1] 303.0024
mean(acuspike$Active.Users) #[1] 84.11538


# Use this model to generate an ASE from forecasting the last 15 years 
# of sunspot data.
# Forecasting Last 15 observations
spike.s7.mod.for = fore.aruma.wge(sunspot$MeanSpots,phi=spike.s7.mod$phi,s=7,
                                 n.ahead=15,lastn=TRUE,limits=FALSE)

# Calculating ASE for Last 15 observations
n = 15
x = acuspike$Active.Users
x.pred = spike.s7.mod.for$f
ASE = mean((x[(length(x)-n+1):(length(x))]-x.pred)^2)
print(paste0('ASE: ',round(ASE,3)))
# [1] "ASE: 1602.609"


# Forecasting Next 10 observations
spike.s7.mod.for10 = fore.aruma.wge(sunspot$MeanSpots,phi=spike.s7.mod$phi,s=7,
                                  n.ahead=15,lastn=FALSE,limits=FALSE)
