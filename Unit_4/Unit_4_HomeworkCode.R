#  MSDS 6373- Time Series Analysis - Unit 4 Homework Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 06/26/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 3)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

#Problem 3.2
#Generate and plot a realization of length n=200 from the AR(p) models
#a)	X_t – 0.95X_t-1 = a_t where σ_a2 = 1
#b)	(X_t - 10) – 1.5(X_t-1 – 10) + 0.9(X_t-2 – 10) = a_t  where σ_a2 = 1
#For the original data and the three filtered realizations obtained in (a-c), plot the following:
#  i)	The realization generated
# ii)	The theoretical autocorrelations.
#iii)	The theoretical spectral density
#Discuss the information available in the plots. Also find
# iv)	µ and σx2
# v)	µhat and σhatx2 for the realization in (a)

#a)	X_t – 0.95X_t-1 = a_t where σ_a2 = 1
x_a = gen.arma.wge(200, phi=c(0.95), vara = 1, sn=2)
x_a_true = plotts.true.wge(n=200,phi=c(0.95), vara = 1)

mean(x_a_true$data) #[1] -0.7596384
var(x_a_true$data) #[1] 9.351287

print(paste0('Mean of x_a_true: ',mean(x_a_true$data)))
print(paste0('Mean of x_a_true: ',mean(x_a_true$data)))

#b)	(X_t - 10) – 1.5(X_t-1 – 10) + 0.9(X_t-2 – 10) = a_t  where σ_a2 = 1
x_b_true = plotts.true.wge(n=200,phi=c(1.5,-0.9), vara = 1)
x_b_true10 = x_b_true$data+10
plotts.wge(x_b_true10)

mean(x_b_true10) #[1] 10.15926
var(x_b_true10) #[1] 10.00866



#Problem 3.3
#Determine whether the following models are stationary and explain your answers.
#a)	Xt – 1.55Xt-1 + Xt-2 – 0.25Xt-3 = at
#b)	Xt – 2Xt-1 + 1.76Xt-2 – 1.6Xt-3 + 0.77Xt-4 = at
#c)	n/a
#d)	Xt – 1.9Xt-1 + 2.3Xt-2 – 2Xt-3 + 1.2Xt-4 – 0.4Xt-5 = at

# Model
#a)	Xt – 1.55Xt-1 + Xt-2 – 0.25Xt-3 = at
factor.wge(phi = c(1.55,-1,0.25))
    # Coefficients of Original polynomial:  
    #   1.5500 -1.0000 0.2500 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9877B+0.4446B^2    1.1107+-1.0076i      0.6668       0.1173
    # 1-0.5623B              1.7785               0.5623       0.0000

# Model
#b)	Xt – 2Xt-1 + 1.76Xt-2 – 1.6Xt-3 + 0.77Xt-4 = at
factor.wge(phi = c(2,-1.76, 1.6, -0.77))
    # Coefficients of Original polynomial:  
    #   2.0000 -1.7600 1.6000 -0.7700 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.1977B              0.8349               1.1977       0.0000
    # 1+0.0012B+0.8001B^2   -8e-04+-1.1180i      0.8945       0.2501
    # 1-0.8035B              1.2446               0.8035       0.0000

#Model
#d)	Xt – 1.9Xt-1 + 2.3Xt-2 – 2Xt-3 + 1.2Xt-4 – 0.4Xt-5 = at
factor.wge(phi = c(1.9,-2.3, 2.0, -1.2, 0.4))
    # Coefficients of Original polynomial:  
    #   1.9000 -2.3000 2.0000 -1.2000 0.4000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+0.0080B+0.7888B^2   -0.0050+-1.1259i      0.8882       0.2507
    # 1-1.1206B+0.6440B^2    0.8700+-0.8921i      0.8025       0.1270
    # 1-0.7873B              1.2701               0.7873       0.0000



