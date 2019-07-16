#  MSDS 6373- Time Series Analysis - Unit 5 Homework Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 06/27/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 3)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

#Problem 3.2
#Generate and plot a realization of length n=200 from the MA(3) models
#a)	X_t -25 = a_t – 0.95a_t-1 + 0.9a_t-2 – 0.855a_t-3 where σ_a2 = 1

#For the original data, plot the following:
#  i)	The realization generated
# ii)	The theoretical autocorrelations.
#iii)	The theoretical spectral density
#Discuss the information available in the plots. Also find
# iv)	µ and σx2
# v)	µhat and σhatx2 for the realization in (a)

#a)	X_t -25 = a_t – 0.95a_t-1 + 0.9a_t-2 – 0.855a_t-3
x1 = gen.arma.wge(200, theta=c(0.95,-0.9,0.855), vara = 1, sn=2)

x = plotts.true.wge(n=200,theta=c(0.95,-0.9,0.855), vara = 1)
x25 = x$data+25
plotts.wge(X25)

mean(x25) #[1] 25.01371
var(x25) #[1] 2.997201

#Problem 3.3
#Determine whether the following models are stationary and explain your answers.
#a)	X_t = a_t – 2a_t-1 + 1.76a_t-2 – 1.6a_t-3 + 0.77a_t-4

factor.wge(phi = c( 2, -1.76, 1.6, -0.77))
    # Coefficients of Original polynomial:  
    #   2.0000 -1.7600 1.6000 -0.7700 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.1977B              0.8349               1.1977       0.0000
    # 1+0.0012B+0.8001B^2   -8e-04+-1.1180i      0.8945       0.2501
    # 1-0.8035B              1.2446               0.8035       0.0000


#Problem 3.6
#Consider the model:
#  X_t – X_t-1 + 0.26X_t-2 +0.64X_t-3 – 0.576X_t-4 = a_t – 2.4a_t-1 + 2.18a_t-2 – 0.72a_t-3 
#a)	This specifies an ARMA(p,q) model.  What are p and q? Hint Factor the polynomial

# AR(p) portion
factor.wge(phi =  c(1, -0.26, -0.64, 0.576))
  # Coefficients of Original polynomial:  
  #   1.0000 -0.2600 -0.6400 0.5760 
  # 
  # Factor                 Roots                Abs Recip    System Freq 
  # 1-1.0000B+0.9000B^2    0.5556+-0.8958i      0.9487       0.1616
  # 1-0.8000B              1.2500               0.8000       0.0000
  # 1+0.8000B             -1.2500               0.8000       0.5000

# MA(q) portion
factor.wge(phi =  c(2.4, -2.18, 0.72))
  # Coefficients of Original polynomial:  
  #   2.4000 -2.1800 0.7200 
  # 
  # Factor                 Roots                Abs Recip    System Freq 
  # 1-1.6000B+0.9000B^2    0.8889+-0.5666i      0.9487       0.0903
  # 1-0.8000B              1.2500               0.8000       0.0000



# Problem 3.7
# Consider the following models:
#   a)	Xt – 0.1Xt-1 + 0.5Xt-2 + 0.08Xt-3 – 0.24Xt-4 = at
#   b)	Xt – 1.3Xt-1 + 0.4Xt-2 = at – 1.9at-1
#   c)	Xt – 1.9Xt-1 = at – 1.3at-1 + 0.4at-2
#   d)	Xt – 2.95Xt-1 + 3.87Xt-2 – 2.82Xt-3 + 0.92Xt-4 = at – 0.9at-1
#   e)	(1 – B – 0.49B2 + 0.9B3 – 0.369B4)Xt = (1 + B + B2 + 0.75B3)at
#   For each process:
#     i)	Determine whether the process is stationary
# ii)	Determine whether the process is invertible
# iii)	Plot the true autocorrelations and spectral density for the processes that are both stationary and invertible.  Discuss how the features of the model as shown in the factor table are visually displayed in the autocorrelation and spectrum.

#a) Xt – 0.1Xt-1 + 0.5Xt-2 + 0.08Xt-3 – 0.24Xt-4 = at
factor.wge(phi = c(0.1, -0.5, -0.08, 0.24))
  # Coefficients of Original polynomial:  
  #   0.1000 -0.5000 -0.0800 0.2400 
  # 
  # Factor                 Roots                Abs Recip    System Freq 
  # 1-0.1455B+0.8048B^2    0.0904+-1.1110i      0.8971       0.2371
  # 1+0.5693B             -1.7565               0.5693       0.5000
  # 1-0.5238B              1.9091               0.5238       0.0000
plotts.true.wge(phi = c(0.1, -0.5, -0.08, 0.24))


#   b)	Xt – 1.3Xt-1 + 0.4Xt-2 = at – 1.9at-1
# AR(p) factoring
factor.wge(phi = c(1.3,-0.4))
    # Coefficients of Original polynomial:  
    #   1.3000 -0.4000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.8000B              1.2500               0.8000       0.0000
    # 1-0.5000B              2.0000               0.5000       0.0000

# MA(q) factoring
factor.wge(phi = c(1.9))
    # Coefficients of Original polynomial:  
    #   1.9000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.9000B              0.5263               1.9000       0.0000


#   c)	Xt – 1.9Xt-1 = at – 1.3at-1 + 0.4at-2
# AR(p) factoring
factor.wge(phi = c(1.9))
    # Coefficients of Original polynomial:  
    #   1.9000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.9000B              0.5263               1.9000       0.0000

# MA(q) factoring
factor.wge(phi = c(1.3,-0.4))
    # Coefficients of Original polynomial:  
    #   1.3000 -0.4000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.8000B              1.2500               0.8000       0.0000
    # 1-0.5000B              2.0000               0.5000       0.0000


#   d)	Xt – 2.95Xt-1 + 3.87Xt-2 – 2.82Xt-3 + 0.92Xt-4 = at – 0.9at-1
# AR(p) factoring
factor.wge(phi = c(2.95, -3.87, 2.82, -0.92))
    # Coefficients of Original polynomial:  
    #   2.9500 -3.8700 2.8200 -0.9200 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.9484B+0.9695B^2    1.0049+-0.1475i      0.9846       0.0232
    # 1-1.0016B+0.9490B^2    0.5278+-0.8805i      0.9742       0.1641

# MA(q) factoring
factor.wge(phi = c(0.9))
    # Coefficients of Original polynomial:  
    #   0.9000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9000B              1.1111               0.9000       0.0000
xd = plotts.true.wge(phi = c(2.95, -3.87, 2.82, -0.92), theta = c(0.9))



#   e)	(1 – B – 0.49B2 + 0.9B3 – 0.369B4)Xt = (1 + B + B2 + 0.75B3)at
# AR(p) factoring
factor.wge(phi = c(1, 0.49, -0.9, 0.369))
    # Coefficients of Original polynomial:  
    #   1.0000 0.4900 -0.9000 0.3690 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+0.9487B             -1.0541               0.9487       0.5000
    # 1-0.9487B              1.0541               0.9487       0.0000
    # 1-1.0000B+0.4100B^2    1.2195+-0.9756i      0.6403       0.1074

# MA(q) factoring
factor.wge(phi = c(-1,-1,-0.75))
    # Coefficients of Original polynomial:  
    #   -1.0000 -1.0000 -0.7500 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+0.1443B+0.8765B^2   -0.0823+-1.0650i      0.9362       0.2623
    # 1+0.8557B             -1.1687               0.8557       0.5000
xe = plotts.true.wge(phi = c(1, 0.49, -0.9, 0.369), theta = c(-1,-1,-0.75))



# Problem 3.8
# Show that each of the following MA(2) processes in not invertible.  In each case,
# find an invertible MA(2) process that has the same autocorrelation as Xt.  
# Use tswge R function true.arma.aut.wge() to verify that the autocorrelations are
# the same.
# a)	Xt = at – 0.5at-1 – 5at-2
# b)	Xt = at – 2.0at-1 + 1.5at-2

# a)	Xt = at – 0.5at-1 – 5at-2
factor.wge(phi = c(0.5,5))
  # Coefficients of Original polynomial:  
  #   0.5000 5.0000 
  # 
  # Factor                 Roots                Abs Recip    System Freq 
  # 1-2.5000B              0.4000               2.5000       0.0000
  # 1+2.0000B             -0.5000               2.0000       0.5000

true.arma.aut.wge(theta = c(0.5,5))
plotts.true.wge(theta = c(0.5,5))
    # $aut1
    # [1]  1.00000000  0.07619048 -0.19047619  0.00000000 
mult.wge(fac1 = 0.4,fac2 = -0.5)
    # $char.poly
    # 1 + 0.1*x - 0.2*x^2 
    # 
    # $model.coef
    # [1] -0.1  0.2
plotts.true.wge(theta = c(-0.1,0.2))
    # $aut1
    # [1]  1.00000000  0.07619048 -0.19047619  0.00000000
factor.wge(phi = c(-0.1,0.2))
    # Coefficients of Original polynomial:  
    #   -0.1000 0.2000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+0.5000B             -2.0000               0.5000       0.5000
    # 1-0.4000B              2.5000               0.4000       0.0000


# b)	Xt = at – 2.0at-1 + 1.5at-2
factor.wge(phi = c(2,-1.5))
    # Coefficients of Original polynomial:  
    #   2.0000 -1.5000 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-2.0000B+1.5000B^2    0.6667+-0.4714i      1.2247       0.0980
true.arma.aut.wge(theta = c(2,-1.5))
plotts.true.wge(theta = c(2,-1.5))
    # $aut1
    # [1]  1.0000000 -0.6896552  0.2068966  0.0000000 

plotts.true.wge(theta = c(1.3333,-0.6667)) # Be very cautous with signs of phi
    # $aut1
    # [1]  1.0000000 -0.6896612  0.2069098  0.0000000 


#Problem 3.11
#Consider the ARMA(4,3) model,
#(1 – B - 0.49B2 + 0.9B3 – 0.369B4)Xt = (1 + B + B2 + 0.75B3)at
#a)	Verify that this is a stationary and invertible process.
#b)	Find ѱi i=1,2,….10.

#a)	Verify that the AR(4) is stationary.
factor.wge(phi = c(1,0.49,-0.9,0.369))
  # Coefficients of Original polynomial:  
  #   1.0000 0.4900 -0.9000 0.3690 
  # 
  # Factor                 Roots                Abs Recip    System Freq 
  # 1+0.9487B             -1.0541               0.9487       0.5000
  # 1-0.9487B              1.0541               0.9487       0.0000
  # 1-1.0000B+0.4100B^2    1.2195+-0.9756i      0.6403       0.1074

#a)	Verify that the MA(3) is invertible.
factor.wge(phi = c(-1,-1,-0.75))
    # Coefficients of Original polynomial:  
    #   -1.0000 -1.0000 -0.7500 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1+0.1443B+0.8765B^2   -0.0823+-1.0650i      0.9362       0.2623
    # 1+0.8557B             -1.1687               0.8557       0.5000


#b)	Find ѱi i=1,2,….10.
psi.weights.wge(phi = c(1,0.49,-0.9,0.369),theta = c(-1,-1,-0.75),lag.max = 10)
    # [1] 2.000000 3.490000 4.320000 4.599100 4.312900 3.966269 3.534480 3.293410 3.047123
    # [10] 2.943415



# Problem 3.13(i)
# Generate realizations of length n=100 from the following stationary AR(p) model.
# i)	(1 - 2.2B + 2.1B2 – 0.8B3)Xt = at 
# For each of the realizations
# a)	Plot the realization along with the additive AR components (See Section 3.4)
# b)	Discuss your results relating each additive component to one of the lines in the factor table.

factor.wge(phi = c(2.2,-2.1, 0.8))
    # Coefficients of Original polynomial:  
    #   2.2000 -2.1000 0.8000 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.3685B+0.9621B^2    0.7112+-0.7305i      0.9809       0.1271
    # 1-0.8315B              1.2026               0.8315       0.0000

xi = gen.arma.wge(n=100, phi = c(2.2,-2.1, 0.8),sn=2)
?factor.comp.wge()
factor.comp.wge(xi,p=3,ncomp=2)