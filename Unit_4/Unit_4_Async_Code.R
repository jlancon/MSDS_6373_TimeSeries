#  MSDS 6373- Time Series Analysis - Unit 4 Async Code 

#  Team Member:  Jeffery Lancon
#  
#  Date: 05/28/2019
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 3)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

# Section / Async Video 4.5.2 Behavior-Case 1: Two Real Roots / Slides 39,40

# Xt - 0.2X_t-1 - .48X_t-2 One Positive One Negative Roots
x = gen.arma.wge(200, phi=c(.2, .48))
plotts.sample.wge(x)
plotts.true.wge(phi = c(.2,.48)) # True spectral density

# Xt - 1.4X_t-1 + .48X_t-2 Two Positive Roots
x = gen.arma.wge(200, phi=c(1.4, -.48))
plotts.sample.wge(x)

# Xt + 1.4X_t-1 + .48X_t-2 Two Negative Roots (1+.8B)(1+.6B)X_t
x = gen.arma.wge(200, phi=c(-1.4, -.48))
plotts.sample.wge(x)
plotts.true.wge(phi = c(-1.4,-.48)) # True spectral density


# Async Problem 4.5.3,4,5,6
x = gen.arma.wge(100, phi=c(-.9,-.2))
plotts.sample.wge(x)

# Section / Async Video 4.6.1
# Xt - 1.6X_t-1 + .8X_t-2 Two Complex conjugate Roots
x = gen.arma.wge(100, phi=c(1.6, -.8))
plotts.sample.wge(x)
plotts.true.wge(phi = c(1.4,-.8))

# Section / Async Video 4.7.1 / Slides 49-52
# Lynx Data Set with AR(2)
data("lynx")
plotts.wge(lynx)
plotts.sample.wge(lynx)
 # Take the log of the data to decrease volitility
data("llynx")
plotts.sample.wge(llynx)
# formula (1 - 1.3B + .75B^2)(X_t - 2.9) = a_t

# # Section / Async Video 4.9.2 / Slides 79-80
# Identifying Factor Tables for AR(p)

# Realization: Xt - 1.95X_t-1 + 1.85X_t-2 - 0.855X_t3 = a_t
# Factor Table
factor.wge(phi=c(1.95,-1.85,0.855))
    # Coefficients of Original polynomial:  
    #   1.9500 -1.8500 0.8550 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-0.9500B              1.0526               0.9500       0.0000
    # 1-1.0000B+0.9000B^2    0.5556+-0.8958i      0.9487       0.1616

plotts.true.wge(phi=c(1.95,-1.85,0.855))


# Async Problem 4.9.3,4,5,6
# # Realization: Xt - 1.59X_t-1 + 0.544X_t-2 + 0.511X_t3 - 0.222X_t4 = a_t
factor.wge(phi=c(1.59,-0.544,-0.511,0.222))
    # Coefficients of Original polynomial:  
    #   1.5900 -0.5440 -0.5110 0.2220 
    # 
    # Factor                 Roots                Abs Recip    System Freq 
    # 1-1.7163B+0.9860B^2    0.8704+-0.5066i      0.9930       0.0839
    # 1+0.5419B             -1.8455               0.5419       0.5000
    # 1-0.4155B              2.4066               0.4155       0.0000



# # Section / Async Video 4.10.1 / Slides 83-85
# Realization: Xt - 1.95X_t-1 + 1.85X_t-2 - 0.855X_t3 = a_t
# Factor Table
factor.wge(phi=c(1.95,-1.85,0.855))
# Plots: Realization / Frequency / Acc
plotts.true.wge(phi=c(1.95,-1.85,0.855))

# # Section / Async Video 4.10.1 / Slides 86-87
# Realization: Xt - 0.2X_t-1 - 1.23X_t-2 + 0.26X_t3 + 0.66Xt_4 = a_t
# Factor Table
factor.wge(phi=c(0.2,1.23,-0.26,-0.66))
# Plots: Realization / Frequency / Acc
plotts.true.wge(phi=c(0.2,1.23,-0.26,-0.66))

# # Section / Async Video 4.10.1 / Slides 88-89
# Realization: Xt - 1.0X_t-1 - 0.5X_t-2 + 0.8X_t3 - 0.7Xt_4 = a_t
# Factor Table
factor.wge(phi=c(1.0,0.5,-0.8,0.7))
# Plots: Realization / Frequency / Acc
plotts.true.wge(phi=c(1.0,0.5,-0.8,0.7)) # Non-Stationary


# Async Problem 4.10.4
factor.wge(phi=c(-0.59,-0.544,-0.511,-0.222))
  # Coefficients of Original polynomial:  
  #   -0.5900 -0.5440 -0.5110 -0.2220 
  # 
  # Factor                 Roots                Abs Recip    System Freq 
  # 1-0.4345B+0.6448B^2    0.3369+-1.1989i      0.8030       0.2064
  # 1+1.0245B+0.3443B^2   -1.4878+-0.8313i      0.5868       0.4189
plotts.true.wge(phi=c(-0.59,-0.544,-0.511,-0.222))


# Async Problem 4.11
# Realization: Xt + 0.5X_t-1 + 0.6X_t-2 = a_t
factor.wge(phi=c(-0.50,-0.6))
plotts.true.wge(phi=c(-0.50,-0.6))
