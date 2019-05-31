#  MSDS 6373- Time Series Analysis - Section 1
#  Unit 2 :  Homework
#  Problem:  1.5 from textbook

#  Team Member:  Jeffery Lancon
#  
#  Date: 05/14/2019
#
#  Case Study from: Applied Time Series Analysis with R : 2nd Edition (Ch 1)


library(tswge)
# https://cran.r-project.org/web/packages/tswge/tswge.pdf


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

# Load Data set
data("fig1.21a")

# Plotting realization for reference
plotts.wge(fig1.21a)

# Plotting acf and spectral density estimate
plotts.sample.wge(fig1.21a)
dev.off()

# Find the Parzen spectral density estimate for a realization with M=31 (displayed in dB)
dev.off()
parzen.wge(fig1.21a,trunc = 31)

# Plot the spectral density estimate without taking the logarithms.
parzen.wge(fig1.21a,trunc = 31,dbcalc = FALSE)