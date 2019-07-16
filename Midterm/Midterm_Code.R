
library(tswge)
##################################
# Midterm - Problem 1
ts <- c(6,8,13,12,10,7,4,2,1)
mean(ts) #[1] 7
n = length(ts)
gamma0 = var(ts)*((n-1)/n)
gamma0  # [1] 15.77778


# Calculating gamma_hat(x)
nlag=n-1
gamma_hat <- rep(0, nlag)
for (k in seq(nlag)){
  sum = 0
  for (i in seq((length(ts)-k))){
    sum = sum + ((ts[i]-mean(ts))*(ts[i+k]-mean(ts)))
    print(sum)
  }
  gamma_hat[k] = sum/length(ts)
}
gamma_hat
    # [1]  14.742188  -1.515625 -15.023438 -16.281250
    # [5]  -6.039062   1.453125   5.195312

# Determining rhos(x)
rhos <- acf(ts,plot = FALSE,lag.max = n-1)
rhos
  # Autocorrelations of series 'ts', by lag
  # Autocorrelations of series ‘ts’, by lag
  # 
  # 0      1      2      3      4      5      6      7      8 
  # 1.000  0.669  0.183 -0.225 -0.451 -0.444 -0.268 -0.007  0.042 
print(rhos$acf[3]) #[1] 0.1830986

factor.wge(phi = c(0.6,-0.4))

P9 = fore.aruma.wge(ts,phi = c(0.6,-0.4),theta = 0,d=0,s=0,
               n.ahead = 4,lastn = FALSE,limits = TRUE)
  # $f
  # [1] 5.4000 8.4400 8.5040 7.3264


data("global.temp")
gt = global.temp[100:length(global.temp)]
x29 = gen.sigplusnoise.wge(gt)
gen.aruma.wge(ph)




psi.weights.wge(phi = c(0.6,-0.4),lag.max = 3 ) #[1]  0.600 -0.040 -0.264

P9 = fore.aruma.wge(ts,phi = c(0.6,-0.4),theta = 0,d=0,s=0,
                    n.ahead = 4,lastn = FALSE,limits = TRUE)
    # $ll
    # [1] -0.5799505  1.4662393  1.5261383  0.1721796
    # $ul
    # [1] 11.37995 15.41376 15.48186 14.48062
    # $resid
    # [1]  0.0  0.0  5.0  1.8  2.4  0.2 -1.8 -3.2 -4.2
    # $wnv
    # [1] 9.308571
    # $se
    # [1] 3.050995 3.558041 3.560134 3.650112
    # $psi
    # [1]  0.6000 -0.0400 -0.2640 -0.1424
    # $ptot
    # [1] 2
    # $phitot
    # [1]  0.6 -0.4


x = gen.aruma.wge(n=100,phi = c(.9),theta = 0,d=0,s=0,vara = 1,sn=0)
plotts.true.wge(phi = c(.99))
?plotts.sample.wge()




x = gen.aruma.wge(n=100,phi = c(-.9),theta = 0,d=0,s=0,vara = 1,sn=0)
?gen.aruma.wge()
fore.aruma.wge(x,phi = c(-.5),theta = 0,d=0,s=0,
               n.ahead = 10,lastn = FALSE,limits = FALSE)
?fore.aruma.wge()
factor.wge(phi = c(0.9,-.4))
# Coefficients of Original polynomial:  
#   0.9000 -0.4000 
# 
# Factor                 Roots                Abs Recip    System Freq 
# 1-0.9000B+0.4000B^2    1.1250+-1.1110i      0.6325       0.1240

factor.wge(phi= c(-.17,-2.75))
# Coefficients of Original polynomial:  
#   -0.1700 -2.7500 
# Factor                 Roots                Abs Recip    System Freq 
# 1+0.1700B+2.7500B^2   -0.0309+-0.6022i      1.6583       0.2582
plotts.true.wge(theta = c(-.17,-2.75))
# $aut1
# [1] 1.00000000 0.07420211 0.32008753

plotts.true.wge(theta = c(-0.0618,-.3636))
# $aut1
# [1] 1.00000000 0.07418018 0.32006360 