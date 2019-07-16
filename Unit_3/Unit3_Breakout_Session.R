#unit 3 Time Series - Lecture Break Out 1

# Breakout 1
library(tswge)
#Part 1
set.seed(2)
x = gen.sigplusnoise.wge(n=100,coef = c(1,0),freq = c(.13,0),vara = 1)
parzen.wge(x)

x = gen.sigplusnoise.wge(n=100,coef = c(1,0),freq = c(.8,0),vara = 1,sn=8)
parzen.wge(x)

ma = stats::filter(x,rep(1,5))/5
plot(ma, type='l')

dif = diff(x,lag = 1)
plot(dif,type='l')
parzen.wge(dif)

x = read.csv(file.choose(),header=TRUE)

plotts.wge(x$x)
parzen.wge(x$x)

ma = stats::filter(ts(x$x),rep(1,5))/5
plot(ma, type='l')
ma <- na.omit(ma, inplace=TRUE)
parzen.wge(ma)

dif = diff(x$x,lag = 1)
plot(dif,type='l')
parzen.wge(dif)

par(mfrow = c(1,1))
xx = butterworth.wge(x$x, type='low',cutoff = 0.45)
parzen.wge(xx$x, trunc = 50)


x = gen.sigplusnoise.wge(n=100,coef = c(1,0),freq = c(.8,0),vara = 1,sn=8)
