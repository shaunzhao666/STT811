library(TSA)
ic <- read.csv("ice_cream.csv")
ic_cons <- ts(ic$cons, start = c(1,2014), frequency = 12)
pacf(ic_cons) # good autocorrelation at 1
ccf(ic$cons, ic$temp) # good correlation at lag 1

arimax(ic_cons, order = c(1,0,0), xreg = lag(ic$temp,1))