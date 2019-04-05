#### Homework 5
##
## Konner Macias
## 004603916
##

data=read.table("http://www.stat.ucla.edu/~jsanchez/data/stockmarket.dat", header=T)
head(data)
stock = ts(data, start = c(1986,6), frequency = 365)

## A
# plot data and describe what you see
plot.ts(stock, main = "Stock Market by Country")

## B
# see acf and diff if needed
acf(stock)
acf(diff(stock))

## C
# fit VAR
install.packages("vars")
library(vars)
VAR.data <- VAR(data, p=2, type="trend")
coef(VAR.data)

## D
# explore the acf of residuals
acf(resid(VAR.data))

## E
## forecast when good residuals
VAR.pred <- predict(VAR.data, n.ahead=4)
Am.pred <- ts(VAR.pred$fcst$Amsterdam[,1], start = c(1994,214), freq = 365)
Am.lb <- ts(VAR.pred$fcst$Amsterdam[,2], start =  c(1994,214), freq = 365)
Am.ub <- ts(VAR.pred$fcst$Amsterdam[,3], start =  c(1994,214), freq = 365)

Fr.pred <- ts(VAR.pred$fcst$Frankfurt[,1], start = c(1994,214), freq = 365)
Fr.lb <- ts(VAR.pred$fcst$Frankfurt[,2], start = c(1994,214), freq = 365)
Fr.ub <- ts(VAR.pred$fcst$Frankfurt[,3], start = c(1994,214), freq = 365)

Lo.pred <- ts(VAR.pred$fcst$London[,1], start = c(1994,214), freq = 365)
Lo.lb <- ts(VAR.pred$fcst$London[,2], start = c(1994,214), freq = 365)
Lo.ub <- ts(VAR.pred$fcst$London[,3], start = c(1994,214), freq = 365)

HK.pred <- ts(VAR.pred$fcst$HongKong[,1], start = c(1994,214), freq = 365)
HK.lb <- ts(VAR.pred$fcst$HongKong[,2], start = c(1994,214), freq = 365)
HK.ub <- ts(VAR.pred$fcst$HongKong[,3], start = c(1994,214), freq = 365)

Ja.pred <- ts(VAR.pred$fcst$Japan[,1], start = c(1994,214), freq = 365)
Ja.lb <- ts(VAR.pred$fcst$Japan[,2], start = c(1994,214), freq = 365)
Ja.ub <- ts(VAR.pred$fcst$Japan[,3], start = c(1994,214), freq = 365)

Si.pred <- ts(VAR.pred$fcst$Singapore[,1], start = c(1994,214), freq = 365)
Si.lb <- ts(VAR.pred$fcst$Singapore[,2], start = c(1994,214), freq = 365)
Si.ub <- ts(VAR.pred$fcst$Singapore[,3], start = c(1994,214), freq = 365)

NY.pred <- ts(VAR.pred$fcst$NewYork[,1], start = 1998, freq = 1)
NY.lb <- ts(VAR.pred$fcst$NewYork[,2], start = 1998, freq = 1)
NY.ub <- ts(VAR.pred$fcst$NewYork[,3], start = 1998, freq = 1)
#cbind(window(ts(data, start = c(1986,1), frequency = 365), start = c(1994,1)),
win.ts <- window(stock, start = c(1993, 213))
par(mfrow=c(2,4))
ts.plot(cbind(Am.pred, Am.lb, Am.ub), lty = c(1,2,2), col=c("blue","red","red"), main = "predictions for Amsterdam", l=c("fewf"))
ts.plot(cbind(Fr.pred, Fr.lb, Fr.ub), lty = c(1,2,2), col=c("blue","red","red"), main = "predictions for Frankfurt")
ts.plot(cbind(Lo.pred, Lo.lb, Lo.ub), lty = c(1,2,2), col=c("blue","red","red"), main = "predictions for London")
ts.plot(cbind(HK.pred, HK.lb, HK.ub), lty = c(1,2,2), col=c("blue","red","red"), main = "predictions for Hong Kong")
ts.plot(cbind(Ja.pred, Ja.lb, Ja.ub), lty = c(1,2,2), col=c("blue","red","red"), main = "predictions for Japan")
ts.plot(cbind(Si.pred, Si.lb, Si.ub), lty = c(1,2,2), col=c("blue","red","red"), main = "predictions for Singapore")
ts.plot(cbind(NY.pred, NY.lb, NY.ub), lty = c(1,2,2), col=c("blue","red","red"), main = "predictions for New York")
dev.off()
VAR.pred <- predict(VAR.data, n.ahead=365)
plot(VAR.pred)


## F
# do impulse reposnse analysis of a shock to US market
irf.iny = irf(VAR.data, impulse = "NewYork", response = c("Amsterdam","Frankfurt","London","HongKong","Japan","Singapore","NewYork"), boot = FALSE, n.ahead = 100)
plot(irf.iny)
## repeat for Japan's
irf.iny = irf(VAR.data, impulse = "Japan", response = c("Amsterdam","Frankfurt","London","HongKong","Japan","Singapore","NewYork"), boot = FALSE, n.ahead = 100)
plot(irf.iny)
## repeat for London
irf.iny = irf(VAR.data, impulse = "London", response = c("Amsterdam","Frankfurt","London","HongKong","Japan","Singapore","NewYork"), boot = FALSE, n.ahead = 100)
plot(irf.iny)


## EXTRA
gimme <- function(x,y) {
  return(.2*x - .8*y)
}
