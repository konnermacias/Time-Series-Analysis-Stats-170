### HW2 Stats 170 10/22
### Last name: Macias  First name: Konner  uid: 004603916
#########################################################

####### Problem 1
set.seed(9876543)
### a
##### AR(1) with alpha = -0.9
y.1 = ts(arima.sim(n = 100, list(ar = c(-0.9)), innov = rnorm(100)))
y.1.ar = arima(y.1, order = c(1,0,0))
y.1.pred = ts(predict(y.1.ar, n.ahead = 10, se.fit = T))
cil.1 = ts((y.1.pred$pred - 1.96 * y.1.pred$se), start = 101)
ciu.1 = ts((y.1.pred$pred + 1.96 * y.1.pred$se), start = 101)
ts.plot(cbind(y.1, y.1.pred$pred, cil.1, ciu.1), lty = c(1,2,3,3),
         col = c('blue','green','red','red'), ylab = 'y_t',
         main = 'AR(1) coefficient -0.9')

##### AR(1) with alpha = -0.5
y.2 = ts(arima.sim(n = 100, list(ar = c(-0.5)), innov = rnorm(100)))
y.2.ar = arima(y.2, order = c(1,0,0))
y.2.pred = ts(predict(y.2.ar, n.ahead = 10, se.fit = T))
cil.2 = ts((y.2.pred$pred - 1.96 * y.2.pred$se), start = 101)
ciu.2 = ts((y.2.pred$pred + 1.96 * y.2.pred$se), start = 101)
ts.plot(cbind(y.2, y.2.pred$pred, cil.2, ciu.2), lty = c(1,2,3,3),
        col = c('blue','green','red','red'), ylab = 'y_t',
        main = 'AR(1) coefficient -0.5')

##### AR(1) with alpha = 0.5
y.3 = ts(arima.sim(n = 100, list(ar = c(0.5)), innov = rnorm(100)))
y.3.ar = arima(y.3, order = c(1,0,0))
y.3.pred = ts(predict(y.3.ar, n.ahead = 10, se.fit = T))
cil.3 = ts((y.3.pred$pred - 1.96 * y.3.pred$se), start = 101)
ciu.3 = ts((y.3.pred$pred + 1.96 * y.3.pred$se), start = 101)
ts.plot(cbind(y.3, y.3.pred$pred, cil.3, ciu.3), lty = c(1,2,3,3),
        col = c('blue','green','red','red'), ylab = 'y_t',
        main = 'AR(1) coefficient 0.5')

##### AR(1) with alpha = 0.9
y.4 = ts(arima.sim(n = 100, list(ar = c(0.9)), innov = rnorm(100)))
y.4.ar = arima(y.4, order = c(1,0,0))
y.4.pred = ts(predict(y.4.ar, n.ahead = 10, se.fit = T))
cil.4 = ts((y.4.pred$pred - 1.96 * y.4.pred$se), start = 101)
ciu.4 = ts((y.4.pred$pred + 1.96 * y.4.pred$se), start = 101)
ts.plot(cbind(y.4, y.4.pred$pred, cil.4, ciu.4), lty = c(1,2,3,3),
        col = c('blue','green','red','red'), ylab = 'y_t',
        main = 'AR(1) coefficient 0.9')

### b
#### alpha = 1.01
y.11 <- w.11 <- rnorm(100)
for (t in 2:100) y.11[t] = 1.01 * y.11[t - 1] + w.11[t]
y.11 = ts(y.11)
y.11.ar = arima(y.11, order = c(1, 0, 0))
y.11.pred = ts(predict(y.11.ar, n.ahead = 10, se.fit = TRUE))
cil.11 = ts((y.11.pred$pred - 1.96 * y.11.pred$se), start = 101)
ciu.11 = ts((y.11.pred$pred + 1.96 * y.11.pred$se), start = 101)
ts.plot(cbind(y.11, y.11.pred$pred, cil.11, ciu.11),col=c("blue", "green", "red","red"), ylab="y_t", lty = c(1,2,3,3),
        main = "Alpha = 1.01")
Mod(polyroot(c(1,-1.01)))

#### alpha = 1.02
y.12 <- w.12 <- rnorm(100)
for (t in 2:100) y.12[t] = 1.02 * y.12[t - 1] + w.12[t]
y.12 = ts(y.12)
y.12.ar = arima(y.12, order = c(1, 0, 0))
y.12.pred = ts(predict(y.12.ar, n.ahead = 10, se.fit = TRUE))
cil.12 = ts((y.12.pred$pred - 1.96 * y.12.pred$se), start = 101)
ciu.12 = ts((y.12.pred$pred + 1.96 * y.12.pred$se), start = 101)
ts.plot(cbind(y.12, y.12.pred$pred, cil.12, ciu.12),col=c("blue", "green", "red","red"), ylab="y_t", lty = c(1,2,3,3),
        main = "Alpha = 1.02")
Mod(polyroot(c(1,-1.02)))

#### alpha = 1.05
set.seed(9876543)
y.15 <- w.15 <- rnorm(100)
for (t in 2:100) y.15[t] = 1.05 * y.15[t - 1] + w.15[t]
y.15 = ts(y.15)
tryCatch({
y.15.ar = arima(y.15, order = c(1, 0, 0))
y.15.pred = ts(predict(y.15.ar, n.ahead = 10, se.fit = TRUE))
cil.15 = ts((y.15.pred$pred - 1.96 * y.15.pred$se), start = 101)
ciu.15 = ts((y.15.pred$pred + 1.96 * y.15.pred$se), start = 101)
ts.plot(cbind(y.11, y.11.pred$pred, cil.11, ciu.11),col=c("blue", "green", "red","red"), ylab="y_t", lty = c(1,2,3,3),
        main = "Alpha = 1.05")
Mod(polyroot(c(1,-1.05)))
}, error = function(e) {
  paste0("Recommend re-running")
})

## c
# perform differencing of the nonstationary series
#### alpha = 1.01
y.11.d = diff(y.11, lag = 1, diff = 1)
acf(y.11.d, main = 'ACF of Regular diff | Alpha = 1.01')
y.11.ar.d = arima(y.11.d, order = c(1, 1, 0))
y.11.pred.d = ts(predict(y.11.ar.d, n.ahead = 10, se.fit = TRUE))
cil.11.d = ts((y.11.pred.d$pred - 1.96 * y.11.pred.d$se), start = 101)
ciu.11.d = ts((y.11.pred.d$pred + 1.96 * y.11.pred.d$se), start = 101)
ts.plot(cbind(y.11.d, y.11.pred.d$pred, cil.11.d, ciu.11.d),col=c("blue", "green", "red","red"), ylab="y_t", lty = c(1,2,3,3),
        main = "Reg Diff | Alpha = 1.01")

### alpha = 1.02
y.12.d = diff(y.12, lag = 1, diff = 1)
acf(y.12.d, main = 'ACF of Regular diff | Alpha = 1.02')
y.12.ar.d = arima(y.12.d, order = c(1, 1, 0))
y.12.pred.d = ts(predict(y.12.ar.d, n.ahead = 10, se.fit = TRUE))
cil.12.d = ts((y.12.pred.d$pred - 1.96 * y.12.pred.d$se), start = 101)
ciu.12.d = ts((y.12.pred.d$pred + 1.96 * y.12.pred.d$se), start = 101)
ts.plot(cbind(y.12.d, y.12.pred.d$pred, cil.12.d, ciu.12.d),col=c("blue", "green", "red","red"), ylab="y_t", lty = c(1,2,3,3),
        main = "Reg Diff | Alpha = 1.02")

### alpha = 1.05
y.15.d = diff(y.15, lag = 1, diff = 2)
acf(y.15.d, main = 'ACF of 2 Regular diff | Alpha = 1.05')
pacf(y.15.d, main = 'ACF of Regular diff | Alpha = 1.05')
y.15.ar.d = arima(y.15.d, order = c(1, 2, 0))
y.15.pred.d = ts(predict(y.15.ar.d, n.ahead = 10, se.fit = TRUE))
cil.15.d = ts((y.15.pred.d$pred - 1.96 * y.15.pred.d$se), start = 101)
ciu.15.d = ts((y.15.pred.d$pred + 1.96 * y.15.pred.d$se), start = 101)
ts.plot(cbind(y.15.d, y.15.pred.d$pred, cil.15.d, ciu.15.d),col=c("blue", "green", "red","red"), ylab="y_t", lty = c(1,2,3,3),
        main = "Reg Diff | Alpha = 1.05")

### Problem 2

## a
set.seed(9876543)
x <- arima.sim(n = 1000, list(order = c(2,0,0),
                              ar = c(5/6,-1/6),
                              sd = 1))
plot(x)


## b
par(mfrow=c(1,2))
acf(x, main = "ACF of Simulated data")
pacf(x, main = "PACF of Simulated data")


## c
# fit an AR model
x.ar <- arima(x, order = c(2,0,0))
x.pred = predict(x.ar, se.fit = T)

## d
cil.x = ts((x.pred$pred - 1.96 * x.pred$se), start = 667)
ciu.x = ts((x.pred$pred + 1.96 * x.pred$se), start = 667)

## f - acf of residuals
acf(x.ar$residuals, main = "Correlogram of Residuals")
hist(x.ar$residuals, main = "Histogram of Residuals")




### Problem 3

## c
x.3 <- arima.sim(n = 1000, list(order = c(1,1,0), ar = 1/2, sd = 1))
y.3 <- x.3[2:1000] - x.3[1:999]

## d
y.3.AR <- arima(y.3, order = c(2,0,0))
y.3.pred = predict(y.3.AR, se.fit = T)
cil.y.3 = ts((y.3.pred$pred - 1.96 * y.3.pred$se), start = 667)
ciu.y.3 = ts((y.3.pred$pred + 1.96 * y.3.pred$se), start = 667)

## e
acf(y.3.AR$residuals, main = "Correlogram of Residuals")
hist(y.3.AR$residuals, main = "Histogram of Residuals")

### Problem 4

## a

# read in data and plot as time series
par(mfrow = c(1,1))
www = "http://www.stat.ucla.edu/~jsanchez/data/cbe.txt"
cbe = read.table(www, header = T)
elec.ts = ts(cbe[,3], start = 1958, freq = 12)

# look at time plots
par(mfrow=c(3,1))
plot.ts(elec.ts, main = "time plot of regular data")
# log transformation
elec.log <- log(elec.ts)
plot(elec.log, main = "time plot of logged data")
# sqrt transformation
elec.sqrt <- sqrt(elec.ts)
plot(elec.sqrt, main = "time plot of sqrt data")

# regular differencing
par(mfrow = c(1,1))
elec.star = diff(elec.sqrt, lag = 1, diff = 1)
acf(elec.star, main = "Correlogram of Reg Diff Data")

# just seasonal differencing
elec.sea = diff(elec.sqrt, lag = 12, diff = 1)
acf(elec.sea, main = "Correlogram of Sea. Diff data")

# seaonal after regular differencing
elec.star.star = diff(elec.star, lag = 12, diff = 1)
acf(elec.star.star, main = "Correlogram of Sea. Reg. Diff Data")
pacf(elec.star.star, main = "PACF of Sea. Reg. Diff Data")

# looks like MA(1)
elec.ar = arima(elec.sqrt, order = c(1,1,1), seas = list(order=c(1,1,1), 12))
par(mfrow = c(1,1))
acf(elec.ar$residuals, main = "ACF of model")
hist(elec.ar$residuals, main = "Residuals Distribution")
elec.ar

# forecast and make predictions
elec.pred = predict(elec.ar, n.ahead = 10, se.fit = T)
cil.elec = ts((elec.pred$pred - 1.96 * elec.pred$se), frequency = 12, start = 1991)
ciu.elec = ts((elec.pred$pred + 1.96 * elec.pred$se), frequency = 12, start = 1991)
ts.plot(cbind(window(elec.sqrt, start = c(1985,1)), elec.pred$pred, cil.elec, ciu.elec),col=c("blue", "green", "red","red"), ylab="y_t", lty = c(1,2,3,3),
        main = "Forecast of Identified Model")


