### HW1 Stats 170 10/9
### Last name: Macias  First name: Konner  uid: 004603916
#########################################################

### Problem 1
set.seed(12345)
## a
par(mfrow=c(3,3)) # to put the next graphs

## b
y.1 = arima.sim(n = 10000, list(ma=-0.3)) # generate MA(1) data
plot.ts(y.1, main = "MA(1), theta = -0.3") # time plot
acf(y.1, main = "MA(1), theta = -0.3") # correlogram
pacf(y.1, main = "MA(1), theta = -0.3") # partial correlogram

y.2 = arima.sim(n = 10000, list(ma=-0.7)) # generate MA(1) data
plot.ts(y.2, main = "MA(1), theta = -0.7") # time plot
acf(y.2, main = "MA(1), theta = -0.7") # correlogram
pacf(y.2, main = "MA(1), theta = -0.7") # partial correlogram

y.3 = arima.sim(n = 10000, list(ma=-0.9)) # generate MA(1) data
plot.ts(y.3, main = "MA(1), theta = -0.9") # time plot
acf(y.3, main = "MA(1), theta = -0.9") # correlogram
pacf(y.3, main = "MA(1), theta = -0.9") # partial correlogram

## g
par(mfrow=c(1,1))
acfs <- c(
  acf(y.1)$acf[2],
  acf(y.2)$acf[2],
  acf(y.3)$acf[2]
)

acfs

## h
acfs


### Problem 4

## a
par(mfrow=c(1,1))
rooms = scan("http://www.stat.ucla.edu/~jsanchez/rooms.txt")
data.ts = ts(rooms, start=c(1977,1), frequency = 12)
plot.ts(data.ts, type="l", ylab = "data values", main = "this is a time plot of the series")

## b
rooms1 = log(rooms)
data.ts.log = ts(rooms1, start=c(1977,1), frequency = 12)
plot.ts(data.ts, type="l", ylab = "data values", main = "this is another time series")

## c
boxplot(data.ts.log ~ cycle(data.ts.log), main = "seasonal box plot of logged series rooms1", xlab="Months")

## d
rm.decom.additive = decompose(data.ts.log)
plot(rm.decom.additive)

## e
y = rm.decom.additive$random
if (class(y) != "ts") {
  y <- ts(y)
}

## f
acf(y[7:162], main = "ACF for logged data y")
pacf(y[7:162], main = "PACF for logged data y")
acf(y[7:162])$acf[c(2,4,5,6,13,14,19)]

## g
par(mfrow = c(2,2))
acf(rm.decom.additive$x, na.action = na.pass, main = "ACF for whole logged time series")
acf(rm.decom.additive$trend, na.action = na.pass, main = "ACF for trend component")
acf(rm.decom.additive$seasonal, na.action = na.pass, main = "ACF for seasonal component")
acf(rm.decom.additive$random, na.action = na.pass, main = "ACF for random component")

### Problem 5
## a
dat.1 <- c(39,35,16,18,7,22,13,18,20,9,-12,-11,-19,-9,-2,16)
dat.2 <- c(47,-26,42,-10,27,-8,16,6,-1,25,11,1,25,7,-5,3)

dat.1.ts <- ts(dat.1)
plot.ts(dat.1.ts, main = 'Time series of Serendipity Data', ylab = 'volumes')

dat.2.ts <- ts(dat.2)
plot.ts(dat.2.ts, main = 'Time series of Cagey Chardonay Data', ylab = 'volumes')

## b
par(mfrow=c(2,1))
acf(dat.1.ts, main = 'ACF of Serendipidity')
acf(dat.2.ts, main = 'ACF of Cagey Chardonay')
