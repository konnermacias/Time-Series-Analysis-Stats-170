#### Homework 6 - Stat 170
#### Konner Macias - 004603916
#
#
### Problem 1 [7(b)-(e)]

# load data
Global=scan("http://www.stat.ucla.edu/~jsanchez/data/global.dat.txt")
Global.ts =ts(Global,st=c(1856,1),end=c(2005,12),fr=12)

plot(Global.ts,main="Global temperature series",ylab="temperature (departures from monthly means)")
# seasonal boxplot
boxplot(Global.ts~cycle(Global.ts))

## (b)
gl.decom = decompose(Global.ts, type = 'additive')
# produce a plot with superimposed seasonal effect
plot(gl.decom)
trend = gl.decom$trend
seasonal = gl.decom$seasonal
ts.plot(cbind(trend, trend+seasonal), lty=1:2, main = 'Trend with superimposed seasonal effect')

## (c) plot correlogram
acf(gl.decom$random, na.action = na.pass, main = 'ACF of Residuals')

## (d) HoltWinters
gl.hw <- HoltWinters(Global.ts, seasonal = 'additive')
gl.hw ; gl.hw$coef ; gl.hw$SSE

## (e) forecast
plot(gl.hw)
gl.predict <- predict(gl.hw, n.ahead = 5 * 12)
ts.plot(Global.ts, gl.predict, lty = 1:2, col = c('black','red'), main = 'Forecast of Fitted Model') # hit window

### Problem 2
MS=read.csv("http://www.stat.ucla.edu/~jsanchez/data/MorgStan.csv", header=T)
MS.ts = ts(MS)

#install.packages("dlm")
library("dlm")

sigma2=c(3.1, 1.2)  
buildMS <- function(sigma2) dlmModPoly(order=1, 
                                         dV = sigma2[1], dW = sigma2[2])
plot(MS.ts)

## the function buildNile is then used to fit the model 
fitMS=dlmMLE(MS.ts, parm=rep(100,2), build=buildMS,lower=rep(1e-9,2), hessian=T)
fitMS$par  # estimates of W and  V 

modMS <- buildMS(fitMS$par)
modMS

### Fitted values of theta 

filterMS <- dlmFilter(MS.ts, modMS)
filterMS  # do not print this. all iterations 
##check residuals 
plot(residuals(filterMS, sd = FALSE), type = "o",
     ylab = "Standardized prediction error")
abline(h = 0)

summary(residuals(filterMS)) #see names of object
acf(residuals(filterMS)$res, main = 'ACF of residuals of fitted model')

## smoothed parameter values 
smoothMS <- dlmSmooth(MS.ts, modMS)
summary(smoothMS)

### smoother with confidence bands
hwidth <- qnorm(0.05, lower = FALSE) *
  + sqrt(unlist(dlmSvd2var(smoothMS$U.S, smoothMS$D.S)))

sm <- cbind(smoothMS$s, as.vector(smoothMS$s) + hwidth %o% c(-1, 1))
class(sm)

####Plots of fitted and smoothed thetas
dev.off()
plot(MS.ts, main="Filtered and smoothed ")
##add smoother and confidence bands
lines(sm[,1],col="green",lty=c(1))
lines(sm[,2],col="red",lty=c(2))
lines(sm[,3],col="red", lty=c(2))

## Add filtered values to the plot 
lines(filterMS$f, col="blue",lty=c(3))
legend("topright", lty=c(1,2,2,3),
       col = c("green", "red", "red", "blue"), 
       legend = c("smoothed", "smoothed CIL",
                  "smoothed CIU", "filtered"
       ) 
)


fhwidth <- qnorm(0.05, lower = FALSE) *
  + sqrt(unlist(dlmSvd2var(filterMS$U.C, filterMS$D.C)))

fm <- cbind(filterMS$s, as.vector(filterMS$s) + fhwidth %o% c(-1, 1))
class(fm)




####Plots of fitted and smoothed thetas
v <- unlist(dlmSvd2var(filterMS$U.C, filterMS$D.C))
pl <- dropFirst(filterMS$m) + qnorm(0.05, sd = sqrt(v[-1]))
pu <- dropFirst(filterMS$m) + qnorm(0.95, sd = sqrt(v[-1]))

plot(MS.ts, main="Filtered and smoothed ")
##add smoother and confidence bands
lines(sm[,1],col="green",lty=c(1))


## Add filtered values to the plot 
lines(filterMS$m, col="blue",lty=c(3))
lines(pl,col="red",lty=c(2))
lines(pu,col="red", lty=c(2))

legend("topright", lty=c(1,2,2,3),
       col = c("green", "red", "red", "blue"), 
       legend = c("smoothed", "filtered CIL",
                  "filtered CIU", "filtered"
       ) 
)

# nope
foreMS <- dlmForecast(filterMS, nAhead = 10)
v <- unlist(dlmSvd2var(filterMS$U.C, filterMS$D.C))
pl <- dropFirst(filterMS$m) + qnorm(0.05, sd = sqrt(v[-1]))
attach(foreMS)
hwidth <- qnorm(0.25, lower = FALSE) * sqrt(unlist(Q))
fore <- cbind(f, as.vector(f) + hwidth %o% c(-1, 1))
class(fore)
rg <- range(c(fore, MS.ts))
plot(fore, type = "o", pch = 16, plot.type = "s", lty = c(1, 3, 3),
     ylab = "MS level", xlab = "", ylim = rg)
lines(MS.ts)
lines(smoothMS$s, lty = 5)
abline(v = mean(c(time(f)[1], tail(time(MS.ts), 1))),
       lty = "dashed", col = "darkgrey")
legend("topleft", lty = c(1, 5, 1, 3), pch = c(1, NA, 16, 16), bty = "n",
       legend = c("observed level", "smoothed level", "forecasted level",
                  "50% probability limits"))
detach(foreMS)