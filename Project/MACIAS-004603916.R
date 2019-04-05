### Mini Project
### Stats 170 - Time Series Analysis
### Konner Macias
### 004603916


### section 2
# read in the time data
pData = read.table("http://www.stat.ucla.edu/~jsanchez/data/hwk6data.csv", sep = ",", header = T)
train = pData[1:(nrow(pData)-12),]
test = pData[(nrow(pData)-11):nrow(pData),]

# get mts object
data = ts(train, class ="mts", start = c(1959,1), frequency = 12)

# create 3 ts objects
hs = ts(train$HOUSTNSA, start = c(1959,1), frequency = 12) # deleted class mts
uw = ts(train$LNU04000002, start = c(1959,1), frequency = 12)
ur = ts(train$UNRATENSA, start = c(1959,1), frequency = 12)

### section 3
# decompose, seasonal box plots, and time plots
data.decom = decompose(hs)
plot(data.decom)

data.decom = decompose(uw)
plot(data.decom)

data.decom = decompose(ur)
plot(data.decom)

# boxplot
boxplot(hs ~ cycle(hs), main = 'Seasonal Boxplot of hs')
boxplot(uw ~ cycle(uw), main = 'Seasonal Boxplot of uw')
boxplot(ur ~ cycle(ur), main = 'Seasonal Boxplot of ur')

# plot time series
ts.plot(hs)
ts.plot(uw)
ts.plot(ur)

# unit root tests
# install.packages(tseries)
library(tseries)
adf.test(hs)
adf.test(uw)
adf.test(ur)

# the null hypothesis fails to be rejected for all

# cointegration tests
po.test(cbind(hs, uw)) # hs and uw are cointegrated
po.test(cbind(hs, ur)) # hs and ur are cointegrated
po.test(cbind(uw, ur)) # uw and ur are cointegrated

# volatility checking
acf(data - mean(data))
acf((data - mean(data))^2)



### section 4
# transformation?
hs.log = log(hs)
hs.sqrt = sqrt(hs)
plot.ts(cbind(hs, hs.log, hs.sqrt), main = 'Org. vs. Log vs. Sqrt Transform')

par(mfrow=c(1,2))
acf(hs.log, main = 'ACF of hs*')
pacf(hs.log, main = 'PACF of hs*')

hs.diff.l = diff(hs.log, lag = 1, diff = 1)
acf(hs.diff.l, main = 'ACF of Diff hs*')
pacf(hs.diff.l, main = 'PACF of Diff hs*')

hs.seas.l = diff(hs.log, lag = 12, diff = 1)
acf(hs.seas.l, main = 'ACF of Seas. Diff hs*')
pacf(hs.seas.l, main = 'PACF of Seas. Diff hs*')

hs.seas.diff.l = diff(hs.diff.l, lag = 12, diff = 1)
acf(hs.seas.diff.l, main = 'ACF of Seas-Reg. Diff hs*')
pacf(hs.seas.diff.l, main = 'PACF of Seas-Reg. Diff hs*')
dev.off()

hs.star.star = hs.seas.diff.l


par(mfrow=c(2,2))
acf(arima(hs.log, order=c(0,1,0), seas=list(order=c(0,1,0),12))$resid, main = 'ACF of Res of M1')
acf(arima(hs.log, order=c(0,1,0), seas=list(order=c(1,1,0),12))$resid, main = 'ACF of Res of M2')
acf(arima(hs.log, order=c(0,1,0), seas=list(order=c(0,1,1),12))$resid, main = 'ACF of Res of M3')
acf(arima(hs.log, order=c(0,1,0), seas=list(order=c(1,1,1),12))$resid, main = 'ACF of Res of M4')
acf(arima(hs.log, order=c(0,1,1), seas=list(order=c(0,1,0),12))$resid, main = 'ACF of Res of M5')
acf(arima(hs.log, order=c(0,1,1), seas=list(order=c(1,1,0),12))$resid, main = 'ACF of Res of M6')
acf(arima(hs.log, order=c(0,1,1), seas=list(order=c(0,1,1),12))$resid, main = 'ACF of Res of M7') # keep
acf(arima(hs.log, order=c(0,1,1), seas=list(order=c(1,1,1),12))$resid, main = 'ACF of Res of M8') # keep
acf(arima(hs.log, order=c(1,1,0), seas=list(order=c(0,1,0),12))$resid, main = 'ACF of Res of M9')
acf(arima(hs.log, order=c(1,1,0), seas=list(order=c(1,1,0),12))$resid, main = 'ACF of Res of M10')
acf(arima(hs.log, order=c(1,1,0), seas=list(order=c(0,1,1),12))$resid, main = 'ACF of Res of M11')
acf(arima(hs.log, order=c(1,1,0), seas=list(order=c(1,1,1),12))$resid, main = 'ACF of Res of M12')
acf(arima(hs.log, order=c(1,1,1), seas=list(order=c(0,1,0),12))$resid, main = 'ACF of Res of M13')
acf(arima(hs.log, order=c(1,1,1), seas=list(order=c(1,1,0),12))$resid, main = 'ACF of Res of M14')
acf(arima(hs.log, order=c(1,1,1), seas=list(order=c(0,1,1),12))$resid, main = 'ACF of Res of M15') # keep
acf(arima(hs.log, order=c(1,1,1), seas=list(order=c(1,1,1),12))$resid, main = 'ACF of Res of M16') # keep
dev.off()

sar7 = arima(hs.log, order=c(0,1,1), seas=list(order=c(0,1,1),12))
sar8 = arima(hs.log, order=c(0,1,1), seas=list(order=c(1,1,1),12))
sar11 = arima(hs.log, order=c(0,1,1), seas=list(order=c(0,1,1),12))
sar12 = arima(hs.log, order=c(0,1,1), seas=list(order=c(1,1,1),12))
sar15 = arima(hs.log, order=c(1,1,1), seas=list(order=c(0,1,1),12))
sar16 = arima(hs.log, order=c(1,1,1), seas=list(order=c(1,1,1),12))


# look at p-values of coefficients
(1 - pnorm(abs(sar7$coef)/sqrt(diag(sar7$var.coef))))*2
(1 - pnorm(abs(sar8$coef)/sqrt(diag(sar8$var.coef))))*2
(1 - pnorm(abs(sar11$coef)/sqrt(diag(sar11$var.coef))))*2
(1 - pnorm(abs(sar12$coef)/sqrt(diag(sar12$var.coef))))*2
(1 - pnorm(abs(sar15$coef)/sqrt(diag(sar15$var.coef))))*2
(1 - pnorm(abs(sar16$coef)/sqrt(diag(sar16$var.coef))))*2
# can't eliminate any

# let's forecast
sar7
pred7 = predict(sar7, n.ahead = 12, se.fit = T)
rmse7 = sqrt(sum((test$HOUSTNSA-exp(pred7$pred[1:12]))^2)/12)
rmse7

sar8
pred8 = predict(sar8, n.ahead = 12, se.fit = T)
rmse8 = sqrt(sum((test$HOUSTNSA-exp(pred8$pred[1:12]))^2)/12)
rmse8

sar11
pred11 = predict(sar11, n.ahead = 12, se.fit = T)
rmse11 = sqrt(sum((test$HOUSTNSA-exp(pred11$pred[1:12]))^2)/12)
rmse11

sar12
pred12 = predict(sar12, n.ahead = 12, se.fit = T)
rmse12 = sqrt(sum((test$HOUSTNSA-exp(pred12$pred[1:12]))^2)/12)
rmse12


sar15
pred15 = predict(sar15, n.ahead = 12, se.fit = T)
rmse15 = sqrt(sum((test$HOUSTNSA-exp(pred15$pred[1:12]))^2)/12)
rmse15

sar16
pred16 = predict(sar16, n.ahead = 12, se.fit = T)
rmse16 = sqrt(sum((test$HOUSTNSA-exp(pred16$pred[1:12]))^2)/12)
rmse16


# forecast with actual values
sar15.pred = exp(pred15$pred)
cil = exp(ts(pred15$pred - 1.96 * pred15$se, frequency = 12, start = c(2017,9)))
ciu = exp(ts(pred15$pred + 1.96 * pred15$se, frequency = 12, start = c(2017,9)))
ts.plot(cbind(window(ts(train$HOUSTNSA, start = c(1959,1), frequency = 12), start=c(2010,1)), ts(sar15.pred, start = c(2017,9), frequency = 12), ts(test$HOUSTNSA, start = c(2017,9), frequency = 12), cil, ciu),col=c("blue", "green", "black","red","red"), ylab="y_t", lty = c(1,2,1,3,3),
        main = "Forecast of Identified Model")



### secion 5 - regression w/autocorrelated errors
# fit regression model with other variables as independent vars
lm1 = lm(hs ~ uw + ur)
summary(lm1)

# strong seasonality -> fit dummies
months = cycle(hs)
mod1 = lm(hs ~ uw + ur + factor(months))
summary(mod1)

# compare fitted values in-sample with actual data
plot(ts(fitted(mod1), freq = 12, start = c(1959,1)), col = "red", ylab="data and fitted values",
     type = "l", ylim = range(c(fitted(mod1), hs)), main = 'Fitted vs. In sample')
lines(hs)

# actual values have more of a trend
# check diagnostics
par(mfrow = c(2,2))
plot(y = rstudent(mod1), x = as.vector(time(hs)), xlab = "time", ylab = "Standardized residuals", type = "l")
abline(h=0)
acf(rstudent(mod1))
hist(rstudent(mod1), xlab = "Standardize Residuals")
qqnorm(rstudent(mod1))
dev.off()

# check acf and pacf to see what modlel to fit
par(mfrow=c(1,2))
acf(ts(rstudent(mod1)), main = 'ACF of Resid')
pacf(ts(rstudent(mod1)), main = 'PACF of Resid')
dev.off()

# looks like MA(1), possible AR(1)
modres1 = arima(ts(rstudent(mod1)), order = c(1,0,0))
modres1
acf(modres1$residuals, main = 'ACF of Resid')

# now first gls
library(nlme)
glsmod = gls(hs ~ uw + ur + factor(months), correlation = corARMA(c(0.9189), p=1))
glsmod
acf(ts(residuals(glsmod, type = 'normalized')), main = 'ACF of Resid of Gls')

# now use a time variable
times = time(hs)
glsmod2 = gls(hs ~ uw + ur + factor(months) + times, correlation = corARMA(c(0.9189), p=1))
glsmod2

# look at diagnostics
plot(y = residuals(glsmod2), x = as.vector(time(hs)), xlab = 'time', type = 'l')
abline(h=0)
acf(ts(residuals(glsmod2, type = 'normalized')), main = 'ACF of Res of New Gls')

# forecast
new.t = seq(2017.667, len = 12, by = 1/12)
new.dat = data.frame(uw = test$LNU04000002, ur = test$UNRATENSA, months = rep(1:12,1), times = new.t)
pred.1 = predict(glsmod, new.dat)[1:12]
pred.2 = predict(glsmod2, new.dat)[1:12]

# predicted vals
hat.x.ts.1 = ts(pred.1, start = c(2017,9), freq = 12)
hat.x.ts.2 = ts(pred.2, start = c(2017,9), freq = 12)

ts.plot(hs, hat.x.ts.1, lty = 1:2, main = 'glsmodel forecast', ylab = 'Value of data', xlab = 'Time')
ts.plot(hs, hat.x.ts.2, lty = 1:2, main = 'glsmodel2 forecast', ylab = 'Value of data', xlab = 'Time')

# rmse
sqrt(sum((test$HOUSTNSA - pred.1)^2)/12)
sqrt(sum((test$HOUSTNSA - pred.2)^2)/12)



### section 6 - VAR model
plot(data)

# look at acf and ccf of raw data
acf(data)
acf(diff(data))

library(vars)
VAR.tr = VAR(data, p = 4, type = 'trend')
coef(VAR.tr)
acf(resid(VAR.tr))

# forecast
forecast <- predict(VAR.tr, n.ahead = 12)
plot(forecast)
# rmse
sqrt(sum((test$HOUSTNSA - forecast$fcst$HOUSTNSA[,1])^2)/12)
# impulse analysis
# shock to hs
irf.hs = irf(VAR.tr, impulse = 'HOUSTNSA',
             response = c("HOUSTNSA","LNU04000002","UNRATENSA"),
             boot = F,
             n.ahead = 60)
plot(irf.hs)
# shock to uw
irf.uw = irf(VAR.tr, impulse = 'LNU04000002',
             response = c("HOUSTNSA","LNU04000002","UNRATENSA"),
             boot = F,
             n.ahead = 60)
plot(irf.uw)
# shock to ur
irf.ur = irf(VAR.tr, impulse = 'UNRATENSA',
             response = c("HOUSTNSA","LNU04000002","UNRATENSA"),
             boot = F,
             n.ahead = 60)
plot(irf.ur)

### section 7
avgPreds = (pred.2 + sar15.pred[1:12] + forecast$fcst$HOUSTNSA[,1])/3
avgPreds
rmseAvg = sqrt(sum((test$HOUSTNSA - avgPreds)^2)/12)
rmseAvg