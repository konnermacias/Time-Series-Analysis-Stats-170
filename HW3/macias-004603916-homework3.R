#### Homework 3 - Stat 170
#### Macias, Konner - 004603916
#### October 30th, 2018

## prelude
# read in data
www = "http://www.stat.ucla.edu/~jsanchez/data/osvisit.dat"
osvisit <- read.table(www, header = F)
head(osvisit)
# split into training and test
osv.test = osvisit$V1[217:228] ## testing set
osv.train = osvisit$V1[1:216]  ## training set

## a
# take sqrt of data
y.star = sqrt(ts(osv.train, start=c(1977,1), freq = 12))
plot.ts(cbind(osv.train, y.star), main = 'time plot of Orig. vs. sqrt data')

## b
acf(y.star)


## c
# reg diff only
y.star.diff1 = diff(y.star, lag = 1, diff = 1)
acf(y.star.diff1, main = 'ACF of Reg. Diff. Data')
pacf(y.star.diff1, main = 'PACF of Reg. Diff. Data')
# seas diff only
y.star.seas = diff(y.star, lag = 12, diff = 1)
acf(y.star.seas, main = 'ACF of Seas. Diff. Data')
pacf(y.star.seas, main = 'PACF of Seas Diff. Data')
# seas of reg diff
y.star.star = diff(y.star.diff1, lag = 12, diff = 1)
acf(y.star.star, main = 'ACF of Seas-Reg Diff. Data')
pacf(y.star.star, main = 'PACF of Seas-Reg Diff. Data')

## d
par(mfrow=c(2,2))
y.sar1 = arima(y.star, order=c(1,1,0), seas=list(order = c(1,1,0),12))
acf(y.sar1$residuals, main = 'ACF of Residuals of Model1')
pacf(y.sar1$residuals, main = 'PACF of Residuals of Model 1')
y.sar2 = arima(y.star, order=c(0,1,1), seas=list(order = c(0,1,1),12))
acf(y.sar2$residuals, main = 'ACF of Residuals of Model 2')
pacf(y.sar2$residuals, main = 'PACF of Residuals of Model 2')

## e - fit the models
y.e.sar1 = arima(y.star, order=c(1,1,0), seas=list(order = c(0,1,0),12))
y.e.sar2 = arima(y.star, order=c(1,1,0), seas=list(order = c(1,1,0),12)) # repeat
y.e.sar3 = arima(y.star, order=c(0,1,1), seas=list(order = c(0,1,1),12)) # repeat
y.e.sar4 = arima(y.star, order=c(1,1,0), seas=list(order = c(0,1,1),12))
y.e.sar5 = arima(y.star, order=c(0,1,1), seas=list(order = c(1,1,0),12))
y.e.sar6 = arima(y.star, order=c(1,1,1), seas=list(order = c(1,1,1),12))
y.e.sar7 = arima(y.star, order=c(1,1,1), seas=list(order = c(1,1,0),12))
y.e.sar8 = arima(y.star, order=c(1,1,1), seas=list(order = c(0,1,0),12)) # unsure on last param
par(mfrow=c(2,2))
acf(y.e.sar1$residuals, main = 'ACF of Res of Model 1')
acf(y.e.sar2$residuals, main = 'ACF of Res of Model 2')
acf(y.e.sar3$residuals, main = 'ACF of Res of Model 3')
acf(y.e.sar4$residuals, main = 'ACF of Res of Model 4')
acf(y.e.sar5$residuals, main = 'ACF of Res of Model 5')
acf(y.e.sar6$residuals, main = 'ACF of Res of Model 6')
acf(y.e.sar7$residuals, main = 'ACF of Res of Model 7')
acf(y.e.sar8$residuals, main = 'ACF of Res of Model 8')

# remove modes 1,2,4, and 8
# let's look at pvalue of coefficients of model
(1 - pnorm(abs(y.e.sar3$coef)/sqrt(diag(y.e.sar3$var.coef))))*2
(1 - pnorm(abs(y.e.sar5$coef)/sqrt(diag(y.e.sar5$var.coef))))*2
(1 - pnorm(abs(y.e.sar6$coef)/sqrt(diag(y.e.sar6$var.coef))))*2
(1 - pnorm(abs(y.e.sar7$coef)/sqrt(diag(y.e.sar7$var.coef))))*2

## f - create table from model output
y.e.sar3
pred3 = ts(predict(y.e.sar3, n.ahead = 12, se.fit = T))
rmse3 = sqrt((sum(osv.test-(pred3$pred)^2)^2)/12)
rmse3

y.e.sar5
pred5 = ts(predict(y.e.sar5, n.ahead = 12, se.fit = T))
rmse5 = sqrt((sum(osv.test-(pred5$pred)^2)^2)/12)
rmse5

y.e.sar6
pred6 = ts(predict(y.e.sar6, n.ahead = 12, se.fit = T))
rmse6 = sqrt((sum(osv.test-(pred6$pred)^2)^2)/12)
rmse6

y.e.sar7
pred7 = ts(predict(y.e.sar7, n.ahead = 12, se.fit = T))
rmse7 = sqrt((sum(osv.test-(pred7$pred)^2)^2)/12)
rmse7


## h
par(mfrow=c(1,1))
osv.pred = (pred6$pred)^2
cil = ts((pred6$pred - 1.96 * pred6$se), frequency = 12, start = 1995)^2
ciu = ts((pred6$pred + 1.96 * pred6$se), frequency = 12, start = 1995)^2
ts.plot(cbind(window(ts(osv.train, start = c(1977,1), frequency = 12), start=c(1990,1)), ts(osv.pred, start = c(1995,1), frequency = 12), cil, ciu),col=c("blue", "green", "red","red"), ylab="y_t", lty = c(1,2,3,3),
        main = "Forecast of Identified Model")
