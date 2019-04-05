#### Homework 4
#### Konner Macias
#### 004603916

## a
predatorprey = read.table("http://www.stat.ucla.edu/~jsanchez/data/predatorprey.csv", sep = ",", header = T)
newdata = ts(predatorprey, class ="mts")

## b
ts.plot(newdata, lty = c(1,3), main = "Bivariate time plot")
legend(30,340, c("Prey", "Predator"), lty = c(1,3))

## c
acf(newdata)

## e
# fit var
install.packages('vars')
library(vars)
VAR.predprey = VAR(newdata, p = 2)
coef(VAR.predprey)


## f
Mod(polyroot(c(1,-2.319,2.344,-1.125,0.237)))

## g
par(mfrow = c(1,2))
acf(resid(VAR.predprey)[,1], main = "Res of VAR(2), Prey")
acf(resid(VAR.predprey)[,2], main = "Res of VAR(2), Predator")
dev.off()


## h
VAR.preds <- predict(VAR.predprey, n.ahead = 4)
VAR.preds
prey.preds <- ts(VAR.preds$fcst$Prey[,1], st = 51)
pred.preds <- ts(VAR.preds$fcst$Predator[,1], st = 51)
ts.plot(cbind(ts(newdata[,1]), prey.preds), lty = c(1,2), main = "Forecast of Prey, dashed line")

## i
# give a shock
library(vars)
VAR.predprey.c <- VAR(newdata, p = 2, type = "const")

irf = irf (VAR.predprey.c, impulse = "Prey", response = c("Prey","Predator"), boot = F, n.ahead = 60)
plot(irf)

irf = irf (VAR.predprey.c, impulse = "Predator", response = c("Prey","Predator"), boot = F, n.ahead = 60)
plot(irf)


## question 2
# read in formula with coefficients
# pass that into irf


