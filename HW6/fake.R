############################################
# Stat 170/Sanchez  State Space code       #
#  Basic introduction to state space       # 
# Fitting a random walk with varying       #
#  coefficients                            # 
############################################

## We will use the Nile data that comes with R. 
## Hence the names given to the objects 

install.packages("dlm")
library("dlm")

###Before estimation, we must construct a 
###function with the dlm object
sigma2=c(3.1, 1.2)   # Placeholders for  V, W 
##Give the G and F and V and W matrices
buildNile <- function(sigma2) dlmModPoly(order=1, 
                                         dV = sigma2[1], dW = sigma2[2])

### We will use the Nile data 

help(Nile)  # familiarize yourself with the Nile data set 
plot(Nile)
class(Nile)
## the function buildNile is then used to fit the model 
fitNile=dlmMLE(Nile, parm=rep(100,2), build=buildNile,lower=rep(1e-9,2), hessian=T)
fitNile$par  # estimates of W and  V 
#put estimates in the model

modNile <- buildNile(fitNile$par)
modNile

### Fitted values of theta 

filterNile <- dlmFilter(Nile, modNile)
filterNile  # do not print this. all iterations 
##check residuals 
plot(residuals(filterNile, sd = FALSE), type = "o",
     ylab = "Standardized prediction error")
abline(h = 0)

summary(residuals(filterNile)) #see names of object
acf(residuals(filterNile)$res)

## smoothed parameter values 
smoothNile <- dlmSmooth(Nile, modNile)
summary(smoothNile)

### smoother with confidence bands
hwidth <- qnorm(0.05, lower = FALSE) *
  + sqrt(unlist(dlmSvd2var(smoothNile$U.S, smoothNile$D.S)))

sm <- cbind(smoothNile$s, as.vector(smoothNile$s) + hwidth %o% c(-1, 1))
class(sm)




####Plots of fitted and smoothed thetas

plot(Nile, main="Filtered and smoothed ")
##add smoother and confidence bands
lines(sm[,1],col="green",lty=c(1))
lines(sm[,2],col="red",lty=c(2))
lines(sm[,3],col="red", lty=c(2))

## Add filtered values to the plot 
lines(filterNile$f, col="blue",lty=c(3))
legend("topright", lty=c(1,2,2,3),
       col = c("green", "red", "red", "blue"), 
       legend = c("smoothed", "smoothed CIL",
                  "smoothed CIU", "filtered"
       ) 
)



###Forecasting 

foreNile <- dlmForecast(filterNile, nAhead = 10)
foreNile
class(foreNile)
attach(foreNile)
hwidth <- qnorm(0.25, lower = FALSE) * sqrt(unlist(Q))
fore <- cbind(f, as.vector(f) + hwidth %o% c(-1, 1))
class(fore)
rg <- range(c(fore, window(Nile, start = c(1951, 1))))
plot(fore, type = "o", pch = 16, plot.type = "s", lty = c(1, 3, 3),
     ylab = "Nile level", xlab = "", xlim = c(1951, 1980), ylim = rg)
lines(window(Nile, start = c(1951, 1)), type = 'o')
lines(window(smoothNile$s, start = c(1951,1)), lty = 5)
abline(v = mean(c(time(f)[1], tail(time(Nile), 1))),
       lty = "dashed", col = "darkgrey")
legend("topleft", lty = c(1, 5, 1, 3), pch = c(1, NA, 16, 16), bty = "n",
       legend = c("observed level", "smoothed level", "forecasted level",
                  "50% probability limits"))
detach(foreNile)


####################################################################

##################Random walk with drift (NE#eds fixing) 

help(Nile)  # familiarize yourself with the Nile data set 
plot(Nile)
sigma2=c(3.1, 1.2)   # initial values for v_t and w_t 

###Before estimation, we must construct a function with the dlm object

buildNile <- function(sigma2) dlmModPoly(order=2, dV = sigma2[1], dW = c(sigma2[2],0))

buildNile 
## the function is then use to fit the model 
fitNile=dlmMLE(Nile, parm=rep(100,2), build=buildNile,lower=rep(1e-9,2), hessian=T)
fitNile$par  # estimates of W and  V 
fitNile$hessian
## Check that it is positive definite
all(eigen(fitNile$hessian, only.values = TRUE)$values > 0)
## Get the asymptotic standard errors of the coefficients 
## inverse of hessian matrix of the negative log likelihgood 
## evaluated at the MLE is the asymptotic variance-cov matrix
aVar <- solve(fitNile$hessian)
sqrt(diag(aVar))



modNile <- buildNile(fitNile$par)
modNile

## smoothed parameter values 
smoothNile <- dlmSmooth(Nile, modNile)
summary(smoothNile)

smoothNile$s
###Plot
### smoother with confidence bands

lower=0
for(i in 1:101){ lower[i] =smoothNile$U.S[[i]][1,1] }
hwidth <- qnorm(0.05, lower = FALSE) *
  + sqrt(lower, smoothNile$D.S[,1])))
sm <- cbind(smoothNile$s[,1], as.vector(smoothNile$s[,1]) + hwidth %o% c(-1, 1))
class(sm)

plot(Nile)
lines(sm[,1],col="green")
lines(sm[,2],col="red")
lines(sm[,3],col="red")


### Fitted values 

filterNile <- dlmFilter(Nile, modNile)
filterNile

## Add filtered values to the plot 
lines(filterNile$f, col="blue")
plot(residuals(filterNile, sd = FALSE), type = "o",
     ylab = "Standardized prediction error")
abline(h = 0)
###Forecasting 

foreNile <- dlmForecast(filterNile, nAhead = 10)
class(foreNile)
attach(foreNile)
hwidth <- qnorm(0.25, lower = FALSE) * sqrt(unlist(Q))
fore <- cbind(f, as.vector(f) + hwidth %o% c(-1, 1))
class(fore)
rg <- range(c(fore, window(Nile, start = c(1951, 1))))
plot(fore, type = "o", pch = 16, plot.type = "s", lty = c(1, 3, 3),
     ylab = "Nile level", xlab = "", xlim = c(1951, 1980), ylim = rg)
lines(window(Nile, start = c(1951, 1)), type = 'o')
lines(window(smoothNile$s[,1], start = c(1951,1)), lty = 5)
abline(v = mean(c(time(f)[1], tail(time(Nile), 1))),
       lty = "dashed", col = "darkgrey")
legend("topleft", lty = c(1, 5, 1, 3), pch = c(1, NA, 16, 16), bty = "n",
       legend = c("observed level", "smoothed level", "forecasted level",
                  "50% probability limits"))
detach(foreNile)




#### Ignore below here 



help(Nile)  # familiarize yourself with the Nile data set 
plot(Nile)
x=c(3.1, 1.2)
buildNile <- function(x) dlmModPoly(1, dV = x[1], dW = x[2])
fitNile=dlmMLE(Nile, parm=rep(100,2), build=buildNile,lower=rep(1e-9,2), hessian=T)
fitNile$par
aVar <- solve(fitNile$hessian)
aVar
sqrt(diag(aVar))
dlmpredict(fitNile)
modTemp <- buildTemp(fitNile$par)
dlmForecast(fitNile,nAhead=10)


summary(fitNile)

albertopaid=c(50.04,94.24,42.37,82.39,19.69,
              104.74,97.94,95.6, 36.20,36.33,
              57.00,78.41,53.15,3022.06,260.24,
              366.62)
juanapaid=c(61.3,64.2,526.4,177,28)




install.packages('dlm')
library(dlm)
### Simple random walk with changing coefficient

##Create dlm object with dlm() function 
dlmobject=dlm(FF=1,GG=1,V=0.8,W=0.1,m0=0,C0=1e7)
names(dlmobject)

## Fit the model 
mod= dlmModPoly(order=1, dV=0.8, dW=0.1)
names(mod)
dlm(FF=matrix(c(1,0),nrow=1),
    GG=matrix(c(1,0,1,1),ncol=2),
    V=0.8, W=diag(c(0.2,0.1)),
    m0=c(0,0), C0=diag(c(1e7,1e7)))
dlmModPoly(order=2, dV=0.8, dW=c(0.2,0.1))

set.seed(1)
# We generate data that has structural change, jumps after 
# interventions, etc. 
Plummet.dat=20+2*rnorm(20)+c(rep(0,10),rep(-10,10))
plot.ts(Plummet.dat)
n=length(Plummet.dat)

## Prepare the matrix for the Kalman filter 
Plummet.mat=matrix(Plummet.dat,nrow=n,ncol=1)
m1= SS(y=Plummet.mat, 
       Fmat=function(tt,x,phi) return( matrix(1) ), 
       Gmat=function(tt,x,phi) return(matrix(1) ), 
       Wmat=function(tt,x,phi) return(matrix(0.1) ), 
       Vmat= function(tt,x,phy) return(matrix(2) ), 
       m0=matrix(25), CO=matrix(10)
)