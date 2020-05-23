# http://numerical.recipes/CS395T/lectures2008/12-MaxLikelihoodEstimationAgain.pdf
# http://stats.stackexchange.com/questions/68080/basic-question-about-fisher-information-matrix-and-relationship-to-hessian-and-s

setwd('~/repos/ssmodels/resources/')

rm(list=ls()) 
library(MASS)
library(ggplot2)
library(underhill)
library(optimx)

source('~/repos/ssmodels/dev/loadSSFn.R')
source('~/repos/ssmodels/dev/quickTimePlot.R')

nSteps = 100
p = 2; d = 0; q = 2;
AR = c(0.05, 0.6, -0.2, 0.8, 0.2, 0.34); MA = c(0.2, 0.2, -0.5, 0.3, -0.13, -0.7)
params = c(p,d,q)
type = "trend"

y = as.data.frame(read.table("y_arima_test.csv", header=F, sep=","));

if(d > 0){
  yproc = diff(as.matrix(y), differences = d)
  params = c(p,0,q)
} else {
  yproc = y - mean(as.matrix(y))
  params = c(p,0,q)
}

N = nrow(yproc)
t = 1:(nrow(yproc)+nSteps);
DM= as.data.frame(cbind(1,t))
#DM = as.data.frame(cbind(1,t,cos(pi*t*(1/6)), sin(pi*t*(1/6)), cos(pi*t*(2/6)), sin(pi*t*(2/6)), cos(pi*t*(3/6)), sin(pi*t*(3/6))))
X = DM[1:(N+1),]
XPred = DM[(N+1):(N+nSteps),]
X = XPred = data.frame()

D = DM[1:(N+1),]
DPred = DM[(N+1):(N+nSteps),]
D = DPred = data.frame()

op = switch(type, arima = list(method="Nelder-Mead", coefs=initCoef(yproc,X,params), lwr=rep(-Inf, length(params)+ncol(X))), 
                  level = list(method="L-BFGS-B",    lwr=rep(0, 2), coefs=c(1,1)),
                  trend = list(method="L-BFGS-B",    lwr=rep(0, 3), coefs=c(1,1,1)))

optimize = optim(par=as.vector(op$coefs), fn=likSS, y=y, X0=X, D0=D, ord=params, type=type, method=op$method, lower = op$lwr)
coefsMLE = optimize$par
coefsMLE

#mod = localLevelSS(coefsMLE,y)
#kl = kalmanLL(y,mod)
#mod$Q <- coefsMLE[1] * kl$sigmahat2
#mod$H <- coefsMLE[2] * kl$sigmahat2
#coefsMLE = coefsMLE*kl$sigmahat2

mod = localLevelTrendSS(coefsMLE,y)
kl = kalmanLL(y,mod)
mod$Q[cbind(1L:2, 1L:2)] <- coefsMLE[-(2 + 1L)]*kl$sigmahat2
mod$H <- coefsMLE[2 + 1L]*kl$sigmahat2
mod$P0[] <- 1e+06*var(y)/100
mod$X = mod$D = mod$E = data.frame()
coefsMLE = coefsMLE*kl$sigmahat2

mod$P0[] <- 1e+06*var(y)/100
kf = kalmanRun(y,mod)
kf.fit = kf$fit 
kf.05 = kf.fit - 1.96*kf$se
kf.95 = kf.fit + 1.96*kf$se
kf.sigma2 = sqrt(kf$sigmahat2)  

N = 100
fit <- StructTS(y,type)
yf = cbind(y[2:N-1,],kf.fit[2:N],fit$fitted[1:N-1,1])
X11(); quickTimePlot(yf[1:N-1,],F)$p

#optimize =optim(par=c(1, 1), fn=likSS, y=y, X0=data.frame(), D0=data.frame(), ord=c(0,0,0), type=type, method = "L-BFGS-B", lower = rep(1e-4, 2))
#coefsMLE = unlist(optimize[1:length(coefs)])

# -----------------------------------------
# - stats

H = hessian(y, coefsMLE, X, D, params, 0.00001, type) 
vcov_mle = 2*ginv(H)
se = sqrt(diag(abs(vcov_mle)))
pval = (1-pnorm(abs(coefsMLE)/se))*2

# -----------------------------------------
# - fit

mod = arimaSS(coefsMLE,c(p,d,q),yproc,D,1)
mod = designMatrixSS(coefsMLE, params, yproc, X, mod)
kf = kalmanRun(yproc,mod)
kf.fit = kf$fit
kf.sigma2 = kf$sigmahat2  

# -----------------------------------------
# - predict

mod = arimaSS(coefsMLE,c(p,d,q),yproc,DPred,1)
mod = designMatrixSS(coefsMLE, params, yproc, XPred, mod)
kp = kalmanPred(nSteps,mod,kf)

# -----------------------------------------
# - R

if(ncol(X) > 0){
  y.est = arima(y, order = c(p,d,q), xreg = X[1:nrow(yproc),2:ncol(X)])
  y.est.fit = y - as.data.frame(y.est$residuals) 
  y.est.pred = predict(y.est, n.ahead = nSteps, newxreg = XPred[,2:ncol(X)]);   
} else if(ncol(D) > 0){
  y.est = arima(y, order = c(p,d,q), xreg = D[1:nrow(y),2:ncol(D)])
  y.est.fit = y - as.data.frame(y.est$residuals) 
  y.est.pred = predict(y.est, n.ahead = nSteps, newxreg = DPred[,2:ncol(D)]);   
} else {
  y.est = arima(yproc, order = c(p,d,q))
  y.est.fit = yproc - as.data.frame(y.est$residuals) 
  y.est.pred = predict(y.est, n.ahead = nSteps);   
}

# -----------------------------------------
# - plot

yf = cbind(yproc[1:N,],kf.fit[1:N],y.est.fit[1:N,])
quickTimePlot(yf[1:N,],F)$p

#yp = cbind(kp$pred,y.est.pred$pred)
#quickTimePlot(yp[1:nSteps,],F)$p

kp$se
y.est.pred$se

y.est
coefsMLE





