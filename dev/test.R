source('~/repos/ssmodels/dev/loadSSFn.R')

library(KFAS)
library(ggplot2)
library(stats)
library(optimx)

y = data(Nile)
y = as.matrix(Nile)

fit <- StructTS(y,"trend")
sf = KalmanRun(y,fit$model)

c=fit$coef


quickTimePlot(cbind(y,fit$fitted[,1]),F)$p

makeTrend <- function(x) {
  T <- matrix(c(1, 0, 1, 1), 2L, 2L)
  Z <- c(1, 0)
  xm <- if (is.na(x[1L])) {
    mean(x, na.rm = TRUE)
  } else {
    x[1L]
  }
    
  if (is.na(xm)) 
    stop("the series is entirely NA")
  a <- c(xm, 0)
  P <- Pn <- matrix(0, 2L, 2L)
  h <- 1
  V <- diag(2L)
  return(list(Z = Z, a0 = a, T = T, Q = V, H = h, P0 = Pn))
}


getLike <- function(par,y) {
  p <- cf
  p[mask] <- par
  if (all(p == 0)) 
    return(1000)
  Z = makeTrend(y)
  yv = var(y)/100
  Z$Q[cbind(1L:np, 1L:np)] <- p[-(np + 1L)]
  Z$H <- p[np + 1L]
  Z$X = Z$D = Z$E = data.frame()
  #z <- .Call(C_KalmanLike, y, Z, -1L, FALSE, FALSE, PACKAGE = "stats")
  #Z$Pn = Z$P0;  Z$P = Z$P0; Z$V = Z$Q; Z$h = Z$H; Z$a = Z$a0; kf=KalmanLike(y, Z, nit = -1);  LL = kf$Lik
  kf=kalmanLL(y,Z);  LL = kf$LL
 
  return(LL)
}

Z <- makeTrend(y)
np <- 2L
fixed <- rep(NA_real_, np + 1L)
mask <- is.na(fixed)
vx <- var(y, na.rm = TRUE)/100
cf <- fixed/vx

res <- optim(par=c(1,1,1), y=y,getLike, method = "L-BFGS-B", lower = rep(0, np + 1L), upper = rep(Inf, np + 1L))
#res <- optim(par=c(1,1,1), y=y,getLike)
coefsMLE = res$par


Z = makeTrend(y)
p = res$par
Z$Q[cbind(1L:np, 1L:np)] <- p[-(np + 1L)]
Z$H <- p[np + 1L]
Z$X = Z$D = Z$E = data.frame()
kf=kalmanLL(y, Z)
#Z$Pn = Z$P0;  Z$P = Z$P0; Z$V = Z$Q; Z$h = Z$H; Z$a = Z$a0; kf=KalmanLike(y, Z, nit = -1);  res$par*kf$s2
res$par*kf$sigmahat2

mod = localLevelTrendSS(coefsMLE,y)
mod$Q[cbind(1L:2, 1L:2)] <- coefsMLE[-(2 + 1L)] * kf$sigmahat2
mod$H <- coefsMLE[2 + 1L] * kf$sigmahat2
kf = kalmanRun(y,mod)
kf.fit = kf$fit 
kf.05 = kf.fit - 1.96*kf$se
kf.95 = kf.fit + 1.96*kf$se
kf.sigma2 = sqrt(kf$sigmahat2)  

yf = cbind(y[2:N-1,],kf.fit[2:N],sf$states[1:N-1,1],kf.05[2:N],kf.95[2:N])
X11();quickTimePlot(yf[1:N-1,],F)$p

# ---------------------------------------------

fit <- StructTS(y,"level")
sf = KalmanRun(y,fit$model)

likSS=function(coefs,ord,y,X0,D0){
  mod0 = localLevelSS(coefs,y)
  yv = var(y)/100
  mod0$Q <- coefs[1]*yv
  mod0$H <- coefs[2]*yv
  mod0$P0 <-  1e7
  kf = kalmanLL(y,mod0)
  LL = as.numeric(kf$LL)
  return(LL)
}

#optimize = optimx(par=c(1,1), fn=likSS, y=y, X0=data.frame(), D0=data.frame(), ord=c(0,0,0), method=c("BFGS"))
#optimize = optimx(par=c(1,1), fn=likSS, y=y, X0=data.frame(), D0=data.frame(), ord=c(0,0,0), control=list(all.methods=TRUE, save.failures=TRUE, trace=0))
optimize =optim(par=c(1, 1), fn=likSS, y=y, X0=data.frame(), D0=data.frame(), ord=c(0,0,0), method = "L-BFGS-B", lower = rep(1e-4, 2))
coefsMLE = unlist(optimize[1:2])
print(optimize)

mod = localLevelSS(coefsMLE,y)
kl = kalmanLL(y,mod)
mod$Q <- coefsMLE[1] * kl$sigmahat2
mod$H <- coefsMLE[2] * kl$sigmahat2
#mod$Q <- coefsMLE[1] 
#mod$H <- coefsMLE[2] 
#mod$Q <- fit$coef[1]
#mod$H <- fit$coef[2]
kf = kalmanRun(y,mod)
kf.fit = kf$fit 
kf.05 = kf.fit - 1.96*kf$se
kf.95 = kf.fit + 1.96*kf$se
kf.sigma2 = sqrt(kf$sigmahat2)  

yf = cbind(y[2:N-1,],kf.fit[2:N],sf$states[1:N-1,1],kf.05[2:N],kf.95[2:N])
X11();quickTimePlot(yf[1:N-1,],F)$p