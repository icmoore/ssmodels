source('~/repos/ssmodels/dev/kalmanLL.R')
source('~/repos/ssmodels/dev/quickTimePlot.R')

library(KFAS)
library(ggplot2)
library(stats)
library(optimx)

y = data(Nile)
y = Nile
y = as.matrix(as.data.frame(log10(UKgas)))

fit <- StructTS(y,"trend")

Z$V[cbind(1L:np, 1L:np)] <- p[-(np + 1L)]

c=fit$coef


quickTimePlot(cbind(y,fit$fitted[,1]),F)$p

makeTrend <- function(x) {
  T <- matrix(c(1, 0, 1, 1), 2L, 2L)
  Z <- c(1, 0)
  xm <- if (is.na(x[1L])) 
    mean(x, na.rm = TRUE)
  else x[1L]
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
  Z$Q[cbind(1L:np, 1L:np)] <- p[-(np + 1L)]*vx 
  Z$H <- p[np + 1L]*vx
  #z <- .Call(C_KalmanLike, y, Z, -1L, FALSE, FALSE, PACKAGE = "stats")
  #Z$Pn = Z$P0;  Z$P = Z$P0; Z$V = Z$Q; Z$h = Z$H; Z$a = Z$a0; kf=KalmanLike(y, Z, nit = 0)
  
  kf=kalmanLL(y,Z)
  LL = kf$Lik
  return(LL)
}

# KalmanLike <- function(y, mod, nit = 0, fast=TRUE)
#  {
  ## next call changes objects a, P, Pn if fast==TRUE: beware!
#  x <- .Call(C_KalmanLike, y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h,
#             mod$Pn, as.integer(nit), FALSE, fast=fast, PACKAGE = "stats")
#  names(x) <- c("ssq", "sumlog")
#  s2 <- x[1L]/length(y)
#  list(Lik = 0.5*(log(x[1L]/length(y)) + x[2L]/length(y)), s2 = s2)
#}

Z <- makeTrend(y)
np <- 2L
fixed <- rep(NA_real_, np + 1L)
mask <- is.na(fixed)
vx <- var(y, na.rm = TRUE)/100
cf <- fixed/vx

Z = makeTrend(y)
Z$P[] <- 1e+06 * vx

res <- optim(par=c(1,1,1), y=y,getLike, method = "L-BFGS-B", lower = rep(0, np + 1L), upper = rep(Inf, np + 1L))
#res <- optim(par=c(1,1,1), y=y,getLike)
c[3]/res$par[3]
c[1]/res$par[1]

Z = makeTrend(y)
Z$P0[] <- 1e+06 * vx
Z$Pn = Z$P0;  Z$P = Z$P0; Z$V = Z$Q; Z$h = Z$H; Z$a = Z$a0; 
Z$V[cbind(1L:np, 1L:np)] <- p[1L:np] * vx
Z$h <- p[np + 1L] * vx
p = res$par
Z$Q[cbind(1L:np, 1L:np)] <- p[-(np + 1L)]
Z$H <- p[np + 1L]
kf=kalmanLL(y, Z)
res$par*kf$sigmahat2
Z$Q[cbind(1L:np, 1L:np)] <- p[-(np + 1L)]*kf$sigmahat2
Z$H <- p[np + 1L]*kf$sigmahat2

kr <- KalmanRun(y, Z, -1, update = TRUE)



quickTimePlot(cbind(y,fit$fitted[,1],kr$states[,1]),F)$p
getLike(res$par,y)
