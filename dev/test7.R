rm(list=ls())
source('~/repos/ssmodels/dev/kalmanLL.R')
source('~/repos/ssmodels/dev/quickTimePlot.R')


library(KFAS)
library(ggplot2)

tol = 1E-7
#type = "BSM"; init = c(1,1,1,1)
type = "trend"; init = c(1,1,1)
#type = "level"; init = c(1,1)
#y = as.matrix(Nile)
#y = as.matrix(as.data.frame(log10(UKgas))) 
#y = log10(UKgas)
#y = log10(UKDriverDeaths)
fit <- StructTS(y,type)
fit$coef


fixed = NULL; optim.control = NULL

makeLevel <- function(x) {
  T <- matrix(1, 1L, 1L)
  Z <- 1
  xm <- if (is.na(x[1L])) 
    mean(x, na.rm = TRUE)
  else x[1L]
  if (is.na(xm)) 
    stop("the series is entirely NA")
  a0 <- mean(x, na.rm = TRUE)  
P0 <- matrix(0, 1L, 1L)
  H <- 1
  Q <- diag(1L)
  return(list(Z = Z, a0 = a0, P0 = P0, T = T, Q = Q, H = H))
}

makeTrend <- function(x) {
  T <- matrix(c(1, 0, 1, 1), 2L, 2L)
  Z <- c(1, 0)
  xm <- if (is.na(x[1L])) 
    mean(x, na.rm = TRUE)
  else x[1L]
  if (is.na(xm)) 
    stop("the series is entirely NA")
  a0 <- c(xm, 0)
  P0 <- P <- Pn <- matrix(0, 2L, 2L)
  H <- 1
  Q <- diag(2L)
  return(list(Z = Z, a0 = a0, P0 = P0, T = T, Q = Q, H = H))
}

makeBSM <- function(x, nf) {
  if (nf <= 1L) 
    stop("frequency must be a positive integer >= 2 for BSM")
  T <- matrix(0, nf + 1L, nf + 1L)
  T[1L:2L, 1L:2L] <- c(1, 0, 1, 1)
  T[3L, ] <- c(0, 0, rep(-1, nf - 1L))
  if (nf >= 3L) {
    ind <- 3:nf
    T[cbind(ind + 1L, ind)] <- 1
  }
  Z <- c(1, 0, 1, rep(0, nf - 2L))
  xm <- if (is.na(x[1L])) {
    mean(x, na.rm = TRUE)
  } else {
    x[1L]
  }  
  if (is.na(xm)) 
    stop("the series is entirely NA")
  a0 <- c(xm, rep(0, nf))
  P0 <- matrix(0, nf + 1L, nf + 1L)
  H <- 1
  Q <- diag(c(1, 1, 1, rep(0, nf - 2L)))
  return(list(Z = Z, a0 = a0, P0 = P0, T = T, Q = Q, H = H))
}

getLike <- function(par) {
  p <- par
  if (all(p == 0)) 
    return(1000)
  Z$Q[cbind(1L:np, 1L:np)] <- p[-(np + 1L)] * vy
  Z$H <- p[np + 1L] * vy
  z=kalmanLL(y,Z)
  #Z$P0 = z$P_tt
  #Z$a0 = z$a0
  return(z$Lik)
}


y <- as.ts(y)
#dim(y) <- NULL
#xtsp <- tsp(y)
nf <- frequency(y)
Z <- switch(type, level = makeLevel(y), trend = makeTrend(y), BSM = makeBSM(y, nf))
vy <- var(y, na.rm = TRUE)/100
Z$P0[] <- 1e+06 * vy
np <- switch(type, level = 1L, trend = 2L, BSM = 3L)

res <- optim(init, getLike, method = "L-BFGS-B", lower = rep(0, np + 1L), upper = rep(Inf, np + 1L), control = optim.control)
coef <- res$par


#yp = cbind(fit$fitted[,3],kr$states[,3])
#quickTimePlot(yp,F)$p

#Z$Q[cbind(1L:np, 1L:np)] <- coef[1L:np] * vy
#Z$h <- coef[np + 1L] * vy
#z <- KalmanRun(y, Z, -1, update = TRUE)
#resid <- ts(z$resid)
#tsp(resid) <- xtsp
#cn <- switch(type, level = c("level"), trend = c("level", "slope"), BSM = c("level", "slope", "sea"))
#states <- z$states
#if (type == "BSM") states <- states[, 1L:3L]
#dimnames(states) <- list(time(x), cn)
#states <- ts(states, start = xtsp[1L], frequency = nf)
#coef <- pmax(coef * vy, 0)
#names(coef) <- switch(type, level = c("level", "epsilon"), trend = c("level", "slope", "epsilon"), BSM = c("level", "slope", "seas", "epsilon"))
#loglik <- -length(y) * res$value - 0.5 * sum(!is.na(y)) * log(2 * pi)
#loglik0 <- -length(y) * res$value + length(y) * log(2 * pi)
#res <- list(coef = coef, loglik = loglik, loglik0 = loglik0, 
#            data = y, residuals = resid, fitted = states, call = match.call(), 
#            series = series, code = res$convergence, model = attr(z, "mod"), model0 = Z, xtsp = xtsp)
#class(res) <- "StructTS"
#res

err = 1
Z <- switch(type, level = makeLevel(y), trend = makeTrend(y), BSM = makeBSM(y, nf))
Z$P0[] <- 1e+06 * vy
while(err > tol) {
  Z$Q[cbind(1L:np, 1L:np)] <- coef[-(np + 1L)]
  Z$H <- coef[np + 1L]
  kf=kalmanLL(y, Z)
  coef=coef*kf$sigmahat2;
  err = 1-kf$sigmahat2
  print(err)
}

Z$Pn = Z$P0;  Z$P = Z$P0; Z$V = Z$Q; Z$h = Z$H; Z$a = Z$a0; 
kr <- KalmanRun(y, Z, -1, update = TRUE)
yp = cbind(y,fit$fitted[,1],kr$states[,1])
quickTimePlot(yp,F)$p

p2=fit$coef; p2
coef

