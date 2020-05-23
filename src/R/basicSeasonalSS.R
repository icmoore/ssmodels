basicSeasonalSS <- function(x, nf) {
  x = as.matrix(x)
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
  D = E = X = data.frame()
  return(list(Z = Z, a0 = a0, P0 = P0, T = T, Q = Q, H = H, X = X, E = E, D = D))
}