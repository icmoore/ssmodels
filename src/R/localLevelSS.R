localLevelSS <- function(y) {
  
  # H -> sig_epsilon (15099)
  # Q -> sig_eta     (1469)
  y = as.matrix(y)
  T <- matrix(1, 1L, 1L)
  Z <- 1
  ym <- if (is.na(y[1L])) 
    mean(y, na.rm = TRUE)
  else y[1L]
  if (is.na(ym)) 
    stop("the series is entirely NA")
  a0 <- mean(y, na.rm = TRUE)  
  P0 <- matrix(0, 1L, 1L)
  H <- 1
  Q <- diag(1L)
  D = E = X = data.frame()
  return(list(Z = Z, a0 = a0, P0 = P0, T = T, Q = Q, H = H, X = X, E = E, D = D))
  
  return(out);
}  