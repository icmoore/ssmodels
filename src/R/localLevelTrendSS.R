localLevelTrendSS <- function(y) {
  
  # H -> sig_epsilon (15099)
  # Q -> sig_eta     (1469)
  
  #coefs = as.vector(coefs)
  T = c(); Z = c(); Q = c(); R = c(); a0 = c(); P0 = c(); r = 2;
  D = data.frame(); X = data.frame();
  y = as.matrix(y)
  
  T <- matrix(c(1, 0, 1, 1), 2L, 2L)
  Z <- c(1, 0)
  ym <- if (is.na(y[1L])) 
    mean(y, na.rm = TRUE)
  else y[1L]
  if (is.na(ym)) 
    stop("the series is entirely NA")
  a0 <- c(ym, 0)
  P0 <- P <- Pn <- matrix(0, 2L, 2L)
  H <- 1
  Q <- diag(2L)
  D = E = X = data.frame()
  return(list(Z = Z, a0 = a0, P0 = P0, T = T, Q = Q, H = H, X = X, E = E, D = D))
  
  return(out);
}  