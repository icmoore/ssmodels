localLevelTrendSS <- function(coefs,y) {
  
  # H -> sig_epsilon (15099)
  # Q -> sig_eta     (1469)
  
  coefs = as.vector(coefs)
  T = c(); Z = c(); Q = c(); R = c(); a0 = c(); P0 = c(); r = 2;
  D = data.frame(); X = data.frame();
  y = as.matrix(y)
  
  ym <- if (is.na(y[1L])) {
    mean(y, na.rm = TRUE)
  } else {
    y[1L]
  }

  
  T = matrix(0,r,r); Q = matrix(0,r,r); R = matrix(0,r,1); Z = matrix(0,r,1); E = matrix(0,r,1)
  
  T = !lower.tri(matrix(0, r, r)); T[T == F] = 0
  
  Z = c(1,0); R = diag(r); Q[1,1] = coefs[1]; Q[2,2] = coefs[2]; H = coefs[3];
  
  a_tt  = matrix(0,r,1); a_tt = c(ym, 0)
  P_tt = matrix(0, r, r)
  
  out = list(T=T,Z=Z,Q=Q,R=R,H=H,a0=a_tt,P0=P_tt,E=E,D=D,X=X);
  
  return(out);
}  