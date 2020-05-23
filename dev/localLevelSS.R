localLevelSS <- function(coefs,y) {
  
  # H -> sig_epsilon (15099)
  # Q -> sig_eta     (1469)
  
  T = c(); Z = c(); Q = c(); R = c(); a0 = c(); P0 = c(); r = 1;
  D = data.frame(); X = data.frame();
  
  T = matrix(0,r,r); R = matrix(0,r,1); Z = matrix(0,r,1); E = matrix(0,r,1)

  T[1,1] = 1; Z[1,1] = 1; R[1,1] = 1; Q = coefs[1]; H = coefs[2];
  
  a_tt  = matrix(0,r,1); a_tt[1] = mean(as.matrix(y))
  P_tt = matrix(0,r,1)
  
  out = list(T=T,Z=Z,Q=Q,R=R,H=H,a0=a_tt,P0=P_tt,E=E,D=D,X=X);
  
  return(out);
}  