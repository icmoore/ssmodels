kalmanRun=function(y,mod){
  
  T = mod$T
  Z = mod$Z
  Q = mod$Q
  R = mod$R
  a_tt = mod$a0
  P_tt = mod$P0
  X = mod$X
  E = mod$E
  D = mod$D
  c = ncol(X)
  d = ncol(D)
  H = matrix(0,length(a_tt),1); 
  
  N = nrow(y); logsum = 0; sumsq  = 0; fit = c(); se = c();
  for(i in 1:N){  
    
    if(c > 0) Z[1:c,] = t(X[i,])
    if(d > 0) H = E %*% t(D[i,])
    a_t1t  = T %*% a_tt + H;
    P_t1t = T %*% P_tt %*% t(T) + Q; 
    yhat_t1t  = t(Z) %*% a_t1t;
    omega     = t(Z)  %*%  P_t1t %*% Z;
    delta_t1 = (P_t1t %*% Z) / omega[1];
    innov     = as.matrix(as.numeric(y[i,1] - yhat_t1t));
    a_t1t1 = a_t1t + (delta_t1 %*% innov);
    P_t1t1 = P_t1t - (delta_t1 %*% t(Z) %*%  P_t1t);
    
    logsum = logsum + log(omega);
    sumsq  = sumsq  + innov^2/omega;
    
    a_tt  = a_t1t1; 
    P_tt = P_t1t1;
    
    fit = c(fit,yhat_t1t)
    se = c(se,omega)

    
  }
  
  L = logsum + N*log(sumsq);
  sigmahat = sqrt(sumsq/N);
  
  sigmahat2 = sigmahat*sigmahat
  se = sqrt(se*sigmahat2)
  
  out = list(fit = fit, se = se, LL = L, sigmahat2 = sigmahat2, a_tt = a_tt, P_tt = P_tt) 
  
  return(out)
}  