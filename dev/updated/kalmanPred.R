kalmanPred=function(N,mod,kf){
  
  T = mod$T
  Z = mod$Z
  Q = mod$Q
  R = mod$R
  a_tt = kf$a_tt
  P_tt = kf$P_tt
  y = kf$fit[length(kf$fit)]
  X = mod$X
  E = mod$E
  D = mod$D
  c = ncol(X)
  d = ncol(D)
  H = matrix(0,length(a_tt),1); 
  
  logsum = 0; sumsq  = 0; pred = c(); se = c();
  for(i in 1:N){  
    
    if(c > 0) Z[1:c,] = t(X[i,])
    if(d > 0) H = E %*% t(D[i,])
    a_t1t  = T %*% a_tt + H;
    P_t1t = T %*% P_tt %*% t(T) + Q; 
    yhat_t1t  = t(Z) %*% a_t1t;
    omega     = t(Z)  %*%  P_t1t %*% Z;
    #delta_t1 = (P_t1t %*% Z) / omega[1];
    innov     = as.matrix(as.numeric(y[i] - yhat_t1t));
    #a_t1t1 = a_t1t + (delta_t1 %*% innov);
    #P_t1t1 = P_t1t - (delta_t1 %*% t(Z) %*%  P_t1t);
    
    logsum = logsum + log(omega);
    sumsq  = sumsq  + innov^2/omega;
    
    a_tt  = a_t1t; 
    P_tt = P_t1t;
    
    pred = c(pred,yhat_t1t)
    se = c(se,omega)
    y = c(y,yhat_t1t)
  }
  
  L = logsum + N*log(sumsq);
  sigmahat = sqrt(sumsq/N);
  
  sigmahat2 = sigmahat*sigmahat
  se = sqrt(se*kf$sigmahat2)
  
  out = list(pred = pred, se = se, LL = L, sigmahat2 = sigmahat2) 
  
  return(out)
}  