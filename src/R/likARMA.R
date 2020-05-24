likARMA <- function(coefs, ord, y0, type) {
  
  p = ord[1]
  d = ord[2]
  q = ord[3]
  
  #coefs = coefs_mle
  #y0 = y
  #type=1

  y0 = as.matrix(y0) 

  best = 1e10;
  nSteps = 100;
  fit = c(); pred = c();
  
  m = nrow(y0) 
  n = ncol(y0)  
  
  if(n > 1) {
    T = n; y0 = t(y0)
  } else {
    T = m
  }
  
  r = max(p,q+1);
  phi   = coefs[1:p]; 
  theta = coefs[(p+1):(p+q)];
  
  if((d == 0) || (type == 0)){
    A = matrix(0,r,r); R = matrix(0,r,1); Z = matrix(0,r,1)
    A[1:p,1] = phi; A[1:(r-1),2:r] = diag(r-1)
    R[1,1] = 1; R[2:(q+1),] = theta
    Z[1,1] = 1; Q = R %*% t(R) 
    
    xhat_tt  = matrix(0,r,1);
    Sigma_tt = matrix(qr.coef(qr(diag(r^2)- A %x% A),c(Q)),r,r)
    
  } else {
    Od = rep(1, d)
    Ud = !lower.tri(matrix(0, d, d))
    Ud[Ud == F] = 0
    
    A = matrix(0,r+d,r+d); A0 = matrix(0,r,r); R = matrix(0,r+d,1); R0 = matrix(0,r,1); Z = matrix(0,r+d,1)
    A0[1:p,1] = phi; A0[1:(r-1),2:r] = diag(r-1)
    A[1:d,1+d] = Od; A[1:d,1:d] = Ud; A[(1+d):(p+d),(1+d)] = phi; A[(1+d):(d+r-1),(2+d):(r+d)] = diag(r-1); 
    
    R0[1,1] = 1; R0[2:(q+1),] = theta
    R[1+d,1] = 1; R[(2+d):(d+q+1),] = theta; 
    Z[1,1] = 1; Z[2:3,1] = Od; Q = R %*% t(R);  Q0 = R[(d+1):(r+d),] %*% t(R[(d+1):(r+d),])
    Q0 = R0 %*% t(R0);
    
    xhat_tt  = matrix(0,r+d,1);
    #Sigma_tt = matrix(qr.coef(qr(diag((r)^2) - A0 %x% A0),c(Q0)),r,r)
    Sigma_tt = matrix(qr.coef(qr(diag((r)^2) - A[(d+1):(r+d),(d+1):(r+d)] %x% A[(d+1):(r+d),(d+1):(r+d)]),c(Q0)),r,r)
    Sigma_tt[is.na(Sigma_tt)] <- 0

    Sigma_0 = matrix(0,r+d,r+d)
    Sigma_0[(d+1):(r+d),(d+1):(r+d)] = Sigma_tt
    Sigma_tt = Sigma_0
  }
  

  
  logsum = 0;
  sumsq  = 0;
  
  if((type==0) || (type==1)){
    startPt = 0; endPt = T-1
  } else if(type==2){
    startPt = 0; endPt = T+nSteps-2
  }  
  
  pred = NULL
  
  for(i in startPt:endPt){  
    
    xhat_t1t  = A %*% xhat_tt;
    Sigma_t1t = A %*% Sigma_tt %*% t(A) + Q; 
    yhat_t1t  = t(Z) %*% xhat_t1t;
    omega     = t(Z)  %*%  Sigma_t1t %*% Z;
    delta_t1 = (Sigma_t1t %*% Z) / omega[1];
    innov     = as.matrix(as.numeric(y0[i+1,1] - yhat_t1t));
    xhat_t1t1 = xhat_t1t + (delta_t1 %*% innov);
    Sigma_t1t1 = Sigma_t1t - (delta_t1 %*% t(Z) %*%  Sigma_t1t);
    
    # one-step ahead prediction
    xhat_t2t1 = A %*% xhat_t1t1
    yhat_t2t1 = t(Z) %*% xhat_t2t1
    
    logsum = logsum + log(omega);
    sumsq  = sumsq  + innov^2/omega;
    
    xhat_tt  = xhat_t1t1; 
    Sigma_tt = Sigma_t1t1;
    
    
    if(type==1){
      fit = c(fit,yhat_t1t)
    } else if(type==2){
      ind = i-T+1
      if(ind == 0) {
        y0 = rbind(y0,as.numeric(yhat_t2t1))
        pred = yhat_t2t1
      }  
      if(ind > 0) {
        y0 = rbind(y0,as.numeric(yhat_t2t1))
        pred = c(pred,as.numeric(yhat_t2t1))    
      }  
    }
    
  }
  
  L = logsum + T*log(sumsq);
  sigmahat = sqrt(sumsq/T);
  
  out = -1;
  
  if(type==0){
    out = L
  } else if(type==1){
    out = as.matrix(fit) 
  } else if(type==2){
    out = as.matrix(pred) 
  }
  
  return(out);
  
}