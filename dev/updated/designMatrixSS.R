designMatrixSS <- function(coefs, ord, y, X, mod) {
  
  p = mod$ord[1]
  d = mod$ord[2]
  q = mod$ord[3]
  
  k = ncol(X)
  if(k > 0){
    
    r = max(p,q+1);
    phi   = coefs[1:p]; 
    theta = coefs[(p+1):(p+q)];
    beta = coefs[(p+q+1):length(coefs)]
    
    Z = matrix(0,r+k,1);T = matrix(0,r+k,r+k); R = matrix(0,r+k,1); 
    
    T[1:k,1:k] = diag(k)
    T[(k+1):(k+r),(k+1):(k+r)] = mod$T
    
    R[1:k] = 0; R[(k+1):(k+r)] = mod$R
    
    Z[1:k] = 0; Z[k+1,1] = 1; Q = R %*% t(R);
    
    a_tt  = matrix(0,r+k,1);
    a_tt[1:k] = beta
    P_tt = matrix(0,r+k,r+k); P_tt[(k+1):(k+r),(k+1):(k+r)] = mod$P0 
    
    A = array(0,dim=c(1,r+k,nrow(y)))
    for(i in 1:nrow(y)){
      A[,,i]=Z
    }
    
    
  } else {
    T=mod$T;
    Z=mod$Z;
    Q=mod$Q;
    R=mod$R;
    a_tt=mod$a0;
    P_tt=mod$P0;
    mod$X=X
  }
  
  out = list(T=T,Z=Z,Q=Q,R=R,a0=a_tt,P0=P_tt,X=X,ord=mod$ord,E=mod$E,D=mod$D);
  
  return(out)
}  