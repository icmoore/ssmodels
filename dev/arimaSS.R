arimaSS <- function(coefs, ord, y, D, type) {
  
  p = ord[1]
  d = ord[2]
  q = ord[3]
 
  T = c(); Z = c(); Q = c(); R = c(); a0 = c(); P0 = c();
  
  r = max(p,q+1);
  phi   = coefs[1:p]; 
  theta = coefs[(p+1):(p+q)];
  
  if((d == 0) || (type == 0)){
    T = matrix(0,r,r); R = matrix(0,r,1); Z = matrix(0,r,1)
    if(p > 0) T[1:p,1] = phi; 
    T[1:(r-1),2:r] = diag(r-1)
    R[1,1] = 1; R[2:(q+1),] = theta
    Z[1,1] = 1; Q = R %*% t(R) 
    
    a_tt  = matrix(0,r,1); 
    P_tt = matrix(qr.coef(qr(diag(r^2)- T %x% T),c(Q)),r,r)
    
  } else {
    Od = rep(1, d)
    Ud = !lower.tri(matrix(0, d, d))
    Ud[Ud == F] = 0
    
    T = matrix(0,r+d,r+d); T0 = matrix(0,r,r); R = matrix(0,r+d,1); R0 = matrix(0,r,1); Z = matrix(0,r+d,1)
    if(p > 0) T0[1:p,1] = phi; 
    T0[1:(r-1),2:r] = diag(r-1)
    T[1:d,1+d] = Od; T[1:d,1:d] = Ud; T[(1+d):(p+d),(1+d)] = phi; T[(1+d):(d+r-1),(2+d):(r+d)] = diag(r-1); 
    
    R0[1,1] = 1; R0[2:(q+1),] = theta
    R[1+d,1] = 1; R[(2+d):(d+q+1),] = theta; 
    Z[1,1] = 1; #Z[2:3,1] = Od; 
    Q = R %*% t(R);  Q0 = R[(d+1):(r+d),] %*% t(R[(d+1):(r+d),])
    Q0 = R0 %*% t(R0);
    
    a_tt  = matrix(0,r+d,1);
    P_tt = matrix(qr.coef(qr(diag((r)^2) - T[(d+1):(r+d),(d+1):(r+d)] %x% T[(d+1):(r+d),(d+1):(r+d)]),c(Q0)),r,r)
    P_tt[is.na(P_tt)] <- 0
    
    P_0 = matrix(0,r+d,r+d)
    P_0[(d+1):(r+d),(d+1):(r+d)] = P_tt
    P_tt = P_0
  }
  
  if(ncol(D) > 0) {
    beta = coefs[(p+q+1):length(coefs)]
    c = length(beta)
    E = matrix(0,r+d,c)
    E[1,1:c] = beta
  } else {
    E = matrix(0,r,1)
  } 
  
  #A = array(0,dim=c(1,r,nrow(y)))
  #for(i in 1:nrow(y)){
  #  A[,,i]=Z
  #}
  
  out = list(T=T,Z=Z,Q=Q,R=R,H=0,a0=a_tt,P0=P_tt,ord=ord,X=c(),E=E,D=D);

  return(out);
}  