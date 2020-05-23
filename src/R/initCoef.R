initCoef <- function(y0, X0, ord) {
  
  p = ord[1]
  d = ord[2]
  q = ord[3]
  
  #coefs = coefs_mlefdsf
  #y0 = yproc
  #type=0
  #X0=D
  
  #LSbet = c()
  #if(nrow(X0) >= 0) {
  #  X1 = as.matrix(X0[1:nrow(y0),])
  #  LSbet = ginv(t(X1) %*% X1) %*% t(X1) %*% as.matrix(y0)
  #  yRes = y0 - X1 %*% LSbet
  #  y0 = yRes;
  #}
  
  best = 1e10;
  
  m = nrow(y0) 
  n = ncol(y0)  
  
  if(n > 1) {
    T = n; y0 = t(y0)
  } else {
    T = m
  }
  
  lMax = ceiling(log(T)^1.5) 
  
  for(h in p:lMax) {
    Y =  na.omit(y0[(h+1):T,])
    X = c();
    if(h != 0){
      for(j in 1:h){
        if(j == 1) {
          X = y0[(h+1-j):(T-j),]   
        } else {
          X = cbind(X, y0[(h+1-j):(T-j),])      
        }
      }
    }

    if(p == 0) {
      res = Y
    } else {
      bet = ginv(t(X) %*% X) %*% t(X) %*% Y
      res = Y - X %*% bet;
    }
    
    sigma2 = (t(res) %*% res)/length(res);
    BIC = log(sigma2) + h*log(T)/T;
    
    if(BIC < best){
      best = BIC; 
      horder=h; 
      residuals = res;   
    }
  }
  
  #Step2: Run regression on lagged values and residuals
  nlag = max(p,q);
  Y = na.omit(y0[(horder+nlag+1):T,]);
  X = c(); 
  
  if(p > 0) {
    for(i in 1:p){
      if(length(X)==0){
        X = na.omit(y0[(horder+nlag+1-i):(T-i),]);  
      } else {
        X = cbind(X, na.omit(y0[(horder+nlag+1-i):(T-i),]));  
      } 
    } 
  }  
  if(q > 0) {
    for(i in 1:q){
      if(length(X)==0){
        X = residuals[(nlag+1-i):(T-horder-i)];
      } else {
        X = cbind(X,residuals[(nlag+1-i):(T-horder-i)]); 
      }  
      
    }
  }  
  
  if(ncol(X0) > 0) {
    ahead = nrow(y0) - nrow(X)
    #X1 = X0[1:(nrow(X0)-ahead+1),]
    X1 = X0[(ahead+1):nrow(y0),]
    X = as.matrix(cbind(X,X1))
  }
  
  bet = ginv(t(X) %*% X) %*% t(X) %*% Y
  coefs = bet
  
  #if(nrow(X0) > 0) {
  #  coefs = c(bet, LSbet)
  #} else {
  #  coefs = bet
  #}  
  #if(q == 0) coefs = t(bet[1:p,]);
  #if(p == 0) coefs = t(bet[1:q,]);
  #if((q != 0) && (p != 0))  coefs = bet;
  
  
  return(coefs);
  
  
}