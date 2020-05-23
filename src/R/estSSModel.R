estSSModel <- function(y0, X0, D0, params, type){
  
  X0 = X; D0 = D; y0 = y
  
  DM = data.frame();
  if(ncol(X0) == 0) {
    DM = D0 
  } else if (ncol(D0) == 0) {
    DM = X0
  }    
 
  op = switch(type, arima = list(method="Nelder-Mead", coefs=initCoef(yproc,DM,params), lwr=rep(-Inf, length(params)+ncol(X))), 
              level = list(method="L-BFGS-B",    lwr=rep(0, 2), coefs=c(1,1)),
              trend = list(method="L-BFGS-B",    lwr=rep(0, 3), coefs=c(1,1,1)),
              BSM = list(method="L-BFGS-B",    lwr=rep(0, 4), coefs=c(1,1,1,1)))
  
  if(type != "arima"){
    optimize = optim(par=as.vector(op$coefs), fn=likSS, y=y, X0=X, D0=D, ord=params, type=type, method=op$method, lower=op$lwr)
    coefsMLE = optimize$par
    err = 1
    Z <- switch(type, level = localLevelSS(y), trend = localLevelTrendSS(y), BSM = basicSeasonalSS(y, 4))
    Z$P0[] <- 1e+06 * var(y)/100
    while(err > 0.00000001) {
      np = length(coefsMLE)-1
      Z$Q[cbind(1L:np, 1L:np)] <- coefsMLE[-(np + 1L)]
      Z$H <- coefsMLE[np + 1L]
      kf=kalmanLL(y, Z)
      coefsMLE=coefsMLE*kf$sigmahat2;
      err = 1-kf$sigmahat2
    }  
  }  else {
    optimize = optim(par=as.vector(op$coefs), fn=likSS, y=y, X0=X, D0=D, ord=params, type=type, method=op$method)
    coefsMLE = optimize$par
    Z = arimaSS(coefsMLE,c(params[1],0,params[3]),y0,D0,1)
    Z = designMatrixSS(coefsMLE, c(params[1],0,params[3]), y0, X0, Z)
  }
  
  out = list(coef=coefsMLE, Z=Z);
  
  return(out);
}  


