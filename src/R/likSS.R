likSS=function(coefs,ord,y,X0,D0,type){
  
  arimaModel <- function(coefs) {
    mod0 = arimaSS(coefs,ord,y,D0,0)
    mod0 = designMatrixSS(coefs, ord, y, X0, mod0)
    return(mod0)
  }  
  
  localLevelModel <- function(coefs){
    mod0 = localLevelSS(y)
    yv = var(y)/100
    mod0$Q <- coefs[1]*yv
    mod0$H <- coefs[2]*yv
    mod0$P0 <-  1e7 
    return(mod0)
  }

  localLevelTrendModel <- function(coefs){
    mod0 = localLevelTrendSS(y)
    yv = var(y)/100
    mod0$Q[cbind(1L:2, 1L:2)] <- coefs[-(2 + 1L)]
    mod0$H <- coefs[2 + 1L]
    mod0$X = mod0$D = mod0$E = data.frame()
    mod0$P0[] <- 1e+06 * yv
    return(mod0)
  }
  
  basicSeasonalModel <- function(coefs) {
    mod0 = basicSeasonalSS(y, 4) 
    np = length(coefs)-1
    p <- coefs
    yv = var(y)/100
    if (all(p == 0)) 
      return(1000)
    mod0$Q[cbind(1L:np, 1L:np)] <- p[-(np + 1L)] * yv
    mod0$H <- p[np + 1L] * yv
    mod0$P0[] <- 1e+06 * yv
    mod0$X = mod0$D = mod0$E = data.frame()
    return(mod0)
  }
  
  mod0 <- switch(type, arima = arimaModel(coefs), 
                        level = localLevelModel(coefs), 
                        trend = localLevelTrendModel(coefs), 
                        BSM = basicSeasonalModel(coefs))
  
  kf = kalmanLL(y,mod0)
  LL = as.numeric(kf$LL)
  
  return(LL)
}
