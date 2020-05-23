hessian <- function(y0, coefs, X0, D0, ord, del, type) {
  
  # y0=yproc; X0=X; D0=D; ord=params; del = 0.00001; ord = c(p,d,q)
  c=coefs
  del = 0.000001
  H = matrix(0, length(c), length(c))
  ca = rep(0, length(c)); cb = rep(0, length(c)); cc = rep(0, length(c)); cd = rep(0, length(c))
  for (i in 1:length(c)) {
    for (j in 1:i) {
      ca = c; ca[i] = ca[i]+del; ca[j] = ca[j]+del; 
      cb = c; cb[i] = cb[i]-del; cb[j] = cb[j]+del; 
      cc = c; cc[i] = cc[i]+del; cc[j] = cc[j]-del; 
      cd = c; cd[i] = cd[i]-del; cd[j] = cd[j]-del; 
      H[i,j] = (likSS(ca,ord,y0,X0,D0,type)+likSS(cd,ord,y0,X0,D0,type)-likSS(cb,ord,y0,X0,D0,type)-likSS(cc,ord,y0,X0,D0,type))/(4*del^2)
      H[j,i] = H[i,j]
    }
  }
  
  return(H);
}  