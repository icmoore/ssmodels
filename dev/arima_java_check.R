library(MASS)
library(ggplot2)

setwd('~/repos/ssmodels/resources/')

p = 2; d = 0; q = 2;
y = as.data.frame(read.table("y_arima_test.csv", header=F, sep=","));

N = nrow(y)
nAhead = 100;
t = 1:(N+nAhead)
DM = as.data.frame(cbind(t,cos(pi*t*(1/6)), sin(pi*t*(1/6)), cos(pi*t*(2/6)), sin(pi*t*(2/6)), cos(pi*t*(3/6)), sin(pi*t*(3/6))))
#DM[1:171,1] = 0
DM = as.data.frame(t)
#DM = data.frame()

XReg = DM[1:(N+1),]
XRegPred = DM[(N+1):(N+nAhead),]

#####################################################

if(ncol(DM) > 0){
  y.est = arima(y, order = c(p,d,q), xreg = DM[1:(N),])
  y.est.pred = predict(y.est, n.ahead = nAhead, newxreg = XRegPred); 
} else {
  y.est = arima(y, order = c(p,d,q))
  y.est.pred = predict(y.est, n.ahead = nAhead);  
}

y.est.fit = y - as.data.frame(y.est$residuals) 
y.est.pred.se = as.matrix(y.est.pred$se)
y.est.pred = as.matrix(y.est.pred$pred)
y.est.pred.05 = y.est.pred-1.96*y.est.pred.se;
y.est.pred.95 = y.est.pred+1.96*y.est.pred.se;

fit.Java = as.data.frame(read.table("y_hat_arima.csv", header=F, sep=","));
fit.se.Java = as.data.frame(read.table("y_hat_se_arima.csv", header=F, sep=","));
fit.se.05.Java = fit.Java-1.96*fit.se.Java;
fit.se.95.Java = fit.Java+1.96*fit.se.Java;

pred.Java = as.data.frame(read.table("y_pred_arima.csv", header=F, sep=","));
pred.se.Java = as.data.frame(read.table("y_pred_se_arima.csv", header=F, sep=","));
pred.se.05.Java = pred.Java-1.96*pred.se.Java;
pred.se.95.Java = pred.Java+1.96*pred.se.Java;

dat = data.frame(t= 1:nrow(y), y=y, fit.R = y.est.fit, 
                 fit.Java  = fit.Java, 
                 fit05.Java = fit.se.05.Java, 
                 fit95.Java = fit.se.95.Java)
colnames(dat) = c('t','y','fit.R','fit.Java', 'fit05.Java','fit95.Java')

dat_pred = data.frame(t= (nrow(y)+1):(nrow(y.est.pred)+nrow(y)),
                        pred.R = y.est.pred, 
                        pred.Java = pred.Java,
                        pred05.R = y.est.pred.05, 
                        pred95.R = y.est.pred.95,
                        pred05.Java = pred.se.05.Java, 
                        pred95.Java = pred.se.95.Java)
colnames(dat_pred) = c('t','pred.R','pred.Java','pred05.R','pred95.R','pred05.Java','pred95.Java')

p0 = ggplot() +
  geom_line(data=dat[800:N,], aes(x=t, y = y, colour = "Raw"), size=1,linetype="dashed") + 
  geom_line(data=dat[800:N,], aes(x=t, y = fit.R, colour = "R Fit")) +
  geom_line(data=dat[800:N,], aes(x=t, y = fit.Java , colour = "Java Fit"), size=1,linetype="dashed") +
  geom_line(data=dat_pred[1:nAhead,], aes(x=t, y = pred.R, colour = "R Pred")) +
  geom_line(data=dat_pred[1:nAhead,], aes(x=t, y = pred.Java , colour = "Java Pred"), size=1,linetype="dashed") +
  geom_ribbon(data=dat_pred[1:nAhead,],aes(x=t, ymin=pred05.R,ymax=pred95.R),alpha=0.2) +
  geom_ribbon(data=dat_pred[1:nAhead,],aes(x=t, ymin=pred05.Java,ymax=pred95.Java),alpha=0.2) +
  geom_ribbon(data=dat[800:N,],aes(x=t, ymin=fit05.Java,ymax=fit95.Java),alpha=0.2) +
  ggtitle(paste0("ARMAX(",p,",",d,",",q,") pred R-Java  comparison")) +
  labs(color="Legend text") +
  xlab('sample') +
  ylab('') + 
  scale_color_manual(values=c( "#000000", "#00ff00", "#0D9BE7", "#E70D73", "#A6B8C1"))

X11(); p0

var(y-fit.Java)
var(y-y.est.fit)
y.est

#rm(list=ls()) 









