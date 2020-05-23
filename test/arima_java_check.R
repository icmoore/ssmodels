library(MASS)
library(ggplot2)

setwd('~/repos/ssmodels/resources/')

p = 2; d = 1; q = 2;

y = as.data.frame(read.table("y_arima2_test.csv", header=F, sep=","));

#####################################################

y.est = arima(y, order = c(p,d,q))
y.est.fit = y - as.data.frame(y.est$residuals) 
y.est.pred = predict(y.est, n.ahead = 100); y.est.pred = as.matrix(y.est.pred$pred)

fit.Java = as.data.frame(read.table("y_hat_arima.csv", header=F, sep=","));
pred.Java = as.data.frame(read.table("y_pred_arima.csv", header=F, sep=","));

dat = data.frame(t= 1:nrow(y), y=y, fit.R = y.est.fit, fit.Java  = fit.Java)
colnames(dat) = c('t','y','fit.R','fit.Java')

dat_pred = data.frame(t= (nrow(y)+1):(nrow(y.est.pred)+nrow(y)), pred.R = y.est.pred, pred.Java = pred.Java  )
colnames(dat_pred) = c('t','pred.R','pred.Java')

p0 = ggplot() +
  geom_line(data=dat[800:1000,], aes(x=t, y = y, colour = "Raw"), size=1,linetype="dashed") + 
  geom_line(data=dat[800:1000,], aes(x=t, y = fit.R, colour = "R Fit")) +
  geom_line(data=dat[800:1000,], aes(x=t, y = fit.Java , colour = "Java Fit"), size=1,linetype="dashed") +
  geom_line(data=dat_pred[1:100,], aes(x=t, y = pred.R, colour = "R Pred")) +
  geom_line(data=dat_pred[1:100,], aes(x=t, y = pred.Java , colour = "Java Pred"), size=1,linetype="dashed") +
  ggtitle(paste0("ARMA(",p,",",d,",",q,") pred R-Java  comparison")) +
  labs(color="Legend text") +
  xlab('sample') +
  ylab('') + 
  scale_color_manual(values=c( "#000000", "#00ff00", "#0D9BE7", "#E70D73", "#A6B8C1"))

X11(); p0










