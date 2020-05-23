library(MASS)
library(ggplot2)
setwd('~/repos/ssmodels/dev/')
source("~/repos/ssmodels/dev/initialize_arma.R")
source("~/repos/ssmodels/dev/log_likelihood.R")

p = 2;
d = 1
q = 2;

AR = c(0.05, 0.6, -0.2)
MA = c(0.2, 0.2, -0.5)

y = as.data.frame(read.table("y_arima2_test.csv", header=F, sep=","));
#y = as.data.frame(arima.sim(list(order = c(p,d,q), ar =  AR[1:p],  ma = MA[1:q]), n = 1000))

if(d > 0){
  yproc = diff(as.matrix(y), differences = d)
} else {
  yproc = y
}

# function: initalize_arma
params = c(p,d,q)
coefs = initialize_arma(yproc,params)
optimize = optim(par=coefs, fn=log_likelihood2,dat=yproc,ord=params,type=0)
coefs_mle = optimize$par

y.est.fit.RCode = log_likelihood2(coefs_mle,params,y,1)
y.est.pred.RCode = log_likelihood2(coefs_mle,params,y,2)

#####################################################

y.est = arima(y, order = c(p,d,q))
y.est.fit = y - as.data.frame(y.est$residuals) 
y.est.pred = predict(y.est, n.ahead = 100); y.est.pred = as.matrix(y.est.pred$pred)

dat = data.frame(t= 1:nrow(y), y=y, fit.R = y.est.fit, fit.Rcode  = log_likelihood(coefs_mle,params,y,1))
colnames(dat) = c('t','y','fit.R','fit.Rcode')

dat_pred = data.frame(t= (nrow(y)+1):(nrow(y.est.pred)+nrow(y)), pred.R = y.est.pred, pred.Rcode = log_likelihood(coefs_mle,params,y,2) )
colnames(dat_pred) = c('t','pred.R','pred.Rcode')

p = ggplot() +
  geom_line(data=dat[900:1000,], aes(x=t, y = y, colour = "Raw"), size=1,linetype="dashed") + 
  geom_line(data=dat[900:1000,], aes(x=t, y = fit.R, colour = "R Fit")) +
  geom_line(data=dat[900:1000,], aes(x=t, y = fit.Rcode, colour = "Rcode Fit"), size=1,linetype="dashed") +
  geom_line(data=dat_pred[1:10,], aes(x=t, y = pred.R, colour = "R Pred")) +
  geom_line(data=dat_pred[1:10,], aes(x=t, y = pred.Rcode, colour = "Rcode Pred"), size=1,linetype="dashed") +
  ggtitle("ARMA(2,2) pred R-Rcode comparison") +
  labs(color="Legend text") +
  xlab('sample') +
  ylab('') + 
  scale_color_manual(values=c( "#000000", "#00ff00", "#0D9BE7", "#E70D73", "#A6B8C1"))

X11(); p

#####################################################

library(MASS)
library(ggplot2)
rm(list=ls()) 

setwd('~/repos/ssmodels/resources/')
source("~/repos/ssmodels/dev/initialize_arma.R")
source("~/repos/ssmodels/dev/log_likelihood2.R")
source("~/repos/ssmodels/dev/log_likelihood3.R")
source("~/repos/ssmodels/dev/log_likelihood.R")

nSteps = 100
p = 2; d = 1; q = 2;
AR = c(0.05, 0.6, -0.2); MA = c(0.2, 0.2, -0.5)

#y = as.data.frame(arima.sim(list(order = c(p,d,q), ar =  AR[1:p],  ma = MA[1:q]), n = 1000))
y = as.data.frame(read.table("y_arima2_test.csv", header=F, sep=","));

if(d > 0){
  yproc = diff(as.matrix(y), differences = d)
  yproc = yproc - mean(yproc)
  params = c(p,0,q)
} else {
  yproc = y
  params = c(p,0,q)
}

params = c(p,d,q)

# R implementation
y.est = arima(y, order = params)
y.est.fit = y - as.data.frame(y.est$residuals) 
y.est.pred = predict(y.est, n.ahead = nSteps); y.est.pred = as.matrix(y.est.pred$pred)

# My implementation
coefs = initialize_arma(yproc,params)
optimize = optim(par=coefs, fn=log_likelihood2,y0=yproc,ord=params,type=0)
coefs_mle = optimize$par

y.est.fit.RCode = log_likelihood2(coefs_mle,c(p,d,q),y,1)
y.est.pred.RCode = log_likelihood2(coefs_mle,c(p,d,q),y,2)

data = data.frame(t= 1:nrow(y), y=y, fit.R = y.est.fit, fit.Rcode  = y.est.fit.RCode)
colnames(data) = c('t','y','fit.R','fit.Rcode')

data_pred = data.frame(t= (nrow(y)+1):(nrow(y.est.pred)+nrow(y)), pred.R = y.est.pred, pred.Rcode = y.est.pred.RCode )
colnames(data_pred) = c('t','pred.R','pred.Rcode')

p0 = ggplot() +
  geom_line(data=data[800:1000,], aes(x=t, y = y, colour = "Raw"), size=1,linetype="dashed") + 
  geom_line(data=data[800:1000,], aes(x=t, y = fit.R, colour = "R Fit")) +
  geom_line(data=data[800:1000,], aes(x=t, y = fit.Rcode, colour = "Rcode Fit"), size=1,linetype="dashed") +
  geom_line(data=data_pred[1:nSteps,], aes(x=t, y = pred.R, colour = "R Pred")) +
  geom_line(data=data_pred[1:nSteps,], aes(x=t, y = pred.Rcode, colour = "Rcode Pred"), size=1,linetype="dashed") +
  ggtitle(paste0("ARMA(",p,",",d,",",q,") pred R-Rcode comparison")) +
  labs(color="Legend text") +
  xlab('sample') +
  ylab('') + 
  scale_color_manual(values=c( "#000000", "#00ff00", "#0D9BE7", "#E70D73", "#A6B8C1"))

X11(); p0

#####################################################

setwd('~/repos/ssmodels/resources/')
write.table(y, file = "y_arima2_test.csv", sep = ",", row.names = F, col.names = F)

rm(list=ls()) 










