library(MASS)
library(ggplot2)

setwd('~/repos/ssmodels/resources/')

N = 100
fit <- StructTS(y,"trend")
y.est.fit = fit$fitted[,1]

#####################################################

fit.Java = as.data.frame(read.table("y_hat_ll.csv", header=F, sep=","));
fit.se.Java = as.data.frame(read.table("y_hat_se_ll.csv", header=F, sep=","));
fit.se.05.Java = fit.Java-1.645*fit.se.Java;
fit.se.95.Java = fit.Java+1.645*fit.se.Java;

dat = data.frame(t= 1:nrow(y), y=y, fit.R = y.est.fit, 
                 fit.Java  = fit.Java, 
                 fit05.Java = fit.se.05.Java, 
                 fit95.Java = fit.se.95.Java)
colnames(dat) = c('t','y','fit.R','fit.Java', 'fit05.Java','fit95.Java')

p0 = ggplot() +
  geom_line(data=dat[2:N,], aes(x=t, y = y, colour = "Raw"), size=1,linetype="dashed") + 
  geom_line(data=dat[2:N,], aes(x=t, y = fit.R, colour = "R Fit")) +
  geom_line(data=dat[2:N,], aes(x=t, y = fit.Java , colour = "Java Fit"), size=1,linetype="dashed") +
  geom_ribbon(data=dat[2:N,],aes(x=t, ymin=fit05.Java,ymax=fit95.Java),alpha=0.2) +
  ggtitle(paste0("Local Level Trend Model R-Java Comparison")) +
  labs(color="Legend text") +
  xlab('sample') +
  ylab('') + 
  scale_color_manual(values=c( "#000000", "#00ff00", "#0D9BE7", "#E70D73", "#A6B8C1"))

X11(); p0

var(fit$residuals)

fit$coef


#rm(list=ls()) 









