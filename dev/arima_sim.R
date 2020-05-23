# http://numerical.recipes/CS395T/lectures2008/12-MaxLikelihoodEstimationAgain.pdf
# http://stats.stackexchange.com/questions/68080/basic-question-about-fisher-information-matrix-and-relationship-to-hessian-and-s
library(MASS)
library(ggplot2)
rm(list=ls()) 

setwd('~/repos/ssmodels/resources/')

source("~/repos/ssmodels/dev/initialize_arma.R")
source("~/repos/ssmodels/dev/log_likelihood2.R")
source("~/repos/ssmodels/dev/log_likelihood3.R")
source("~/repos/ssmodels/dev/log_likelihood4.R")
source("~/repos/ssmodels/dev/log_likelihood.R")
source("~/repos/ssmodels/dev/hessian.R")
source("~/repos/ssmodels/dev/arimaSS.R")
source("~/repos/ssmodels/dev/quickTimePlot.R")

nSteps = 10
p = 2; d = 0; q = 1;
params = c(p,d,q)
AR = c(0.05, 0.6, -0.2, 0.8, 0.2, 0.34); MA = c(0.2, 0.2, -0.5, 0.3, -0.13, -0.7)
#AR = c(0.05, 0.6, -0.2); MA = c(0.2, 0.2, -0.5, 0.3, -0.13, -0.7)

#
if(q == 0) {
  y = as.data.frame(arima.sim(list(order = c(p,d,q), ar =  AR[1:p]), n = 1000))
} else if (p == 0) {
  y = as.data.frame(arima.sim(list(order = c(p,d,q), ma = MA[1:q]), n = 1000))
} else {
  y = as.data.frame(arima.sim(list(order = c(p,d,q), ar =  AR[1:p],  ma = MA[1:q]), n = 1000))
}

t = 1:nrow(y)
#y = 1 + t*0.05 + y + 0.9*sin(pi*t*(1/6)) + 0.2*cos(pi*t*(1/6)) - 0.4*cos(pi*t*(2/6)) + 0.3*sin(pi*t*(2/6)) + 0.1*cos(pi*t*(3/6)) - 0.1*sin(pi*t*(3/6))
#y = 1 + t*0.05 + y + 0.9*sin(pi*t*(1/6)) + 0.2*cos(pi*t*(1/6)) - 0.4*cos(pi*t*(2/6)) + 0.3*sin(pi*t*(2/6)) 
y = 3 + t*0.05 + y 
#y = log(as.data.frame(UKDriverDeaths));




quickTimePlot(y,F)$p

setwd('~/repos/ssmodels/resources/')
write.table(y, file = "y_arima_test.csv", sep = ",", row.names = F, col.names = F)

y.est = arima(y, order = params)
y.est


