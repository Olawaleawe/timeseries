###Load Libraries
library(ggplot2)
library(forecast)
library(fpp2)
library(dplyr)
library(tseries)
library(fpp2)

############# Africa's Temperature##############################################
datat=read.csv("Africa_Temp.csv", header=TRUE)
datat
names(datat)
dim(datat)
str(datat)
names(datat)
datn=datat[,-c(1,2)]#
datn
names(datn)

####################Nigerian Temperature #################################################
nt=ts(nt,start='1991',frequency = 12)
nt=datat[,40]
nt
plot(nt)
################################################################################
fit1 <- holt(nt, alpha=0.8, beta=0.2, initial="simple", h=26) 
fit2 <- holt(nt, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=26) 
fit3 <- holt(nt, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=26) 
fit4 <- HoltWinters(nt,seasonal="multiplicative")
fit5= auto.arima(nt,seasonal=T,trace=T)
fit6=nnetar(nt, decay = 0.5, maxit = 150)


summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

forc1=forecast(fit1, h = 26);forc1
forc2=forecast(fit2, h = 26);forc2
forc3=forecast(fit3, h = 26);forc3
forc4=forecast(fit4, h = 26 );forc4
forc5=forecast(fit5, h = 26);forc5
forc6=forecast(fit6, h = 26);forc6
autoplot(nt)
ntd = decompose(nt)
ntd
plot(ntd)
#################################################################################
adf.test(nt)
lambda = BoxCox.lambda(nt, method = 'loglik')
lambda
##########################

par(mfrow = c(2,2))
checkresiduals(fit1)
checkresiduals(fit2)
#checkresiduals(fit3)
checkresiduals(fit4)
checkresiduals(fit6)

forecast.fit2 = forecast(fit2, h = 10)
forecast.fit6 = forecast(fit6, h = 10)
forecast.fit2
forecast.fit6
plot(nt, xlim = c(1991, 2020),  ylab = "Temperature", xlab = "Year", 
     main="10 Month Ahead Forecast")                       
lines(fit3$mean, col="purple", type = "o")
lines(fitted(fit3), col="purple")
lines(fitted(fit2), col="red")
lines(forecast.fit2$mean, col = "red", type = "o")
lines(fitted(fit6), col="green3", lty=1)
lines(forecast.fit6$mean, col = "green3", type = "o")
legend("topleft",lty=1, pch = 1, text.width = 12, col=c("black","purple", "green3", "red"), 
       c("Temperature","Holt", 'Holt-Winters', "ANN"))
###############################################################################

par(mfrow = c(2,1))
temp = c(0.663, 0.769, 0.879,0.047 )
plot(temp, ylab='Temperature', main='RMSE' ,type ='h', ylim= c(0,1.2), pch = 10, xlab='', lty='solid',col=rainbow(4), lwd=15, xaxt='n')
axis(1, at=c(1:4), labels=names)
rain = c(20.16, 33.01, 43.65, 18.99)
plot(rain, ylab= 'Rainfall',  type = 'h', col=rainbow(4), lwd=15, xaxt='n', xlab='Performance Metric')
axis(1, at=c(1:4), labels=names)
names = c('SES', 'HOLT', 'HOLT-WINTERS', 'ANN')
################################################################################
################################################################################
############################Africa's Rainfall####################################################
################################################################################
datar=read.csv("Africa_Rain.csv", header=TRUE)
datar
names(datar)
nr=datar[,40]
dim(datar)
str(datar)
names(datar)
dim(datar)
datnr=datar[,-c(1,2)]
datnr
names(datnr)

######## Nigerian Rainfall######################################################
nr=ts(nr,start='1991',frequency = 12)
nr
adf.test(nr)
plot(nr)
autoplot(nr)

################################################################################
f1 <- holt(nr, alpha=0.8, beta=0.2, initial="simple", h=26) 
f2 <- holt(nr, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=26) 
f3 <- holt(nr, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=26) 
f4 <- HoltWinters(nr,seasonal="multiplicative")
f5= auto.arima(nr,seasonal=T,trace=T)
f6=nnetar(nr, decay = 0.5, maxit = 150)


summary(f1)
summary(f2)
summary(f3)
summary(f4)
summary(f5)
summary(f6)

for1=forecast(f1, h = 26);for1
for2=forecast(f2, h = 26);for2
for3=forecast(f3, h = 26);for3
for4=forecast(f4, h = 26 );for4
for5=forecast(f5, h = 26);for5
for6=forecast(f6, h = 26);for6

nrd = decompose(nr)
nrd
plot(nrd)
########################################
library(fpp2)
par(mfrow = c(2,2))
checkresiduals(f1)
checkresiduals(f2)
#checkresiduals(fit3)
checkresiduals(f4)
checkresiduals(f6)


forecast.f2 = forecast(f2, h = 10)
forecast.f6 = forecast(f6, h = 10)
forecast.f2
forecast.f6
plot(nr, xlim = c(1991, 2020), ylim=c(-20,350), ylab = "Rainfall", xlab = "Year", 
     main="10 Month Ahead Forecast")                       
lines(f3$mean, col="purple", type = "o")
lines(fitted(f3), col="purple")
lines(fitted(f2), col="red")
lines(forecast.f2$mean, col = "red", type = "o")
lines(fitted(f6), col="green3", lty=1)
lines(forecast.f6$mean, col = "green3", type = "o")
legend("topleft",lty=1, pch = 1,  col=c("black","purple", "green3", "red"), 
       c("Rainfall","Holt", "Holt-Winters", "ANN"))



par(mfrow = c(2,1))

