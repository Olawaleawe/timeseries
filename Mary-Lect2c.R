
## Time Series Forecasting
##install.packages(forecast)
##install.packages(fpp)
library(forecast)
data(elecsales, package = "fpp")
elecsales
plot(elecsales,ylab="Electricity", xlab="Year")
ma.1 <- ma(elecsales,order=5)
plot(elecsales, main="Electricity",
     ylab="GWh", xlab="Year")
lines(ma(elecsales,3),col="red")
lines(ma(elecsales,9),col="blue")
print(ma.1)
print(elecsales)
#################################################################################
data1=read.csv("AmericasTemp.csv", header=TRUE)
data1
bt=data1[,5]
dim(data1)
str(data1)
names(data1)
dim(data1)
dat1=data1[,-c(1,7)]
dat1
###################################################################################
library("ggplot2")
library("forecast")
data(oil, package = "fpp")
plot(oil)
oildata <- window(oil,start=1996,end=2007)
plot(oildata, ylab="Oil (millions of tonnes)", xlab="Year")
#Simple exponential smoothing
fit1 <- ses(oildata, alpha=0.2, initial="simple", h=3) #lo=y1 or initial=optimal
fit2 <- ses(oildata, alpha=0.6, initial="simple", h=3)
fit3 <- ses(oildata, h=3)
#Plot for alpha=0.2,0.6,0.89
plot(fit1, plot.conf=FALSE, ylab="Oil (millions of tonnes)",
     xlab="Year", main="", fcol="white", type="l")
lines(fitted(fit1), col="blue", type="l")
lines(fitted(fit2), col="red", type="l")
lines(fitted(fit3), col="green", type="l")
lines(fit1$mean, col="blue", type="l")
lines(fit2$mean, col="red", type="l")
lines(fit3$mean, col="green", type="l")
legend("topleft",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)),pch=1)
#level lt for each alpha
fit1$model$state
fit2$model$state
fit3$model$state
#fitted(fit1)
#fitted(fit2)
#fitted(fit3)
#Summary of results
summary(fit1)
summary(fit2)
summary(fit3)

library(forecast)
data(ausair, package = "fpp")
#original data - from 1970 until 2010
plot(ausair) 
air <- window(ausair,start=1990,end=2004)
fit1 <- holt(air, alpha=0.8, beta=0.2, initial="simple", h=5) 

fit2 <- holt(air, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=5) 
fit3 <- holt(air, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=5) 
plot(air,ylab="Air passengers", xlab="Year")
air
#print(air)
#Results for first model:
#level lt and trend bt
fit1$model$state 
#Fitted values (lt+bt)
fitted(fit1)
#Forecasts
fit1$mean
#Plots
#######################################################################
## Temprature Forecast
bt= read.csv('AmericasTemp.csv')
bt
bt=ts(bt,start='1991',frequency = 12)
##############################################
#####################################################################
#Temperature
############################################################
fit1 <- holt(bt, alpha=0.8, beta=0.2, initial="simple", h=26) 
fit2 <- holt(bt, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=26) 
fit3 <- holt(bt, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=26) 
fit4 <- hw(bt,seasonal="multiplicative", h=26)
fit5= auto.arima(bt,seasonal=T,trace=T)
fit6=nnetar(bt, decay = 0.5, maxit = 150)
#####################################################################
#####################################################################
plot(bt,ylab="Temperature", xlab="Year")
plot(fit2, type="l", ylab="Temperature", xlab="Year", 
     fcol="white", plot.conf=FALSE, main="")
lines(fitted(fit1), col="blue") 
lines(fitted(fit2), col="red")
lines(fitted(fit3), col="green")
lines(fitted(fit4),col='yellow')
lines(fitted(fit5),col='brown')
lines(fitted(fit6),col='wheat')

###############################################################

lines(fit1$mean, col="blue", type="l") 
lines(fit2$mean, col="red", type="l")
lines(fit3$mean, col="green", type="l")
lines(fit4$mean,col='yellow',type='l')
lines(fit5$mean,col='brown',type='l')
lines(fit6$mean,col='wheat',type='l')
legend("topleft", lty=1, col=c("black","blue","red","green",'yellow'), 
       c("Temperature","SES","HW-Exponential trend","Additive damped trend",'Multiplicative','SARIMA','ANN'))


#Forecast results
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


#plot(forc1,ylab='Temperature')
#plot(forc2,ylab='Temperature')
#plot(forc3,ylab= 'Temperature')
#plot(forc4,ylab='Temperature')
#plot(forc5,ylab = 'Temperature')
#plot(forc6,ylab = 'Temperature')

#accuracy(forc1)
#accuracy(forc2)
#accuracy(forc3)
#accuracy(forc4)
#accuracy(forc5)
#accuracy(forc6)
###########################################################################
##The best model is the model with the lowest RMSE
###########################################################################
## Rainfall Forecast
br= read.csv('AmericasRainfall.csv')
br = br[,5]
br = ts(br, start = 1991, frequency = 12)
plot(br)
#####################################################################
########Rainfall
############################################################
mod1 <- holt(br, alpha=0.8, beta=0.2, initial="simple", h=26) 
mod2 <- holt(br, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=26) 
mod3 <- holt(br, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=26) 
mod4 <- hw(br,seasonal="multiplicative", h = 26)
mod5= auto.arima(br,seasonal=T,trace=T)
mod6=nnetar(br, decay =0.5, maxit = 150)
#####################################################################
#####################################################################
plot(br,ylab="Rainfall", xlab="Year")
plot(mod2, type="l", ylab="Rainfall", xlab="Year", 
     fcol="white", plot.conf=FALSE, main="")
lines(fitted(mod1), col="blue") 
lines(fitted(mod2), col="red")
lines(fitted(mod3), col="green")
lines(fitted(mod4),col='yellow')
lines(fitted(mod5),col='brown')
lines(fitted(mod6),col='wheat')

###############################################################

lines(mod1$mean, col="blue", type="l") 
lines(mod2$mean, col="red", type="l")
lines(mod3$mean, col="green", type="l")
lines(mod4$mean,col='yellow',type='l')
lines(mod5$mean,col='brown',type='l')
lines(mod6$mean,col='wheat',type='l')
legend("topleft", lty=1, col=c("black","blue","red","green",'yellow'), 
       c("Rainfall","SES","HW-Exponential trend","Additive damped trend",'Multiplicative','SARIMA','ANN'))

##########################################################################################################
#Forecast results
summary(mod1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

ft1=forecast(mod1, h = 26);ft1
ft2=forecast(mod2, h  = 26);ft2
ft3=forecast(mod3, h = 26);ft3
ft4=forecast(mod4, h = 26);ft4
ft5=forecast(mod5, h = 26);ft5
ft6=forecast(mod6, h = 26);ft6


#plot(forc1,ylab='Rainfall')
#plot(forc2,ylab='Rainfall')
#plot(forc3,ylab= 'Rainfall')
#plot(forc4,ylab='Rainfall')
#plot(forc5,ylab = 'Rainfall')
#plot(forc6,ylab = 'Rainfall')

accuracy(forc1)
accuracy(forc2)
accuracy(forc3)
accuracy(forc4)
accuracy(forc5)
accuracy(forc6)
#####################################
###########################################################################
## Temperature
#seasonal=N,A,M



#########################################################################


mod3=HoltWinters(bt, alpha =T)
forcast3=forecast(mod3, h=12)
plot(forcast3,  ylab= 'Rainfall')
accuracy(forcast3)


#################ARIMA###################################################
#########################################################################
####################################################################
#######################################################################
king=read.csv("kings.csv")
plot(king)
acf(king)
pacf(king)
Box.test(king, 10)
Box.test(king, 10, type = "Ljung-Box")
ar(king)
kg.ar1 <- arima(king, c(3, 0, 0))
kg.ar2 <- arima(king, c(1, 0, 0)) #BIC
summary(kg.ar2)
tsdiag(kg.ar2) 
#Diagnostics
tsdiag(kg.ar1) 
#Automatic modelling: AR models
kgAR <- auto.arima(king, d=0, D=0, max.p=12, max.q=0, max.P=0, max.Q=0, 
                   max.order=12, ic="bic", stepwise=FALSE, approximation=FALSE)
summary(kgAR)
#Automatic modelling: ARMA models
kgARMA <- auto.arima(king,  trace=T, d=0, D=0, max.p=12, max.q=12, max.P=0, max.Q=0, 
                     max.order=6, ic="aic", stepwise=F, approximation=FALSE)
kgARMA
summary(kgARMA)

### Best model is ARIMA(1,0,0) with non-zero mean.

#library(forecast)
#data(austourists, package = "fpp")
#original data - from 1970 until 2010
#plot(austourists, ylab="Tourists", xlab="Year") 
#aust <- window(austourists,start=2005)
#print(aust)
fit1 <- hw(br,seasonal="additive")
fit2 <- hw(br,seasonal="multiplicative")
fit3 <- hw(br,seasonal="additive", damped=TRUE)
fit4 <- hw(br,seasonal="multiplicative", damped=TRUE)
fit5=  auto.arima(br,seasonal=T, trace=T)
fit6= nnetar(br)
#level lt, trend bt, and seasons 
fit1$model$state 
#Fitted values (lt+bt)
fitted(fit1)
#Forecasts
fit1$mean
plot(fit2,ylab="Temperature",
     plot.conf=FALSE, type="l", fcol="white", xlab="Year",main="")
lines(fitted(fit1), col="red", lty=2)
lines(fitted(fit2), col="green", lty=2)
lines(fit1$mean, type="l", col="red")
lines(fit2$mean, type="l", col="green")
lines(fitted(fit3), col="blue", lty=2)
lines(fitted(fit4), col="grey", lty=2)
lines(fit3$mean, type="l", col="blue")
lines(fit4$mean, type="l", col="grey")

legend("topleft",lty=1, pch=1, col=1:3, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))
#Forecast results
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)
##################################################################################
##################################################################################
##################################################################################
####Install packages#######################################################
##########################################################################
library(neuralnet)
library(tseries)
library(lmtest)
library(car)
library(MASS)
library(forecast)
library(tidyverse)
#############################################################################
###########################################################
##################################################################
###################################################################
###Brazil Temperature
##################################################################
data1=read.csv("AmericasTemp.csv", header=TRUE)
bt=data1[,5]
bt=ts(bt,start = 1991, frequency = 12)
plot(bt,ylab=" Brazilian Temperature" )

####ARIMA
mod2=auto.arima(bt,seasonal = T)
autoplot(mod2)
check_res(mod2)
forcast2=forecast(mod2)
forcast2=forecast(mod2, h=12)
plot(forcast2,ylab= 'Temperature')
accuracy(forcast2)

mod3=HoltWinters(b, alpha = T)
forcast3=forecast(mod3, h=12)
plot(forcast3,  ylab= 'Temperature')
accuracy(forcast3)

####Neural Network
library(neuralnet)
library(forecast)
mod=nnetar(bt)
forecast4=forecast(mod, h=12, level=95)
plot(forecast4, ylab= 'Temperature',xlab='Year')
accuracy(forecast4)
###############################################################
e1= tsCV(bt,ses,h=1)

f1=forecast(e1, h=12)
plot(f1, ylab= 'Temperature', xlab='Year')
accuracy(f1)

e2= tsCV(bt,holt,h=1)
f2=forecast(e2, h=12)
plot(f2, ylab= 'Temperature', xlab='Year')
accuracy(f2)

e3= tsCV(bt,holt,damped=T, h=1)
f3=forecast(e3,h=12)
accuracy(f3)
###############################################################
acf(bt, main='')
pacf(bt,main='')
plot(bt,ylab='Brazillian Temperature',col=2)
library(tseries)
library(urca)
library(TSA)
library(Kendall)
adf.test(bt)

SeasonalMannKendall((bt))
plot((bt), ylab='Brazillian Temperature')
lines(lowess(time(bt),bt), col='green')

#####################################################################
###########RAINFALL
#####################################################################
##################################################################
###################################################################
data2=read.csv("AmericasRainfall.csv", header=TRUE)
data2
br=data2[,5]
br=ts(br,start = 1991, frequency = 12)
plot(br,ylab=" Brazilian Rainfall" )

####SARIMA
mod2=auto.arima(b2,seasonal = T)
autoplot(mod2)
checkresiduals(mod2)
forcast2=forecast(mod2)
forcast2=forecast(mod2, h=12)
plot(forcast2,ylab= 'Rainfall')
accuracy(forcast2)

mod3=HoltWinters(br, alpha =T)
forcast3=forecast(mod3, h=12)
plot(forcast3,  ylab= 'Rainfall')
accuracy(forcast3)

####Neural Network
library(neuralnet)
library(forecast)
mod=nnetar(br)
forecast4=forecast(mod, h=12, level=95)
plot(forecast4, ylab= 'Rainfall',xlab='Year')
accuracy(forecast4)
###############################################################
e1= tsCV(br,ses,h=1)
f1=forecast(e1, h=12)
plot(f1, ylab= 'Rainfall', xlab='Year')
accuracy(f1)

e2= tsCV(br,holt,h=1)
f2=forecast(e2, h=12)
plot(f2, ylab= 'Rainfall', xlab='Year')
accuracy(f2)

e3= tsCV(br,holt,damped=T, h=1)
f3=forecast(e3,h=12)
plot(f3)
accuracy(f3)


mod=nnetar(br)
forecast4=forecast(mod, h=12)
plot(forecast4, ylab= 'Temperature')
accuracy(forecast4)

###################################################################
###################################################################
######Lowess Plots
###################################################################
par(mfrow=c(2,2))
plot(bt,ylab=" Brazilian Temperature")
plot(br,ylab=" Brazilian Rainfall")

plot(bt,ylab=" Brazilian Temperature")
lines(lowess(time(bt),bt), col='green')
plot(br,ylab=" Brazilian Rainfall")
lines(lowess(time(br),br), col='green')


############################################################################
#################################################################################
#####################################################################
#Temperature
############################################################
###############################################################################
data1=read.csv("AmericasTemp.csv", header=TRUE)
data1
bt=data1[,5]
dim(data1)
str(data1)
names(data1)
dim(data1)
dat1=data1[,-c(1,7)]
dat1
bt=ts(bt,start='1991',frequency = 12)
####################################################################################
fit1 <- holt(bt, alpha=0.8, beta=0.2, initial="simple", h=26) 
fit2 <- holt(bt, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=26) 
fit3 <- holt(bt, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=26) 
fit4 <- HoltWinters(bt,seasonal="multiplicative")
fit5= auto.arima(bt,seasonal=T,trace=T)
fit6=nnetar(bt, decay = 0.5, maxit = 150)

forc1=forecast(fit1, h = 26);forc1
forc2=forecast(fit2, h = 26);forc2
forc3=forecast(fit3, h = 26);forc3
forc4=forecast(fit4, h = 26 );forc4
forc5=forecast(fit5, h = 26);forc5
forc6=forecast(fit6, h = 26);forc6
################################################################################

####Plot of Temperature
#######################################################################################
fit1=c(26.8026, 26.9212, 27.0398, 27.1584, 27.2770, 27.3956, 27.5143, 27.6329, 27.7516, 27.8702, 27.9888, 28.1074, 28.2260, 28.3446, 28.4632, 28.5819, 28.7005, 28.8191, 28.9377, 29.0564, 29.1750, 29.2936, 29.4123, 29.5309, 29.6495, 29.7681)
length(fit1)
fit2=c(26.8199, 26.9519, 27.0846, 27.2178, 27.3519, 27.4865, 27.6218, 27.7578, 27.8944, 28.0317, 28.1697, 28.3084, 28.4477, 28.5877, 28.7285 ,28.8699, 29.0120 , 29.1548, 29.2983 , 29.4425 , 29.5875 , 29.7331 , 29.8795 , 30.0265, 30.1743 , 30.3229)
length(fit2)
fit3 = c(26.7179, 26.7532 , 26.7814 , 26.8040 , 26.8221 , 26.8365, 26.8481, 26.8573, 26.8647, 26.8707, 26.8754, 26.8792, 26.8822, 26.8846, 26.8866, 26.8881, 26.8894, 26.8904, 26.8912, 26.8918, 26.8923, 26.8927, 26.8930, 26.8933 , 26.8935, 26.8937)
length(fit3)
fit4 = c(26.6258, 26.5136, 26.5040, 26.1090, 25.2203, 24.6162, 24.4263, 25.3707, 26.6589, 27.0268, 29.7916, 26.6248, 26.6357, 26.5235, 26.5138, 26.1186, 25.2297, 24.6253, 24.4354, 25.3801, 26.6688, 27.0369, 26.8015, 26.6347, 26.6456, 26.5333)
length(fit4)
fit5 = c(216.2992, 206.0131, 224.068, 181.3423, 131.2396, 81.6417, 59.4806, 48.4747, 60.2697, 110.5524, 156.3328, 178.9330, 224.3237, 207.4583, 223.1162, 181.1793, 127.0148, 83.9460, 57.6122, 49.0292, 60.0504, 109.9300, 157.6829, 179.0502, 220.5178, 206.6567)
length(fit5)
fit6=c(26.60010, 26.31272, 26.27324, 26.09223, 25.39000, 24.54112, 24.36022, 25.42036, 26.48345, 26.97312, 26.70671, 26.56978,
26.42159, 26.31663, 26.28926, 25.95573, 25.13437, 24.50675, 24.41104, 25.31760, 26.48464, 26.88564, 26.72972, 26.57186
,26.41126, 26.24665 )  
length(fit6)


par(mfrow=c(3,2))
plot(ts(fit1, start = 2021, frequency = 12))
plot(ts(fit2, start = 2021, frequency = 12))
plot(ts(fit3, start = 2021, frequency = 12))
plot(ts(fit4, start = 2021, frequency = 12))
plot(ts(fit5, start = 2021, frequency = 12))
plot(ts(fit6, start = 2021, frequency = 12))
###########################################################################################################################
#######################################################################################################################
#############################################################################################
########################################################################
br= read.csv('AmericasRainfall.csv')
br = br[,5]
br
br = ts(br, start = 1991, frequency = 12)
br
plot(br)
#####################################################################
########Rainfall
############################################################
mod1 <- holt(br, alpha=0.8, beta=0.2, initial="simple", h=26) 
mod2 <- holt(br, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=26) 
mod3 <- holt(br, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=26) 
mod4 <- HoltWinters(br,seasonal="multiplicative")
mod5= auto.arima(br,seasonal=T,trace=T)
mod6=nnetar(br, decay =0.5, maxit = 150)


ft1=forecast(mod1, h = 26);ft1
ft2=forecast(mod2, h= 26);ft2
ft3=forecast(mod3, h = 26);ft3
ft4=forecast(mod4, h = 26);ft4
ft5=forecast(mod5, h = 26);ft5
ft6=forecast(mod6, h = 26);ft6
################################################################################################################
#################################################################################################################
#######Plots of Rainfall Forecasts
#############################################################################################

f1=c(187.1709, 199.7135, 212.2561, 224.7987, 237.3414, 249.8840, 262.4266, 274.9692, 287.5118, 300.0544, 312.5970, 325.1397, 337.6823, 350.2249, 362.7675, 375.3101, 387.8527, 400.3954, 412.9380, 425.4806, 438.0232, 450.5658, 463.1084, 475.6510, 488.1937, 500.7363)
f1
f1=ts(f1, start=2021, frequency = 12)
plot(f1)
#axis(1, at=year)
length(f1)
f2=c(208.9535, 244.8965, 287.0222, 336.3941, 394.2587, 462.0729, 541.5608, 634.7170, 743.8974, 871.8585, 1021.8306, 1197.6002, 1403.6046, 1645.0448, 1928.0161, 2259.6626, 2648.3570, 3103.9125, 4263.5890, 4996.9876, 3637.8300, 5856.5413, 6863.9505, 8044.6486, 9428.4437, 11050.2714)
length(f2)
f3 = c(189.2139, 199.9109, 208.4684, 215.3144, 220.7912, 225.1727, 228.6779, 231.4820, 233.7253, 235.5200, 236.9557, 238.1042, 239.0231, 239.7582, 240.3463, 240.8167, 241.1931, 241.4942, 241.7350, 241.9277, 242.0819, 242.2052, 242.3039, 242.3828, 242.4460, 242.4965)
length(f3)
f4 = c(217.0541,213.1422, 226.6809, 181.5060, 127.7093, 76.8797, 60.7354, 51.3949, 66.3941, 113.6601, 155.6158, 188.8219, 217.7092, 213.7854, 227.3647, 182.0534, 128.0944, 77.1115, 60.9185, 51.5498, 66.5941, 114.00238, 156.0843, 189.3902, 218.3643, 214.4285)
length(f4)
f5 = c(219.2992, 206.0132, 224.0682, 181.3423, 131.2396, 81.6417, 59.4806, 48.4747, 60.2697, 110.5524, 156.3328, 178.9330, 224.3237, 207.4583, 223.1162, 181.1793, 127.0148, 83.9460, 57.6122, 49.0292, 60.0504, 109.9300,  157.6829, 179.0502, 220.5178, 206.6567)
length(f5)
f6=c(26.60298, 26.327762, 26.27151, 26.11394, 25.41625, 24.54490, 24.35429, 25.41579, 26.46018, 26.97927, 26.71948, 26.56491,
     26.42478, 26.32980, 26.30147, 25.97951, 25.15736, 24.52841, 24.40592, 25.30244, 26.47167, 26.89152, 26.75767, 26.57943,
     26.41765, 26.26537)  
length(f6)

year=seq(as.Date("2021/01"), by="1 month", length.out=26)
year



br = ts(br, start = 1991, frequency = 12)

f1=ts(f1, start=2021, frequency = 12)
plot(f1)



#par(mfrow=c(3,2))
plot(f1)
axis(1,at=year)
plot(ts(f2, start = 2021, frequency = 12))
f3=as.numeric(f3)
plot(ts(f3, start = 2021, frequency = 12))
plot(ts(f4, start = 2021, frequency = 12))
plot(ts(f5, start = 2021, frequency = 12))
plot(ts(f6, start = 2021, frequency = 12))

###################################################################################################################################################################################################################################################
##############
#######################################################################################################
############################################################################################################################################
dates<-seq(from=as.Date("2021/01/01"), by="month", length.out=26)

par(mfrow=c(2,2))
#Rainfall
#y<-f1
#myts<-ts(data=y,
#         start=c(as.numeric(format(min(dates),"%Y")),
#                 as.numeric(format(min(dates),"%m"))),
 #        frequency=12,
#         deltat=1/12)
#plot(myts,ylab='Y',xlab='Date',type='l')

#dates_label = as.character(dates)
#plot(x = dates, y, las = 2, xaxt = "n", xlab = "", type = "l", ylab='SES Rainfall Forecasts ',col=1 )
#axis(1, at = dates, labels = dates_label, las = 2, cex.axis = .85)

##
#y<-f2
#myts<-ts(data=y,
  #       start=c(as.numeric(format(min(dates),"%Y")),
 #                as.numeric(format(min(dates),"%m"))),
 #        frequency=12,
 #        deltat=1/12)
#plot(myts,ylab='Y',xlab='Date',type='l')

#dates_label = as.character(dates)
#plot(x = dates, y, las = 2, xaxt = "n", xlab = "", type = "l", ylab='Holt Rainfall Forecasts ',col=2 )
#axis(1, at = dates, labels = dates_label, las = 2, cex.axis = .85)

############################################################################################################
y<-f3
myts<-ts(data=y,
         start=c(as.numeric(format(min(dates),"%Y")),
                 as.numeric(format(min(dates),"%m"))),
         frequency=12,
         deltat=1/12)
#plot(myts,ylab='Y',xlab='Date',type='l')

dates_label = as.character(dates)
plot(x = dates, y, las = 2, xaxt = "n", xlab = "", type = "l", ylab='Damped Trend',col= 3 )
axis(1, at = dates, labels = dates_label, las = 2, cex.axis = .85)

############################################################################################################
y<-f4
myts<-ts(data=y,
         start=c(as.numeric(format(min(dates),"%Y")),
                 as.numeric(format(min(dates),"%m"))),
         frequency=12,
         deltat=1/12)
#plot(myts,ylab='Y',xlab='Date',type='l')

dates_label = as.character(dates)
plot(x = dates, y, las = 2, xaxt = "n", xlab = "", type = "l", ylab='Holt-Winters',col= 4 )
axis(1, at = dates, labels = dates_label, las = 2, cex.axis = .85)

############################################################################################################
y<-f5
myts<-ts(data=y,
         start=c(as.numeric(format(min(dates),"%Y")),
                 as.numeric(format(min(dates),"%m"))),
         frequency=12,
         deltat=1/12)
#plot(myts,ylab='Y',xlab='Date',type='l')

dates_label = as.character(dates)
plot(x = dates, y, las = 2, xaxt = "n", xlab = "", type = "l", ylab='SARIMA',col= 5 )
axis(1, at = dates, labels = dates_label, las = 2, cex.axis = .85)

############################################################################################################
y<-f6
myts<-ts(data=y,
         start=c(as.numeric(format(min(dates),"%Y")),
                 as.numeric(format(min(dates),"%m"))),
         frequency=12,
         deltat=1/12)
#plot(myts,ylab='Y',xlab='Date',type='l')

dates_label = as.character(dates)
plot(x = dates, y, las = 2, xaxt = "n", xlab = "", type = "l", ylab='ANN',col= 6 )
axis(1, at = dates, labels = dates_label, las = 2, cex.axis = .85)




########################################################################################################################################
####################################################################################################################################################
###################################################################################################################################################################
#########################################################################################################################################################################################
##### Temperature


par(mfrow=c(2,2))
#TEMPERATURE
#y<-fit1
#myts<-ts(data=y,
#         start=c(as.numeric(format(min(dates),"%Y")),
#                 as.numeric(format(min(dates),"%m"))),
#         frequency=12,
#         deltat=1/12)
#plot(myts,ylab='Y',xlab='Date',type='l')

#dates_label = as.character(dates)
#plot(x = dates, y, las = 2, xaxt = "n", xlab = "", type = "l", ylab='SES Temprature Forecasts ',col=1 )
#axis(1, at = dates, labels = dates_label, las = 2, cex.axis = .85)

##
#y<-fit2
#myts<-ts(data=y,
 #        start=c(as.numeric(format(min(dates),"%Y")),
 #                as.numeric(format(min(dates),"%m"))),
#         frequency=12,
#         deltat=1/12)
#plot(myts,ylab='Y',xlab='Date',type='l')

#dates_label = as.character(dates)
#plot(x = dates, y, las = 2, xaxt = "n", xlab = "", type = "l", ylab='Holt Temprature Forecasts ',col=2 )
#axis(1, at = dates, labels = dates_label, las = 2, cex.axis = .85)

############################################################################################################
y<-fit3
myts<-ts(data=y,
         start=c(as.numeric(format(min(dates),"%Y")),
                 as.numeric(format(min(dates),"%m"))),
         frequency=12,
         deltat=1/12)
#plot(myts,ylab='Y',xlab='Date',type='l')

dates_label = as.character(dates)
plot(x = dates, y, las = 2, xaxt = "n", xlab = "", type = "l", ylab='Damped Trend ',col= 3 )
axis(1, at = dates, labels = dates_label, las = 2, cex.axis = .85)
abline(v = c(7, 19), col="red")
############################################################################################################
y<-fit4
myts<-ts(data=y,
         start=c(as.numeric(format(min(dates),"%Y")),
                 as.numeric(format(min(dates),"%m"))),
         frequency=12,
         deltat=1/12)
#plot(myts,ylab='Y',xlab='Date',type='l')

dates_label = as.character(dates)
plot(x = dates, y, las = 2, xaxt = "n", xlab = "", type = "l", ylab='Holt-Winters ',col= 4 )
axis(1, at = dates, labels = dates_label, las = 2, cex.axis = .85)
abline(v = c(7, 19), col="red")
############################################################################################################
y<-fit5
myts<-ts(data=y,
         start=c(as.numeric(format(min(dates),"%Y")),
                 as.numeric(format(min(dates),"%m"))),
         frequency=12,
         deltat=1/12)
#plot(myts,ylab='Y',xlab='Date',type='l')

dates_label = as.character(dates)
plot(x = dates, y, las = 2, xaxt = "n", xlab = "", type = "l", ylab='SARIMA',col= 5 )
axis(1, at = dates, labels = dates_label, las = 2, cex.axis = .85)
abline(v = c(7, 19), col="red")
############################################################################################################
y<-fit6
myts<-ts(data=y,
         start=c(as.numeric(format(min(dates),"%Y")),
                 as.numeric(format(min(dates),"%m"))),
         frequency=12,
         deltat=1/12)
#plot(myts,ylab='Y',xlab='Date',type='l')

dates_label = as.character(dates)
plot(x = dates, y, las = 2, xaxt = "n", xlab = "", type = "l", ylab='ANN',col= 6 )
axis(1, at = dates, labels = dates_label, las = 2, cex.axis = .85)
abline(v = c(7, 19), col="red")
############################################################################################################



#####################################################


library(changepoint)
value = cpt.mean(fit6) #mean changepoints using PELT
cpts(mvalue)

#The command “cpts” lists the changepoints.   Time to try BinSeg.

mvalue = cpt.mean(value.ts, method="BinSeg")
cpts(mvalue)
plot(mvalue)

###variance

vvalue = cpt.var(diff(value.ts), method=”PELT”)
cpts(vvalue)

 plot(vvalue)
 par(mfrow=c(2,1))
plot(value.ts)
plot(vvalue)


#try method “SegNeigh” (segment neighborhoods)

vnvalue = cpt.var(diff(value.ts), method=”SegNeigh”, Q=6) #the Q=6 caps the maximum number of changepoints at 6



d = decompose(ts(br),frequency=12)
plot(d)

cpt =cpt.mean(diff(br), method="PELT")
plot(cpt)

###############################################


#####################################################
#Plots of Temp Changepoints
#####################################################
#par(mar = c(4, 3, 3, 1))
par(mfrow=c(3,1))
plot(fit4, type='l', main='Change-Points in Forecasts with Holt-Winters', xlab ='')
abline(v = c(7,11, 19, 22), col="red")

plot(fit5, type='l', main='Change-Points in Forecasts with SARIMA', xlab ='')
abline(v = c(3,8,14, 20), col="blue")

plot(fit6, type='l', main='Change-Points in Forecasts with ANN', xlab ='Year')
abline(v = c(7,10, 19, 22), col="purple")

###############################################################################
#Plots of Rainfall Changepoints
#par(mar = c(4, 3, 3, 1))
par(mfrow=c(3,1))
plot(f4, type='l', main='Change-Points in Forecasts with Holt-Winters', xlab ='')
abline(v = c(3,15,20), col="red")

plot(f5, type='l', main='Change-Points in Forecasts with SARIMA', xlab ='')
abline(v = c(3, 9, 21), col="blue")

plot(f6, type='l', main='Change-Points in Forecasts with ANN', xlab ='Year')
abline(v = c(7,10, 19), col="purple")
##########################################################################################4
plot(bt, type='l', main='Change-Points in Forecasts with SARIMA', xlab ='')
abline(v = c(3, 9, 21), col="blue")

plot(br, type='l', main='Change-Points in Forecasts with ANN', xlab ='Year')
abline(v = c(7,10, 19), col="purple")
p=decompose(br)
plot(p)
t=decompose(bt)
plot(t)
