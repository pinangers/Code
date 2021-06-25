######################################
#forecast 
#AMAT_132
#Santillana, Josephine P.
##############################

#load package
library(fpp2)
library(urca)
#read the data
data <- read.csv("/users/Josephine/Desktop/SEA_Vax.csv")

#declare that it is a time series
Y <-ts(data[,2],start=c(2021,1,11),frequency = 365.25)

#Preliminary Analysis
#Time Plot
autoplot(Y)+
  ggtitle("Time Plot: Number of Vaccinee in Southeast Asia per day")+
  xlab("Days")+
  ylab("Millions of Vaccinee")


#unit root test to determine if the differencing is required
Y %>% ur.kpss() %>% summary()

#Take the first difference
#Time plot of difference
DY <- diff(Y)

autoplot(DY)+
  ggtitle("Time Plot: Change in number of vaccinee in Southeast Asia per day")+
  xlab("Days")+
  ylab("Millions of Vaccinee")

#unit root test again
Y %>% diff() %>% ur.kpss() %>% summary()

#Use exponential smoothing in fitting the model
fit_ets <- ets(Y)
print(summary(fit_ets))
checkresiduals(fit_ets)

#Use ARIMA to fit the model
fit_arima <- auto.arima(Y, d=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)

#forecast with ARIMA model
fcst <- forecast(fit_arima,h=192)
autoplot(fcst)
print(summary(fcst))




