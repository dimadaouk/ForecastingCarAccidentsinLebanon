library(ggfortify)
library(fpp2)
library(forecast)
library(ggplot2)
library(urca)
library(tseries)

#load data

rm(list=ls())
df = read.csv('/Users/dimadaouk/Desktop/Forecasting Final Project/Car Accidents Files/CarAccidents.csv')
accidents <- ts(df$accidents, frequency=12, start=c(2007,1), end=c(2018,12))
accidents

#visualize data

plot(accidents)
autoplot(accidents) + ylab("Number of Accidents") + xlab("Year") + ggtitle("Monthly Accidents in Lebanon")
summary(accidents)
ggseasonplot(accidents,year.labels=TRUE,year.labels.left=TRUE, polar=TRUE)+ ylab("Accidents") +
  ggtitle("Seasonal plot: car accidents in Lebanon")

ggtsdisplay(accidents)


#remove outlier
df = read.csv('/Users/T/Desktop/lara/317/CarAccidents.csv')
accidents <- ts(df$accidents, frequency=12, start=c(2007,1), end=c(2016,11))
accidents

#split train and test

train <- window(accidents, start=c(2007,1), end=c(2014,12))
test <- window(accidents, start=c(2015,1), end=c(2016,11))

length(train)
length(test)
length(accidents)
summary(train)

#check if data stationary or not

#adf test to prove non-seasonality and non-stationary data
adf.test(train) #p-value is 0.01 < 0.05, we reject H0 which means data is stationary and seasonal

#kpss test to check for stationary and seasonality data
summary(ur.kpss(train)) 


#check for differencing
#since data is seasonal 
nsdiffs(accidents)
#there is a need for differencing


difference <- train %>% diff(lag=12)
nsdiffs(difference)
summary(ur.kpss(difference))

autoplot(difference)
ggtsdisplay(difference)
ggAcf(difference)
ggPacf(difference)

optimal <- auto.arima(train)
#ARIMA(1,0,0)(2,1,1)[12]

model <- forecast(optimal, h=1)

checkresiduals(model)
#residuals are white noise -> good model, p-value 01 > 0.05 so accept H0 => residuals uncorrelated/are white noise

autoplot(forecast(Arima(accidents, order=c(1,0,0), seasonal=list(order=c(2,1,1), period=12)), h=1))
#point forecast = 230 replaced by outlier of 110



#Do arima on whole data with outlier replaced

df = read.csv('/Users/T/Desktop/lara/317/CarAccidents.csv')
#replaced outlier with forecasted value
data<-replace(df$accidents, df$accidents==110, 230)
data

accidents <- ts(data, frequency=12, start=c(2007,1), end=c(2018,12))
accidents

#visualize data

plot(accidents)
autoplot(accidents) + ylab("Number of Accidents") + xlab("Year") + ggtitle("Monthly Accidents in Lebanon")

ggseasonplot(accidents,year.labels=TRUE,year.labels.left=TRUE, polar=TRUE)+ ylab("Accidents") +
  ggtitle("Seasonal plot: car accidents in Lebanon")


#split train and test

train <- window(accidents, start=c(2007,1), end=c(2016,12))
test <- window(accidents, start=c(2017,1), end=c(2018,12))

length(train)
length(test)
summary(train)

adf.test(train) #p-value 0.01<0.05 -> reject H0 => data stationary and seasonal
summary(ur.kpss(train)) #p-value 0.38 => less than almost all critical values -> reject H0 => data non-stationary and seasonal
#conflicting results in tests so check ndiffs

nsdiffs(train)
ndiffs(train)

#seasonal difference
difference <- train %>% diff(lag=12) 
ndiffs(difference)

#we need an additional difference
difference <- train %>% diff(lag=12) %>% diff()
ndiffs(difference)

#to check if differenced data is stationary
summary(ur.kpss(difference)) #t = 0.0331 < all critical values => data stationary

autoplot(difference)
ggtsdisplay(difference)
ggAcf(difference)
ggPacf(difference)
#acf shows 1 significant lag at the seasonal lag
#Pacf shows overall sinusoidal behaviour
#q has higher effect than p

#since we want to build our model on the train set, the difference should then made on the train set
#since the train set needs 2 differences; 1 seasonal difference and 1 non-seasonal difference, our arima model should be based on these findings

auto.arima(train)

#model optimization 

model1 <- Arima(train, order = c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
AIC1 = model1$aicc
rmse1 <- accuracy(forecast(model1, h=12)$mean, test)[2]

model2 <- Arima(train, order = c(0,1,2), seasonal=list(order=c(0,1,1), period=12))
AIC2 = model2$aicc
rmse2 <- accuracy(forecast(model2, h=12)$mean, test)[2]

model3 <- Arima(train, order = c(0,1,1), seasonal=list(order=c(0,1,2), period=12))
AIC3 = model3$aicc
rmse3 <- accuracy(forecast(model3, h=12)$mean, test)[2]

model4 <- Arima(train, order = c(0,1,2), seasonal=list(order=c(0,1,2), period=12))
AIC4 = model4$aicc
rmse4 <- accuracy(forecast(model4, h=12)$mean, test)[2]

model5 <- Arima(train, order = c(1,1,1), seasonal=list(order=c(0,1,1), period=12))
AIC5 = model5$aicc
rmse5 <- accuracy(forecast(model5, h=12)$mean, test)[2]

model6 <- Arima(train, order = c(1,1,1), seasonal=list(order=c(1,1,1), period=12))
AIC6 = model6$aicc
rmse6 <- accuracy(forecast(model6, h=12)$mean, test)[2]

model7 <- Arima(train, order = c(1,1,1), seasonal=list(order=c(1,1,2), period=12))
AIC7 = model7$aicc
rmse7 <- accuracy(forecast(model7, h=12)$mean, test)[2]

model8 <- Arima(train, order = c(1,1,1), seasonal=list(order=c(2,1,1), period=12))
AIC8 = model8$aicc
rmse8 <- accuracy(forecast(model8, h=12)$mean, test)[2]

model9 <- Arima(train, order = c(1,1,2), seasonal=list(order=c(0,1,1), period=12))
AIC9 = model9$aicc
rmse9 <- accuracy(forecast(model9, h=12)$mean, test)[2]

model10 <- Arima(train, order = c(1,1,2), seasonal=list(order=c(0,1,2), period=12))
AIC10 = model10$aicc
rmse10 <- accuracy(forecast(model10, h=12)$mean, test)[2]

model11 <- Arima(train, order = c(1,1,2), seasonal=list(order=c(1,1,1), period=12))
AIC11 = model11$aicc
rmse11 <- accuracy(forecast(model11, h=12)$mean, test)[2]

model12 <- Arima(train, order = c(1,1,2), seasonal=list(order=c(1,1,2), period=12))
AIC12 = model12$aicc
rmse12 <- accuracy(forecast(model12, h=12)$mean, test)[2]


name <- c('ARIMA(0,1,1)(0,1,1)[12]', 'ARIMA(0,1,2)(0,1,1)[12]', 'ARIMA(0,1,1)(0,1,2)[12]', 'ARIMA(0,1,2)(0,1,2)[12]', 
          'ARIMA(1,1,1)(0,1,1)[12]', 'ARIMA(1,1,1)(1,1,1)[12]', 'ARIMA(1,1,1)(1,1,2)[12]', 'ARIMA(1,1,1)(2,1,1)[12]',
          'ARIMA(1,1,2)(0,1,1)[12]', 'ARIMA(1,1,2)(0,1,2)[12]', 'ARIMA(1,1,2)(1,1,1)[12]', 'ARIMA(1,1,2)(1,1,2)[12]')

AICc <- c(AIC1, AIC2, AIC3, AIC4, AIC5, AIC6, AIC7, AIC8, AIC9, AIC10, AIC11, AIC12)

RMSE <- c(rmse1, rmse2, rmse3, rmse4, rmse5, rmse6, rmse7, rmse8, rmse9, rmse10, rmse11, rmse12)

modelsdf = data.frame(name, AICc, RMSE)
modelsdf


#best model is model 9 with ARIMA(1,1,2)(0,1,1)[12]

best_model <- forecast(Arima(train, order = c(1,1,2), seasonal=list(order=c(0,1,1), period=12)), h=12)

checkresiduals(best_model)
#residuals are white noise, with p-value 0.7 > 0.05 => accept h0 => residuals are uncorrelated/white noise

autoplot(best_model)
accuracy(best_model, test)

autoplot(accidents) + autolayer(forecast(Arima(accidents, order = c(1,1,2), seasonal=list(order=c(0,1,1), period=12)), h=12), PI=FALSE)
autoplot(accidents) + autolayer(forecast(Arima(train, order = c(1,1,2), seasonal=list(order=c(0,1,1), period=12)), h=12), PI=FALSE)


#ETS

autoplot(ets(train))

ETSana <- ets(train, model='ANA')
AICANA <- ETSana$aicc
RMSEana <- accuracy(forecast(ETSana, h=12),test)[4]

ETSmna <- ets(train, model='MNA')
AICMNA <- ETSmna$aicc
RMSEmna <- accuracy(forecast(ETSmna, h=12),test)[4]

ETSmnm <- ets(train, model='MNM')
AICMNM <- ETSmnm$aicc
RMSEmnm <- accuracy(forecast(ETSmnm, h=12),test)[4]

ETSaaa <- ets(train, model='AAA')
AICAAA <- ETSaaa$aicc
RMSEaaa <- accuracy(forecast(ETSaaa, h=12), test)[4]

name_ets <- c('ETS(A,N,A)', 'ETS(M,N,A)', 'ETS(M,N,M)', 'ETS(A,A,A)')

AIC_ets <- c(AICANA, AICMNA, AICMNM, AICAAA)

RMSE_ets <- c(RMSEana, RMSEmna, RMSEmnm, RMSEaaa)

models_ets = data.frame(name_ets, AIC_ets, RMSE_ets)
models_ets

#model with lowest AIC is ETS(A,N,A)

checkresiduals(ETSana)

best_ets <- forecast(ets(train, model='ANA'), h=12)
autoplot(best_ets)
autoplot(accidents) + autolayer((forecast(ets(accidents, model='ANA'), h=12)), PI=FALSE)

autoplot(accidents) + 
  autolayer(forecast(Arima(accidents, order = c(1,1,2), seasonal=list(order=c(0,1,1), period=12)), h=12), series='ARIMA', PI=FALSE) +
  autolayer(forecast(ets(accidents, model='ANA'), h=12), series = 'ETS', PI=FALSE)

autoplot(accidents) +
  autolayer(test, series='Actual') +
  autolayer(forecast(Arima(train, order = c(1,1,2), seasonal=list(order=c(0,1,1), period=12)), h=12), series='ARIMA', PI=FALSE) +
  autolayer(forecast(ets(train, model='ANA'), h=12), series = 'ETS', PI=FALSE)


#best model is ARIMA(1,1,2)(0,1,1)[12]

forecast(Arima(accidents, order = c(1,1,2), seasonal=list(order=c(0,1,1), period=12)), h=12)

#with and without outliers 
#with outlier forecast
df = read.csv('/Users/T/Desktop/lara/317/CarAccidents.csv')
accidents1 <- ts(df$accidents, frequency=12, start=c(2007,1), end=c(2018,12))
train1 <- window(accidents1, start=c(2007,1), end=c(2016,12))
test1 <- window(accidents1, start=c(2017,1), end=c(2018,12))

autoplot(accidents1) +
  autolayer(test1, series='Actual') +
  autolayer(forecast(Arima(train1, order = c(1,1,2), seasonal=list(order=c(0,1,1), period=12)), h=12), series='ARIMA', PI=FALSE)

#without outlier forecast
df = read.csv('/Users/T/Desktop/lara/317/CarAccidents.csv')
data<-replace(df$accidents, df$accidents==110, 230)
accidents2 <- ts(data, frequency=12, start=c(2007,1), end=c(2018,12))
train2 <- window(accidents2, start=c(2007,1), end=c(2016,12))
test2 <- window(accidents2, start=c(2017,1), end=c(2018,12))
autoplot(accidents2) +
  autolayer(test2, series='Actual') +
  autolayer(forecast(Arima(train2, order = c(1,1,2), seasonal=list(order=c(0,1,1), period=12)), h=12), series='ARIMA', PI=FALSE)
  


