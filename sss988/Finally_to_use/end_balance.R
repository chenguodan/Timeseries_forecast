sss988<- read.csv("C:/AFT/Trend/SSS/SSS988_EndBalanceSummary.csv")
attach(sss988)
library(zoo)
library(timeSeries)
library(tseries)
library(forecast)
myformat <- "%d/%m/%Y"
sss988$ReportDate <- as.Date(sss988$ReportDate, myformat)
n=length(sss988$ReportDate)

train_test<- function(x){
  results <- list()
  results$train <- x[1:(n-3)]
  results$test<-x[(n-2):n]
  return(results)}

main=train_test(sss988$MAIN.BALANCE)

fit<- function(main){
  fit_arima_aic <- auto.arima(main$train,ic="aic",test=c("adf"))
  fit_arima_bic <- auto.arima(main$train,ic="bic",test=c("adf"))
  fit_mean <- meanf(main$train,h=7)
  #simple_exponential_moving_average
  fit_ses <-ses(main$train)
  fit_ets <-ets(main$train)
  fit_rwf <-rwf(main$train,h=7, drift=T)
  #Eponential smoothing
  fit_ewma <-HoltWinters(main$train,beta=FALSE,gamma = FALSE)
  #double exponential - models level and trend
  fit_holt <- HoltWinters(main$train, gamma=FALSE)
  fit_holt_log<-HoltWinters(log(main$train),gamma = FALSE)
  fit_holt_season<-HoltWinters(ts(main$train,freq=10),seasonal = 'additive')
  fit_exp <- ets(main$train)
  
  f1=forecast(fit_arima_aic, 3)
  f2=forecast(fit_arima_bic, 3)
  f3=forecast(fit_mean,3)
  f4=forecast(fit_ses,3)
  f5=forecast(fit_holt, 3)
  f6=forecast(fit_holt, 3)
  f7=forecast(fit_holt_log, 3)
  f8=forecast(fit_holt, 3)
  f9=forecast(fit_exp, 3)
  f10=forecast(fit_holt,3)
  f11=forecast(fit_holt_log,3)
  f12=forecast(fit_holt_season,3)
  
  d1<- list()
  d2<- list()
  d3<- list()
  d4<- list()
  d5<- list()
  d6<- list()
  d7<- list()
  d8<- list()
  d9<- list()
  d10<- list()
  d11<- list()
  d12<- list()
  
  d1$u=f1$upper[,2]
  d2$u=f2$upper[,2]
  d3$u=f3$upper[,2][1:3]
  d4$u=f4$upper[,2]
  d5$u=f5$upper[,2]
  d6$u=f6$upper[,2]
  d7$u=f7$upper[,2]
  d8$u=f8$upper[,2]
  d9$u=f9$upper[,2]
  d10$u=f10$upper[,2]
  d11$u=f11$upper[,2]
  d12$u=f12$upper[,2]
  
  d1$l=f1$lower[,2]
  d2$l=f2$lower[,2]
  d3$l=f3$lower[,2][1:3]
  d4$l=f4$lower[,2]
  d5$l=f5$lower[,2]
  d6$l=f6$lower[,2]
  d7$l=f7$lower[,2]
  d8$l=f8$lower[,2]
  d9$l=f9$lower[,2]
  d10$l=f10$lower[,2]
  d11$l=f11$lower[,2]
  d12$l=f12$lower[,2]
  re1=min(d-main$test,main$test-l1)
  return(results)}



# predict next 7 future values


