sss988<- read.csv("C:/AFT/Trend/SSS988_EndBalanceSunmmary/SSS988_EndBalance.csv")

test<-read.csv("C:/AFT/Trend/SSS988_EndBalanceSunmmary/SSS988_TEST.csv")

pairs(sss988[2:14])
#myformat <- "%d/%m/%Y"

#sss988$ReportDate <- as.Date(sss988$ReportDate, myformat)
#test$ReportDate <- as.Date(test$ReportDate, myformat)

OUT.3 <- function(x){ 
  n=length(x)-1
  re <- c()
  for (i in 2:n){
    b=abs(x[i]-x[i-1])
    d=b/x[i]
    if(d>0.3){
      re<-c(re,i)}
  }
  print (re)
  return(re)}


ma_out<-OUT.3(sss988$MAIN.BALANCE)

TS<-function(sbalance){ 
  out<-c(1)
  sbalance<-sbalance[-out]
  return(sbalance) }
ma_ts<-TS(sss988$MAIN.BALANCE)
ma_test<-test$MAIN.BALANCE
ma_test<-ma_test[1:7]

library(lubridate)
#ma_ts <- ts(ma_ts, start=c(2016,4,2), frequency = 365)
ma_ts<-zoo(ma_ts, seq(from = as.Date("2016-04-02"), to = as.Date("2016-07-12"), by = 1))
#ma_test<-ts(ma_test, start=c(2016,7,13), frequency = 365)
ma_test<-zoo(ma_test, seq(from = as.Date("2016-07-13"), to = as.Date("2016-07-19"), by = 1))


library(forecast)
#log_timeseires is White Noise
TEST<-function(timeseies){
  plot(timeseies) 
  a=adf.test(timeseies)
  print (a)
  diff=diff(timeseies)
  b=adf.test(diff,alternative = c("stationary"))
  plot(diff)
  pacf(diff)
  acf(diff)
  print (b)
  #p-value <0.05,???????????????stationary
  #We can use ARIMA(P,1,q)
  c=Box.test(diff,lag=6,type="Ljung-Box")
  print (c)
  #??????????????????,p?????????5%,?????????????????????
  #p-value <0.05 ???WN
  auto=auto.arima(timeseies,ic="aic",test=c("adf")) 
  summary(auto)}


TEST(ma_ts)

ord_ma<- c(1,1,1)
#ARIMA model
MODEL<-function(ts,ord){
  model=arima(ts,order =ord )
  r=model$residuals
  a=Box.test(r,type="Ljung-Box",lag=6)
  print (a) 
  #?????????????????????????????????
  #??????????????????,p>5%,?????????WN 
  return(model)
}

Predict_7 <- function(data_ts,order_data){
  model<-MODEL(data_ts,order_data)
  Fore = predict(model, n.ahead =7)
  U = Fore$pred + 1.96 * Fore$se  
  L = Fore$pred - 1.96 * Fore$se#??????95%???????????? 
  re=data.frame(U,L)
  #plot(data_ts,Fore$pred,U,L,col=c(1,2,4,4),lty=c(1,1,2,2))
  #legend("topleft",c("Actual","Forecast","Bound(95% confidence interval"),col=c(1,2,4),lty=c(1,1,2))
  return(re)
}
Predict_7_value<-function(data,order){
  model<-MODEL(data,order)
  Fore = predict(model, n.ahead =7)
  return(Fore)
}

pre_ma <- Predict_7(ma_ts,ord_ma)
pre_ma1 <- Predict_7_value(ma_ts,ord_ma)
mod <- arima(ma_ts,ord_ma,method="ML")
MA <- forecast.Arima(mod)
plot.forecast(MA)
Box.test(MA$residuals, lag = 6, type='Ljung-Box')
Box.test(MA$residuals^2, lag = 6, type='Ljung-Box')
#raw data and fitted vaule

fit <- arima(ma_ts,ord_ma)
tsdiag(fit)


# predictive accuracy
library(forecast)
accuracy(fit)

# predict next 7 observations
library(forecast)
forecast(fit, 7)
plot(forecast(fit, 7))

#double exponential - models level and trend
fit_ewma<-HoltWinters(ma_ts,beta=FALSE,gamma = FALSE)
fit_holt <- HoltWinters(ma_ts, gamma=FALSE)
fit_holt_log<-HoltWinters(log(ma_ts),gamma = FALSE)



#plot fitted arima model with orginal data

fit<-Arima(ma_ts,ord_ma,method='ML')
plot(fit$x,predict(fit,7),type="o", xlab="Date",col='black')
lines(fitted(fit), col="red", lty=2)
legend("topleft",lty=1, pch=1, col=1:2, c("data","ARIMA"))

#plot holt_winter fitted and orginal data with 7 predicted values 
plot(fit_holt,predict(fit_holt,7))
legend("topleft",lty=1, pch=1, col=1:2, c("data","HOLT_WINTER"))


# predict next 7 future values
library(forecast)
forecast(fit_holt, 7)
plot(forecast(fit_holt, 7))

forecast(fit_holt_log, 7)
plot(forecast(fit_holt, 7))
# Automated forecasting using an exponential model
fit_exp <- ets(ma_ts)
# predict next 7 observations

forecast(fit_exp, 7)
plot(forecast(fit_exp, 7))




mse_arima<-sqrt(mean((ma_test-predict(fit,7)$pred)^2))
mse_ewma<-sqrt(mean((ma_test-predict(fit_ewma,7))^2))
mse_holt<-sqrt(mean((ma_test-predict(fit_holt,7))^2))
mse_holt_log<-sqrt(mean((ma_test-exp(predict(fit_holt_log,7)))^2))
#check the residuals of HoltWinter Method
holt<-forecast.HoltWinters(fit_holt_log)
Box.test(holt$residuals, lag=6, type="Ljung-Box")

