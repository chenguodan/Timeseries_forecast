sss988<- read.csv("C:/AFT/Trend/SSS/SSS988_EndBalanceSummary.csv")
names(sss988)
#[1] "ReportDate"              "MAIN.BALANCE"
#[3] "SPORTBOOK.BALANCE"       "SUNCASINO.BALANCE"
# [5] "HOGAME.BALANCE"          "EAGAME.BALANCE"
# [7] "ONELABSPORTBOOK.BALANCE" "CHARTWELL.BALANCE"
# [9] "GOLDDELUXE.BALANCE"      "BBIN.BALANCE"
# [11] "MICROPOKER.BALANCE"      "PT.BALANCE"
# [13] "TELEBET.BALANCE"         "QUICKFIRE.BALANCE"
# [15] "FFSPORTS.BALANCE"        "UGS.BALANCE"
# test<-read.csv("C:/AFT/Trend/SSS988_EndBalanceSunmmary/SSS988_TEST.csv")
#Ctrl+Shift+C

attach(sss988)
library(zoo)
library(timeSeries)
library(tseries)
myformat <- "%d/%m/%Y"

sss988$ReportDate <- as.Date(sss988$ReportDate, myformat)
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


OUT.3two <- function(x){
  n=length(x)-1
  re <- c()
  for (i in 2:n){
    b=abs(x[i]-x[i-1])
    d=b/x[i]
    c=abs(x[i+1]-x[i])
    e=c/x[i]
    if((d>0.3)&(e>0.3)){
      re<-c(re,i)}
  }
  print (re)
  return(re)}

ma_out<-OUT.3(sss988$MAIN.BALANCE)
sp_out<-OUT.3(sss988$SPORTBOOK.BALANCE)
on_out<-OUT.3(sss988$ONELABSPORTBOOK.BALANCE)


ug_out<-OUT.3(sss988$UGS.BALANCE)
ho_out<-OUT.3(sss988$HOGAME.BALANCE)

su_out<-OUT.3(sss988$SUNCASINO.BALANCE)
go_out<-OUT.3(sss988$GOLDDELUXE.BALANCE)
ea_out<-OUT.3(sss988$EAGAME.BALANCE)

mi_out<-OUT.3(sss988$MICROPOKER.BALANCE)
ch_out<-OUT.3(sss988$CHARTWELL.BALANCE)

pt_out<-OUT.3(sss988$PT.BALANCE)
bb_out<-OUT.3(sss988$BBIN.BALANCE)
qu_out<-OUT.3(sss988$QUICKFIRE.BALANCE[36:74])
#two_side_out
ma_out<-OUT.3two(sss988$MAIN.BALANCE)
sp_out<-OUT.3two(sss988$SPORTBOOK.BALANCE)
on_out<-OUT.3two(sss988$ONELABSPORTBOOK.BALANCE)


ug_out<-OUT.3two(sss988$UGS.BALANCE)
ho_out<-OUT.3two(sss988$HOGAME.BALANCE)

su_out<-OUT.3two(sss988$SUNCASINO.BALANCE)
go_out<-OUT.3two(sss988$GOLDDELUXE.BALANCE)
ea_out<-OUT.3two(sss988$EAGAME.BALANCE)

mi_out<-OUT.3two(sss988$MICROPOKER.BALANCE)
ch_out<-OUT.3two(sss988$CHARTWELL.BALANCE)

pt_out<-OUT.3two(sss988$PT.BALANCE)
bb_out<-OUT.3two(sss988$BBIN.BALANCE)
qu_out<-OUT.3two(sss988[36:,16])

main<-sss988$MAIN.BALANCE[1:74]
main_test<-sss988$MAIN.BALANCE[75:79]

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
  summary(auto)
  return(auto)}


main_fit=TEST(main)

plot(forecast(main_fit,h=7),type="o")
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

fit_mean <- meanf(main,h=7)
plot(forecast(fit_mean,7))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

#simple_exponential_moving_average
fit_ses<-ses(main,h=7)
plot(forecast(fit_ses,7))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

fit_ets<-ets(main)
plot(forecast(fit_ets,7))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

fit_rwf<-rwf(main,h=7, drift=T)
plot(forecast(fit_rwf,7))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")
#Eponential smoothing
fit_ewma<-HoltWinters(main,beta=FALSE,gamma = FALSE)
plot(forecast(fit_ewma,7))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")


#double exponential - models level and trend
fit_holt <- HoltWinters(main, gamma=FALSE)
fit_holt_log<-HoltWinters(log(main),gamma = FALSE)
fit_holt_season<-HoltWinters(ts(main,freq=10),seasonal = 'additive')

plot(forecast(fit_holt,7))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

plot(forecast(fit_holt_log))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

plot(forecast(fit_holt_season))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")


#plot fitted arima model with orginal data
fit_stlf<-stlf(ts(main,freq=10), t.window=15, s.window="per", robust=TRUE)
# fit<-Arima(main,ord_ma,method='ML')
# fit_sarima<-arima(ma_ts,order=c(0,1,1),seasonal=list(order=c(1,1,0),period=7))
# Box.test(fit_sarima$residuals, lag = 6, type='Ljung-Box')
# fit_sarima1<-arima(ma_ts,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=7))
# Box.test(fit_sarima1$residuals, lag = 6, type='Ljung-Box')


library(hydroGOF)
rmse(main,predict(main_fit,7)$pred)
rmse(fit_mean$mean,main_test) #653868.5
rmse(fit_naive$mean,main_test) #1173871
rmse(fit_snaive$mean,main_test)
rmse(fit_ses$mean,main_test)#1262724
accuracy(predict(fit_ets,7),main_test)$RMSE#1262883.8
rmse(fit_rwf$mean,main_test)#1286406
rmse(fit_stlf$mean[1:7],main_test)
rmse(fit_hw$mean,main_test)
rmse(main_test,predict(fit_sarima,7)$pred)
#mse_arima<-sqrt(mean((ma_test-predict(fit,7)$pred)^2))
sqrt(mean((main_test-predict(fit_ewma,7))^2))#1262760
sqrt(mean((main_test-predict(fit_holt,7))^2))#1191856
sqrt(mean((main_test-exp(predict(fit_holt_log,7)))^2))#1232853
rmse(fit_holt_season$fitted[1:7],main_test)#1342062
rmse(main_test,predict(fit_sarima,7)$pred)
#check the residuals of HoltWinter Method
holt<-forecast.HoltWinters(fit_holt_log)
Box.test(holt$residuals, lag=6, type="Ljung-Box")

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
# Automated forecasting using an exponen tial model
fit_exp <- ets(ma_ts)
# predict next 7 observations

forecast(fit_exp, 7)
plot(forecast(fit_exp, 7))
