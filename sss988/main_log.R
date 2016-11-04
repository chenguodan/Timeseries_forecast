
sss988<- read.csv("C:/AFT/Trend/SSS988_EndBalanceSunmmary/SSS988_EndBalance.csv")
attach(sss988)
library(zoo)
library(timeSeries)
library(tseries)
myformat <- "%d/%m/%Y"

sss988$ReportDate <- as.Date(sss988$ReportDate, myformat)

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
sss988$MAIN.BALANCE[25]=(sss988$MAIN.BALANCE[26]+sss988$MAIN.BALANCE[24])/2
ma_out<-OUT.3(sss988$MAIN.BALANCE)

LOG<-function(timeseies){
  Log<-log(timeseies)
  plot(Log) 
  a=adf.test(Log)
  print (a)
  diff=diff(Log)
  b=adf.test(diff,alternative = c("stationary"))
  print (b)
  #p-value <0.05,???????????????stationary
  #We can use ARIMA(P,1,q)
  c=Box.test(diff,lag=6,type="Ljung-Box")
  print (c)
  #??????????????????,p?????????5%,?????????????????????
  #p-value <0.05 ???WN
  auto=auto.arima(timeseies,ic="aic",test=c("adf")) 
  summary(auto)
  return(Log)}

log_ma<-LOG(ma_ts)
ord_ma<- c(0,1,1)
MODEL<-function(ts,ord){
  model=arima(ts,order =ord )
  r=model$residuals
  a=Box.test(r,type="Ljung-Box",lag=6)
  print (a) 
  #?????????????????????????????????
  #??????????????????,p>5%,?????????WN 
  return(model)
}

Predict_7<-function(log_data,order_data){
  model<-MODEL(log_data,order_data)
  Fore = predict(model, n.ahead =7)
  log_U = Fore$pred + 1.28 * Fore$se  
  log_L = Fore$pred - 1.28 * Fore$se#??????95%???????????? 
  U=exp(log_U)
  L=exp(log_L)
  Fore=exp(Fore$pred)
  re=data.frame(U,L)
  return(re)
}
Predict_7_value<-function(log_data,order_data){
  model<-MODEL(log_data,order_data)
  Fore = predict(model, n.ahead =7)
  log_U = Fore$pred + 1.64* Fore$se  
  log_L = Fore$pred - 1.64* Fore$se#??????95%???????????? 
  U=exp(log_U)
  L=exp(log_L)
  Fore=exp(Fore$pred)
  return(Fore)
}
pre_ma<-Predict_7(log_ma,ord_ma)
pre_ma1<-Predict_7_value(log_ma,ord_ma)
mod<-arima(log_ma,ord_ma)
MA <- forecast.Arima(mod)
plot.forecast(MA)
Box.test(MA$residuals, lag = 20, type='Ljung-Box')

