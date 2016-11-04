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
library(cgd.ts)
library(forecast)
myformat <- "%d/%m/%Y"

sss988$ReportDate <- as.Date(sss988$ReportDate, myformat)
#test$ReportDate <- as.Date(test$ReportDate, myformat)

sport<-sss988$SPORTBOOK.BALANCE
outlier = out25(sport)
sport<-data_split(sport,outlier)


fit_arima_aic <- auto.arima(sport$train,ic="aic",test=c("adf"))
plot(forecast(fit_arima_aic, 7))
lines(sss988$SPORTBOOK.BALANCE, col="green", lty=2,type="o")

fit_arima_bic <- auto.arima(sport$train,ic="bic",test=c("adf"))
plot(forecast(fit_arima_bic, 7))
lines(sss988$SPORTBOOK.BALANCE, col="green", lty=2,type="o")

fit_mean <- meanf(sport$train,h=7)
plot(forecast(fit_mean,7))
lines(sss988$SPORTBOOK.BALANCE, col="green", lty=2,type="o")

fit_ses<-ses(sport$train)
plot(forecast(fit_ses,7))
lines(sss988$SPORTBOOK.BALANCE, col="green", lty=2,type="o")

fit_ets<-ets(sport$train)
plot(forecast(fit_ets,7))
lines(sss988$SPORTBOOK.BALANCE, col="green", lty=2,type="o")

fit_rwf<-rwf(sport$train,h=7, drift=T)
plot(forecast(fit_rwf,7))
lines(sss988$SPORTBOOK.BALANCE, col="green", lty=2,type="o")

#Eponential smoothing
fit_ewma<-HoltWinters(sport$train,beta=FALSE,gamma = FALSE)
plot(forecast(fit_ewma,7))
lines(sss988$SPORTBOOK.BALANCE, col="green", lty=2,type="o")

#double exponential - models level and trend
fit_holt <- HoltWinters(sport$train, gamma=FALSE)
fit_holt_log<-HoltWinters(log(sport$train),gamma = FALSE)
fit_holt_season<-HoltWinters(ts(sport$train,freq=10),seasonal = 'additive')

plot(forecast(fit_holt, 7))
lines(sss988$SPORTBOOK.BALANCE, col="green", lty=2,type="o")

forecast(fit_holt_log, 7)
plot(forecast(fit_holt, 7))
lines(sss988$SPORTBOOK.BALANCE, col="green", lty=2,type="o")

# Automated forecasting using an exponential model





