sss988<- read.csv("C:/AFT/Trend/SSS/SSS988_EndBalanceSummary.csv")
names(sss988)

attach(sss988)
library(zoo)
library(timeSeries)
library(tseries)
library(cgd.ts)
library(forecast)
library(e1071)
library(hybridModel)
library(forecastHybrid)
myformat <- "%d/%m/%Y"
sss988$ReportDate <- as.Date(sss988$ReportDate, myformat)



#1:66
# main<-sss988$MAIN.BALANCE[1:102]

main<-sss988$MAIN.BALANCE
outlier = out25(main)
main<-data_split(main,outlier)


ma_ts = ts(rev(sss988$MAIN.BALANCE),start=c(2016,1),frequency=365)
trainData = window(ma_ts, end=c(2016,100))
testData = window(ma_ts, start=c(2016,101))

fit_arima_aic <- auto.arima(trainData,ic="aic",test=c("adf"),stationary=FALSE, seasonal=TRUE, stepwise=TRUE,seasonal.test=c("ocsb","ch"))
plot(forecast(fit_arima_aic, 7))
lines(main, col="green", lty=2,type="o")


fit_arima_aic <- auto.arima(ma$train,ic="aic",test=c("adf"),stationary=FALSE, seasonal=TRUE, stepwise=TRUE,seasonal.test=c("ocsb","ch"))
plot(forecast(fit_arima_aic, 7))
lines(main, col="green", lty=2,type="o")

fit_arima_bic <- auto.arima(ma$train,ic="bic",test=c("adf"))
plot(forecast(fit_arima_bic, 7))
lines(main, col="green", lty=2,type="o")


fit_arima_drift <- auto.arima(main,stepwise = FALSE,approximation = FALSE)
plot(forecast(fit_arima_drift, 5))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")


fit_mean <- meanf(ma$train,h=3)
plot(forecast(fit_mean,3))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

#simple_exponential_moving_average
fit_ses<-ses(ma$train)
plot(forecast(fit_ses,7))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

fit_ets<-ets(main)

fit_rwf<-rwf(main,h=7, drift=T)

#Eponential smoothing
fit_ewma<-HoltWinters(main,beta=FALSE,gamma = FALSE)

#double exponential - models level and trend
fit_holt <- HoltWinters(main, gamma=FALSE)
fit_holt_log<-HoltWinters(log(main),gamma = FALSE)
fit_holt_season<-HoltWinters(ts(main,freq=10),seasonal = 'additive')

forecast(fit_holt, 7)
plot(forecast(fit_holt, 7))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

forecast(fit_holt_log, 7)
plot(forecast(fit_holt, 7))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

# Automated forecasting using an exponential model
fit_exp <- ets(main)
# predict next 7 observations
plot(forecast(fit_exp, 7))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

plot(forecast(fit_holt,7))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

plot(forecast(fit_holt_log,7))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

plot(forecast(fit_holt_season))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")


#SVR for timesries
y=main$train
x=seq(1:length(y))
x_test<-seq(length(y)+1,length(y)+4)
data_train=list(x=x,y=y)
fit_svm<-svm(y~x,data=data_train,kernel =  "radial")
pre_y <- predict(fit_svm,x_test)
plot(sss988$MAIN.BALANCE)
points( pre_y, col = "red", pch=4)


#linear model with time series components

ma_ts = ts(rev(main$train),start=c(2016,1),frequency=365)
ma_train =  window(ma_ts, end=c(2016,110))
ma_test = window(ma_ts,start = c(2016,111))
fit_lm <- tslm(ma_train ~ trend)
plot(forecast(fit_lm,h=length(ma_test)))
points(sss988$MAIN.BALANCE, col="red", type="o")

fit_HoltWinter = HoltWinters(ma_train,gamma=FALSE,seasonal = c("additive", "multiplicative"),start.periods = 1)
forecast_HoltWinter = forecast(fit_HoltWinter, h=length(ma_test))
plot(forecast_HoltWinter)
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")



MAE_arima_list=list()
filename_list=list()
# MAE row vector 
MAE_arima = matrix(NA,1,length(testData))
for(i in 1:length(testData))
{
  MAE_arima[1,i] = abs(arima_forecastData$mean[i] - testData[i])}
MAE_arima_list[k]=sum(MAE_arima[1,1:10])
filename_list[k]=filename
df_arima=data.frame(unlist(filename_list),unlist(MAE_arima_list))
final_arima=df_arima[order(unlist(MAE_arima_list)),]
top10_arima=head(final_arima,n=10)
print("The top 10 stocks with the minimum sum of MAE using ARIMA Model:")
print(top10_arima)



#Neural Network Time Series Forecasts
#"aenst":a (auto.arima), e (ets), n (nnetar), s (stlm) and t (tbats) for combination of this five models
fit_com1 = hybridModel(y = ma$train, models = "aent", weights = "equal")
fit_com2 = hybridModel(y = ma$train, models = "aent", weights ="cv.errors",errorMethod = c("RMSE", "MAE", "MASE"))
fit_com3 = hybridModel(y = ma$train, models = "aent", weights ="cv.errors",errorMethod = "RMSE")
fit_com4 = hybridModel(y = ma$train, models = "aent", weights ="cv.errors",errorMethod = "MAE")
fit_com5 = hybridModel(y = ma$train, models = "aent", weights ="cv.errors",errorMethod = "MASE")


plot(forecast(fit_com1))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

plot(forecast(fit_com2))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

plot(forecast(fit_com3))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

plot(forecast(fit_com4))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")

plot(forecast(fit_com5))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")



fit_nnetar <- nnetar(main$train)
plot(forecast(fit_nnetar,h=3))
lines(sss988$MAIN.BALANCE, col="green", lty=2,type="o")
pre_nnetar<- forecast(fit_nnetar,h=3)



#confidence interval for 
n <- length(y)
m <- frequency(y)

fits <- ts(c(rep(NA,lag),head(y,-lag)) + b, start=tsp(y)[1], frequency=m)
res <- y - fits
fullperiods <- (h-1)/lag+1
#h is forecast period
steps <- rep(1:fullperiods, rep(m,fullperiods))[1:h]
f <- rep(tail(y,lag), fullperiods)[1:h] + steps*b
mse <- mean(res^2, na.rm=TRUE)
se  <- sqrt(mse*steps  + (steps*b.se)^2)





