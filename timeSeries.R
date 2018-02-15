getSymbols('TECHM.NS', from='2012-01-01', to='2015-01-01')
stock_prices=TECHM.NS[,4]
stock=diff(log(stock_prices),lag=1)
stock=stock[!is.na(stock)]
plot(stock,type="l",main="log returns plot")
print(adf.test(stock))
breakpoint=floor(nrow(stock)*2.9/3)
par(mfrow=c(1,1))
acf.stock=acf(stock[c(1:breakpoint),], main="ACF plot",lag.max = 100)
pacf.stock=pacf(stock[c(1:breakpoint),], main="PACF plot",lag.max = 100)
Actual_series=xts(0,as.Date("2014-11-25","%Y-%m-%d"))
forecasted_series=data.frame(Forecasted=numeric())

for(b in breakpoint:(nrow(stock)-1)){
  
  stock_train=stock[1:b,]
  stock_test=stock[(b+1):nrow(stock),]
  
  
  fit=arima(stock_train,order=c(2,0,2),include.mean = FALSE)
  #summary(fit)
  
  arima.forecast=forecast(fit,h=1,level=99)
  #summary(arima.forecast)
  
  par(mfrow=c(1,1))
  #plot(arima.forecast, main = "ARIMA Forecast")
  
  forecasted_series=rbind(forecasted_series,arima.forecast$mean[1])
  colnames(forecasted_series)=c("Forecasted")
  
  Actual_return=stock[(b+1),]
  Actual_series=c(Actual_series,xts(Actual_return))
  rm(Actual_return)
  
  #print(stock_prices[b+1,])
  #print(stock_prices[b+2,])
  
}

Actual_series = Actual_series[-1]
#xts(forecasted_series,index(Actual_series))

plot(Actual_series,type='l',main='Actual Returns Vs Forecasted Returns')
lines(forecasted_series,lwd=1.5,col='red')
legend('bottomright',c("Actual","Forecasted"),lty=c(1,1),lwd=c(1.5,1.5),col=c('black','red'))

# Create a table for the accuracy of the forecast

comparsion = merge(Actual_series,forecasted_series)
comparsion$Accuracy = sign(comparsion$Actual_series)==sign(comparsion$Forecasted)
print(comparsion)

# Compute the accuracy percentage metric
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)
