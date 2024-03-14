rm(list=ls(all=TRUE))
install.packages('readxl')
install.packages('istmr')
install.packages('forecast')
install.packages('tseries')
install.packages('fracdiff')
install.packages('vars')
library(readxl)
library(itsmr)
library(forecast)
library(tseries)
library(fracdiff)
library(vars)
data<-read_excel("ozon.xlsx")
plot(data$seoul)
data<-ts(data$seoul,start=c(2010,1),frequency=12)
plot(data)
acf(data)
pacf(data)
summary(data)
plot(data,main='ozon')
plot(decompose(data))
findfrequency(data)
plot(diff(diff(data,12)))
plot(diff(diff(diff(data,12))))
acf(diff(diff(data,12)),lag.max=50)
pacf(diff(diff(data,12)),lag.max=50)

adf.test(data)
auto.arima(data)
adf.test(diff(diff(data,12)))



aa1<-arima(data, order = c(2,1,0), seasonal=list(order=c(0,1,1), period=12))
aa2<-arima(data, order = c(1,1,0), seasonal=list(order=c(1,1,0), period=12))
aa3<-arima(data, order = c(0,1,0), seasonal=list(order=c(1,1,1), period=12))
aa4<-arima(data, order = c(0,0,0), seasonal=list(order=c(2,1,0), period=12))
aa5<-arima(data, order = c(1,1,1), seasonal=list(order=c(0,1,0), period=12))
aa6<-arima(data, order = c(0,0,0), seasonal=list(order=c(1,1,0), period=12))
2*(1-pnorm(aa4$coef/(sqrt(diag(aa4$var.coef)))))

tsdiag(aa1)
summary(aa1)
test(resid(aa1))
summary(aa2)
test(resid(aa2))
summary(aa3)
test(resid(aa3))
summary(aa4)
test(resid(aa4))
tsdiag(aa4)
summary(aa5)
aa5
test(resid(aa5))
summary(aa6)


forecast(aa6,24)
plot(forecast(aa1))
plot(forecast(aa6,24))

par(mfrow=c(1,1))



no2<-read_excel("no2.xlsx")
no2
plot(no2$no2)
data2<-ts(no2$no2,start=c(2010,1),frequency=12)
plot(data2)
acf(data2)
pacf(data2)
plot(decompose(data2))
findfrequency(data2)
plot(diff(diff(data2,12)))
plot(diff(diff(diff(data2,12))))
acf(diff(diff(data2,12)))
pacf(diff(diff(data2,12)))
adf.test(data2)
auto.arima(data2)
adf.test(diff(diff(data,12)),k=0)
lweplot(data2)

wea<-read_excel("weat.xlsx")
data4<-ts(wea$wea,start=c(2010,1),frequency=12)
plot(data4)
findfrequency(data4)
auto.arima(data4)
cc1<-arima(data4,order=c(1,0,0),seasonal=list(order=c(0,1,1),period=12))
dd11<-resid(dd1)
cc11<-resid(cc1)
acf(cbind(dd11,cc11))

ozo<-read_excel("ozon3.xlsx")
data3<-ts(ozo$seoul,start=c(2010,1),frequency=12)
auto.arima(data3)

bb1<-arima(data2, order = c(1,0,1), seasonal=list(order=c(0,1,1), period=12))
dd1<-arima(data3,order=c(0,0,0),seasonal=list(order=c(2,1,0),period=12))

2*(1-pnorm(aa4$coef/(sqrt(diag(aa4$var.coef)))))

tsdiag(bb1)
summary(bb1)
test(resid(bb1))



plot(forecast(bb1))
plot(forecast(aa4))




aa11<-resid(aa4)
bb11<-resid(bb1)
acf(cbind(aa11,bb11))
ozon<-diff(data)
no2<-diff(data2)
plot(ozon)
plot(no2)
adf.test(ozon,k=0)
adf.test(no2,k=0)

set<-cbind(no2,ozon)
VARselect(set,lag.max=10)
var1<-VAR(set,p=8)
var2<-VAR(set,p=7)
var3<-VAR(set,p=6)
AIvar1
summary(var1)
forecast(var1,24)
causality(var1,cause='no2')
causality(var1,cause='ozon')
summary(var1)
plot(forecast(diffinv(var1$y),24))
plot(forecast(aa1))
forecast(diffinv(var1$y))
Bcoef(var1)


ozonb<-diff(data3)
weat2<-diff(data4)
adf.test(ozonb,k=0)    
adf.test(weat2,k=0)
set2<-cbind(weat2,ozonb)
VARselect(set2,lag.max=10)
var2<-VAR(set2,p=8)
summary(var2)
forecast(diffinv(var2$y))
plot(forecast(diffinv(var2$y)))
Bcoef(var2)


plot(diffinv(ozonb))
plot(diffinv(ozon))


serial.test(var1,type="PT.asymptotic")
serial.test(var2,type="PT.asymptotic")

aa4
