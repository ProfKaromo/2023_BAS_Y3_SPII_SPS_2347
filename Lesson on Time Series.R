#REFERENCES
#For downloading data use the link
#https://finance.yahoo.com/?guccounter=1

setwd("E:\\TF WORK\\CLASS WORKLOAD\\SEM 1\\SEP 2022\\SPII")
library(readxl)
library(quantmod)
library(xts)#xts-extensile time series

AMZN <- read.csv("AMZN.csv")
head(AMZN)
##simulate AR models
ts_AR=arima.sim(n=100,list(ar=-0.8),mean=18)
ts1_AR=arima.sim(n=100,list(ar=c(1.3,-0.7),mean=8)) 
#1.3+-0.7=<1 so that mean is defined (for Stationarity)
par(mfrow=c(1,2))
plot(ts_AR)
plot(ts1_AR)
acf(ts_AR)
pacf(ts_AR) #oder one only one is out of confidence interval
acf(ts1_AR)
pacf(ts1_AR) #lag 2 is significant

## Simulate MA models
ts_MA=arima.sim(n=100,list(ma=-0.8,mean=18))
ts1_MA=arima.sim(n=100,list(ma=c(0.3,0.7),mean=8))
par(mfrow=c(1,3))
plot(ts_MA)
acf(ts_MA); pacf(ts_MA)
plot(ts1_MA)
acf(ts1_MA); pacf(ts1_MA)
ord<-ar(ts_AR)
ord$aic
ord$order#gives order 1 as simulated
ord1<-ar(ts1_AR)
ord1$aic
ord1$order#gives order 2 as simulated 


data.AMZN<-read.csv("AMZN.csv",header = TRUE)
head(data.AMZN)
tail(data.AMZN)
data.AMZN$Date
class(data.AMZN$Date)
#convert date from factor to date variable
Date<-as.Date(data.AMZN$Date,format="%Y-%m-%d")
head(Date)
tail(Date)
#check the class now
class(Date)
#insert the correct data variable
data.AMZN<-cbind(Date,data.AMZN[,-1])
head(data.AMZN)
tail(data.AMZN)

#index by date
data.AMZN<-xts(data.AMZN[,2:7],order.by = data.AMZN[,1])
head(data.AMZN)
class(data.AMZN)
names(data.AMZN)
names(data.AMZN)<-paste(c("AMZN.Open","AMZN.High","AMZN.Low","AMZN.Close","AMZN.Adj.Close","AMZN."))
head(data.AMZN)
#alternative download
plot(data.AMZN$Close)
dim(data.AMZN)
#checking dimensions
summary(data.AMZN)

#Example
data.AMZN<-read.csv("AMZN.csv")
data.AMZN

# Check the class of date
class(data.AMZN$Date)
# convert date from factor to date variable
Date<-as.Date(data.AMZN$Date,format="%Y-%m-%d")
head(Date)
tail(Date)
# check the class now
class(Date)
# insert the correct date variable
data.AMZN<-cbind(Date, data.AMZN[,-1])
head(data.AMZN)
# check class of dataset
class(data.AMZN)
# change from data frame to xts
library(xts)
data.AMZN<-xts(data.AMZN[,2:7],order.by=data.AMZN[,1])
head(data.AMZN)
#getting the returns from data.AMZN data
library(fBasics)
Close<-data.AMZN[,4]
head(Close)
plot(Close,xlab="Time (in Months)",ylab="Closing Stock Prices",main="The Graph of data.AMZN Time Series")

library(tseries)
adf.test(data.AMZN$Close)
#test for staionarity 
data.AMZN.ret<-diff(data.AMZN$Close)
adf.test(na.omit(data.AMZN.ret))
#if p-value is > 0.05 the you cannot reject the null hypothesis of a unit root.
#if pvalue is > 0.05 the the stock is not stationary (H0) if < 0.05 the stock is tationary (Ha)

data.AMZN.r_1<-diff(Close)
data.AMZN.r_2<-diff(log(Close))
par(mfrow=c(1,2))
plot(data.AMZN.r_1,col="blue",main="Plot of rate of return r in the direct way.")
plot(data.AMZN.r_2,col="red",main="Plot of rate of return r in the logarithmic way.")

diff= data.AMZN.r_1-data.AMZN.r_2
plot(abs(diff),main="Difference of the two graphs.")

#test for staionarity of the returns.
adf.test(na.omit(data.AMZN.r_1))
adf.test(na.omit(data.AMZN.r_2))

#Test for independence of residuals for the returns
Box.test(data.AMZN.r_1,type = "Ljung-Box")
#for Ljung-Box: If the p value is greater than 0.05 then the residuals are independent which we 
#want for the model to be correct

#normalTest(data.AMZN.r_1, method='jb')

acf(na.omit(data.AMZN.r_1),lag.max = 20)
pacf(na.omit(data.AMZN.r_2),lag.max = 20)

#ARIMA(pdq)
m2=arima(dax.r_2,order = c(7,0,7))
m2
coeftest(m2)

#candleChart for the data
candleChart(data.AMZN)
#Example in practice
#https://app.expertoption.com/


