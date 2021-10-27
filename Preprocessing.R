library(readxl)
library(tidyr)
library(car)
library(olsrr)
library(skedastic)
library(psych)
library(lmtest)
library(DataCombine)
library(sandwich)
library(systemfit)
library(ggplot2)
library(tseries)
library(graphics)
library(lmtest)
library(forecast)
library(tseries)
library(sarima)
library(timeDate)

data<- read_excel("Ristorazione (2).xls", sheet=2)

colnames(data)

vec<-c("...1","1", "...9","2","...12", "3", "...14","4","...16","5","...18","6","...20")
#subsetting data for keeping interesting columns
data1<-data[,vec]

#changing names 
colnames(data1) <- data1[1,]
data1 <- data1[-1, ]
colnames(data1)<-c("Date","Vendite_1","Scontrini_1","Vendite_2","Scontrini_2",
                   "Vendite_3","Scontrini_3","Vendite_4","Scontrini_4",
                   "Vendite_5","Scontrini_5","Vendite_6","Scontrini_6")
#from string to datetime

#Months
data1$Date<-gsub("gen","01", data1$Date)
data1$Date<-gsub("feb","02", data1$Date)
data1$Date<-gsub("mar","03", data1$Date)
data1$Date<-gsub("apr","04", data1$Date)
data1$Date<-gsub("mag","05", data1$Date)
data1$Date<-gsub("giu","06", data1$Date)
data1$Date<-gsub("lug","07", data1$Date)
data1$Date<-gsub("ago","08", data1$Date)
data1$Date<-gsub("set","09", data1$Date)
data1$Date<-gsub("ott","10", data1$Date)
data1$Date<-gsub("nov","11", data1$Date)
data1$Date<-gsub("dic","12", data1$Date)

#Days
data1$Date<-gsub("Lu", "Monday", data1$Date)
data1$Date<-gsub("Ma", "Tuesday", data1$Date)
data1$Date<-gsub("Me", "Wednesday", data1$Date)
data1$Date<-gsub("Gi", "Thursday", data1$Date)
data1$Date<-gsub("Ve", "Friday", data1$Date)
data1$Date<-gsub("Sa", "Saturday", data1$Date)
data1$Date<-gsub("Do", "Sunday", data1$Date)
#dividing days names from date
r<-regexpr("^[A-Za-z]+",data1$Date)
data1$week_day<-regmatches(data1$Date, r)
#formatting date
data1$Date<-as.Date(data1$Date, format="%A%d%m%Y")
#rounding vendite to two decimals 
v_num<-c("Vendite_1","Scontrini_1","Vendite_2","Scontrini_2",
         "Vendite_3","Scontrini_3","Vendite_4","Scontrini_4",
         "Vendite_5","Scontrini_5","Vendite_6","Scontrini_6")

data1<- transform(data1, Vendite_1=as.numeric(Vendite_1),
                  Vendite_2=as.numeric(Vendite_2),
                  Vendite_3=as.numeric(Vendite_3),
                  Vendite_4=as.numeric(Vendite_4),
                  Vendite_5=as.numeric(Vendite_5),
                  Vendite_6=as.numeric(Vendite_6),
                  Scontrini_1=as.numeric(Scontrini_1),
                  Scontrini_2=as.numeric(Scontrini_2),
                  Scontrini_3=as.numeric(Scontrini_3),
                  Scontrini_4=as.numeric(Scontrini_4),
                  Scontrini_5=as.numeric(Scontrini_5),
                  Scontrini_6=as.numeric(Scontrini_6))

data1[,v_num]<-round(data1[,v_num],digits=2)

#to check the number of Null
sum(is.na(data1))

#considering that rows with NA values are associated to closed days, we've replaced them with a 0 value
#for indicating a null income
data1[is.na(data1)]=0

write.csv(data1,"datapuliti.csv")



epiphanie<-ITEpiphany(2017:2021)
venticinqueapril<-ITLiberationDay(2017:2021) 
#ITRepublicAnniversary(2017:2021)
ferragosto<-ITAssumptionOfVirginMary(2017:2021)  #ferragosto
tuttiisanti<-ITAllSaints(2017:2021) 
santambro<-ITStAmrose(2017:2021)
ottodicembre<-ITImmaculateConception(2017:2021)
pasqua<-Easter(2017:2021)
pasquetta<-EasterMonday(2017:2021)
vigilia<-ChristmasEve(2017:2021)
natale<-ChristmasDay(2017:2021) 
capodanno<-NewYearsDay(2017:2021)
morti<-AllSouls(2017:2021)

epiphanie[[2]]
for (data in data1["Date",]){
  for(i in 1:length(epiphanie)){
    if (data==epiphanie[[i]]){
      print("yes")
    }
  }
}


#### Analysis

'1.Identificare i trend di singoli bar'

inds <- seq(as.Date("2017-01-01"), as.Date("2021-04-12"), by = "day")

## Create a time series object
set.seed(25)
timeseries1_day <- ts(data1[,c("Vendite_1")],     # random data
           start = c(2017, as.numeric(format(inds[1], "%j"))),
           frequency = 365)
timeseries2_day <- ts(data1[,c("Vendite_2")],     # random data
                      start = c(2017, as.numeric(format(inds[1], "%j"))),
                      frequency = 365)
timeseries3_day <- ts(data1[,c("Vendite_3")],     # random data
                      start = c(2017, as.numeric(format(inds[1], "%j"))),
                      frequency = 365)
timeseries4_day <- ts(data1[,c("Vendite_4")],     # random data
                      start = c(2017, as.numeric(format(inds[1], "%j"))),
                      frequency = 365)
timeseries5_day <- ts(data1[,c("Vendite_5")],     # random data
                      start = c(2017, as.numeric(format(inds[1], "%j"))),
                      frequency = 365)
timeseries6_day <- ts(data1[,c("Vendite_6")],     # random data
                      start = c(2017, as.numeric(format(inds[1], "%j"))),
                      frequency = 365)
par(mfrow=c(1,6))

plot.ts(timeseries1_day)
plot.ts(timeseries2_day)
plot.ts(timeseries3_day)
plot.ts(timeseries4_day)
plot.ts(timeseries5_day)
plot.ts(timeseries6_day)

par(mfrow=c(1,1))

acf(timeseries1_day[1:length(timeseries1_day)])
acf(timeseries2_day[1:length(timeseries2_day)])
acf(timeseries3_day[1:length(timeseries3_day)])
acf(timeseries4_day[1:length(timeseries4_day)])
acf(timeseries5_day[1:length(timeseries5_day)])
acf(timeseries6_day[1:length(timeseries6_day)])

#Il lag è decimale e significa che abbiamo dati giornalieri con una stagionalità
#di 365. Quindi l'asse orizzontale è in termini di anni, quindi un lag di
#0.08 significa 0.08 anni, ovvero 0.08*365=29 giorni
#1:length(timeseries2_day) è stato aggiunto per dare gli interi


#decomposing
ts_1_components<-decompose(timeseries1_day,"multiplicative")
plot(ts_1_components)


#box-jenkins fare diff stagionali per renderla staz in varianza ->per portarla a AR o MA 
#guardare i picchi sui correlog. 
#aggiungere variabili delle festività-con libreria di R
#quando dividiamo in train e validation prendere anche il covid e aggiungere var. covid


acf(timeseries1_day, lag=48)
pacf

#seasonal adjusting
ts_1_adjusted<-timeseries1_day-ts_1_components$seasonal
acf(ts_1_adjusted,
    lag=48
    )

par(mfrow=c(2,1))
plot(ts_1_adjusted)
plot.ts(timeseries1_day)




#You can see that the seasonal variation has been removed from the seasonally adjusted time series. 
#The seasonally adjusted time series now just contains the trend component and an irregular component.

'You can see from the plot that there is roughly constant level 
(the mean stays constant at about 25 inches). The random fluctuations in the time series seem to be roughly constant 
in size over time, so it is probably appropriate to describe the data using an additive model.
Thus, we can make forecasts using simple exponential smoothing.'

#Forecasting -non ARIMA

#without seasonal
ts_1_adjusted_forecast <- HoltWinters(ts_1_adjusted, gamma=FALSE)
ts_1_adjusted_forecast


'The output of HoltWinters() tells us that the estimated value of the alpha parameter is about 0.079. 
This is very close to zero, telling us that the forecasts are based on both recent 
and less recent observations (although somewhat more weight is placed on recent observations).'

plot(ts_1_adjusted_forecast)


'As a measure of the accuracy of the forecasts, we can calculate the sum of squared errors for the in-sample forecast errors, that is,
the forecast errors for the time period covered by our original time series. The sum-of-squared-errors is stored in a named
element of the list variable "ts_1_adjusted_forecast" called "SSE", so we can get its value by typing:'
ts_1_adjusted_forecast$SSE

ts_1_adjusted_forecast2<-forecast(ts_1_adjusted_forecast,h=90)
plot(ts_1_adjusted_forecast2)

'Here the forecasts are plotted as a blue line, the 80% prediction interval as grey shaded area,
and the 95% prediction interval as a light blue shaded area.'

acf(na.omit(ts_1_adjusted_forecast2$residuals), lag.max=30)

#test non-zero autocorr
Box.test(na.omit(ts_1_adjusted_forecast2$residuals), lag=30, type="Ljung-Box")
#we can state that there is correlation

plot.ts(ts_1_adjusted_forecast2$residuals)

'To check whether the forecast errors are normally distributed with mean zero, 
we can plot a histogram of the forecast errors, with an overlaid normal curve that has mean zero and 
the same standard deviation as the distribution of forecast errors. 
To do this, we can define an R function "plotForecastErrors()"'

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

#Normality
plotForecastErrors(na.omit(ts_1_adjusted_forecast2$residuals))


####ARIMA

#stationarity
par(mfrow=c(1,1))
plot.ts(timeseries1_day)

#p value 0.05 - si rifiuta l'hp non stazionareità->la serie è stazionaria 
adf.test(timeseries1_day)

#correlation
acf(timeseries1_day, lag.max=30)             # plot a correlogram
acf(timeseries1_day, lag.max=30, plot=FALSE)

pacf(timeseries1_day, lag.max=30)             # plot a partial correlogram
pacf(timeseries1_day, lag.max=30, plot=FALSE)

## use auto.arima to choose ARIMA terms
fit<-auto.arima(timeseries1_day)
## forecast for next 60 time points
fore <- forecast(fit, h = 90)
## plot it
plot(fore)



##Model Estimation 
inds <- seq(as.Date("2017-01-01"), as.Date("2021-04-12"), by = "day")

## Create a time series object
set.seed(25)
timeseries1_weekly <- ts(data1[,c("Vendite_1")],     # random data
                      start = c(2017, as.numeric(format(inds[1], "%j"))),
                      frequency = 52)
plot(timeseries1_weekly)

ts_1_adjusted<-timeseries1_day-ts_1_components$seasonal

len<-floor(2*nrow(data1)/3)

timeseries1_daytrain<-timeseries1_day[1:len]

stima<-arima(timeseries1_daytrain,order=c(5,1,7),include.mean = TRUE)
stima

lag.plot(timeseries1_daytrain,lag = 9)

#AIC(stima)
#BIC(stima)

##Validation
par(mfrow=c(2,2))
#Correlation
acf(stima$residuals)
pacf(stima$residuals)
hist(stima$residuals)
#Normality
qqnorm(stima$residuals)
qqline(stima$residuals)


shapiro.test(stima$residuals)


##Prediction
par(mfrow=c(1,1))
tstest1<-as.ts(timeseries1_day[ceiling(len):nrow(data1)])
frc<-predict(stima,n.ahead = length(tstest1))

plot((length(timeseries1_daytrain)-1000):(length(timeseries1_daytrain)+1000),
     timeseries1_day[(length(timeseries1_daytrain)-1000):(length(timeseries1_daytrain)+1000)],type = "o", col="red",
     xlab="Time",ylab = "Vendite",main="Forecasting")
points((length(timeseries1_daytrain)+1):(length(timeseries1_daytrain)+1000),frc$pred[1:1000],type = "o", col="yellow")
abline(v=length(timeseries1_daytrain),lty="dashed")
#il modello fa cagare

'
2.Confrontare le singoli aziende tra di loro

3.Analizzare la stagionalità per identificare se è lago, citta' o montagna...

4.L azienda soffre la stagione? è influenzata da essa?
  
5.Verificare se il rapporto scontrini vendite è aumentato o diminuito dopo il covid.'
'


#Initialising time series

