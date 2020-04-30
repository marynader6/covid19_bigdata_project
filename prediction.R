
require(forecast)
require(TSPred)

rm(list = ls())
setwd("E:\\4th Year\\Big data\\project\\covid19_bigdata_project")
confirmed <- read.table("confirmed.csv", header=TRUE, sep=",")
confirmed2 <- read.table("confirmed2.csv",header=TRUE, sep=",")
#confirmed <- confirmed[24:93,]
#diff <- diff(confirmed$conf)
#confirmed$diff <- diff
#data <- ts(confirmed[,2], start = c(2020,1), frequency = 365)
confirmed$dates <- as.Date(confirmed$dates,"%Y-%m-%d")
#as.numeric(format(dates[1],"%j"))
dates <- seq.Date(as.Date("2020-1-22"),as.Date("2020-4-13"),"day")
confTS <- ts(confirmed[,2],start = c(7), frequency = 7)
confTS2 <- ts(confirmed2[,2],start = c(7), frequency = 7)
plot(confTS)
plot(confirmed$conf~as.Date(confirmed$dates,"%Y-%m-%d"))
plot(stl(confTS,"periodic"))
plot(diff(confTS))
plot(log10(confTS))

ARIMAfit <- auto.arima((confTS2), approximation=FALSE, trace=FALSE)
vcov(ARIMAfit$coef)
summary(ARIMAfit)
plot(ARIMAfit$residuals)

pred <- predict(ARIMAfit, n.ahead = 10)
plot(confTS2,type="l",xlim=c(4,20), ylim=c(1,5000),xlab = "week",ylab = "confirmed")
lines((pred$pred),col="blue")
lines((pred$pred+2*pred$se),col="orange")
lines((pred$pred-2*pred$se),col="orange")
plotarimapred((confTS), ARIMAfit, xlim=c(3,19), range.percent = 0.2)
  