require(forecast)
require(TSPred)
rm(list = ls())
setwd("E:\\4th Year\\Big data\\project\\covid19_bigdata_project")

confirmed <- read.table("confirmed.csv", header=TRUE, sep=",")
confirmed$dates <- as.Date(confirmed$dates,"%Y-%m-%d")
dates <- seq.Date(as.Date("2020-1-22"),as.Date("2020-4-21"),"day")
confTS <- ts(confirmed[1:91,2],start = c(7), frequency = 7)
confTS2 <- ts(confirmed2[,2],start = c(7), frequency = 7)
plot(confTS)
plot(confirmed$conf~as.Date(confirmed$dates,"%Y-%m-%d"))
plot(stl(confTS,"periodic"))


confModel <- auto.arima(confTS, approximation=FALSE, trace=FALSE)

confPred <- predict(confModel, n.ahead = 7)
plot(confTS2,type="l",xlim=c(4,25), ylim=c(1,6000),xlab = "week",ylab = "confirmed")
lines((confPred$pred),col="blue")
lines((confPred$pred+2*confPred$se),col="orange")
lines((confPred$pred-2*confPred$se),col="orange")
plotarimapred((confTS), confModel, xlim=c(4,25), range.percent = 0.2)

accuracy(confModel)
accuracy(confPred$pred,confirmed$conf[92:98],simplify = FALSE)
#-------------------------deaths-------------------------
death <- read.table("deaths.csv", header=TRUE, sep=",")
death$dates <- as.Date(death$dates,"%Y-%m-%d")
diedTS <- ts(death[1:91,2],start = c(7), frequency = 7)
diedTS2 <- ts(death[,2],start = c(7), frequency = 7)
plot(diedTS)
plot(confirmed$conf~as.Date(confirmed$dates,"%Y-%m-%d"))
plot(stl(diedTS,"periodic"))


deathModel <- auto.arima(diedTS, approximation=FALSE, trace=FALSE)

deathPred <- predict(deathModel, n.ahead = 7)
plot(diedTS2,type="l",xlim=c(4,25), ylim=c(1,1000),xlab = "week",ylab = "deaths")
lines((deathPred$pred),col="blue")
lines((deathPred$pred+2*deathPred$se),col="orange")
lines((deathPred$pred-2*deathPred$se),col="orange")
plotarimapred((diedTS), deathModel, xlim=c(4,25), range.percent = 0.2)

accuracy(deathModel)
accuracy(deathPred$pred,death$dead[92:98],simplify = FALSE)
#-----------------------------recovered--------------------------------
recovered <- read.table("recovered.csv", header=TRUE, sep=",")
recovered$dates <- as.Date(recovered$dates,"%Y-%m-%d")
recTS <- ts(recovered[1:91,2],start = c(7), frequency = 7)
recTS2 <- ts(recovered[,2],start = c(7), frequency = 7)
plot(recTS)
plot(recovered$rec~as.Date(recovered$dates,"%Y-%m-%d"))
plot(stl(recTS,"periodic"))


recModel <- auto.arima(recTS, approximation=FALSE, trace=FALSE)

recPred <- predict(recModel, n.ahead = 7)
plot(recTS2,type="l",xlim=c(4,25), ylim=c(1,2000),xlab = "week",ylab = "recovered")
lines((recPred$pred),col="blue")
lines((recPred$pred+2*recPred$se),col="orange")
lines((recPred$pred-2*recPred$se),col="orange")
plotarimapred((recTS), recModel, xlim=c(4,25), range.percent = 0.2)

accuracy(recModel)
accuracy(recPred$pred,recovered$rec[92:98],simplify = FALSE)


