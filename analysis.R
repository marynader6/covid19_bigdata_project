setwd("E:\\4th Year\\Big data\\project\\covid19_bigdata_project")
rm(list = ls())

analysisData <- read.csv("preprocessedfinalData.csv",header = TRUE)

#--------------------- analysing age ------------------------

color <- c("red","green","blue","purple","pink","violet",
            "orange","yellow","grey","brown","white")
ages<-table(analysisData$age)
barplot(ages,xlab="age",ylab = "number")  
pie(ages , col = color)


#------------------------ age with death,recovered ---------------------------------
dAges <- table(analysisData[analysisData$death == 1 , c("age")])
barplot(dAges)
pie(dAges , col = color)

rAges <- table(analysisData[analysisData$recovered == 1 , c("age")])
barplot(rAges)
pie(rAges , col = color)

iAges <- table(analysisData[analysisData$recovered == 0 & analysisData$death == 0 , c("age")])
barplot(iAges)
pie(iAges , col = color)

maxdAgesCat <- table(analysisData[analysisData$age == "60-69" , c("death")]) # people of ages "60-69" die by 2.018%
maxrAgesCat <- table(analysisData[analysisData$age == "60-69" , c("recovered")]) # people of ages "60-69" recover by 6.6%

secdAgesCat <- table(analysisData[analysisData$age == "40-49" , c("death")]) # people of ages "40-49" die by 0.63%
secrAgesCat <- table(analysisData[analysisData$age == "40-49" , c("recovered")]) # people of ages "40-49" recover by 9%

#------------------------age with gender ------------------------------

fAges <- table(analysisData[analysisData$gender == "F" , c("age")])
barplot(fAges)
pie(fAges , col = color)

mAges <- table(analysisData[analysisData$gender == "M" , c("age")])
barplot(mAges)
pie(mAges , col = color)

#-----------------------------gender-----------------------------------
gender<- table(analysisData$gender)
barplot(gender)
pie(gender,col = c("pink","blue"))

recGender <- table(analysisData[analysisData$recovered == 1 , c("gender")])
barplot(recGender,col = c("pink","blue"))
pie(recGender,col = c("pink","blue"))

dGender <- table(analysisData[analysisData$death == 1 , c("gender")])
barplot(dGender,col = c("pink","blue"))
pie(dGender,col = c("pink","blue"))

recM <- table(analysisData[analysisData$gender== "M" , c("recovered")]) # males recover by 7.42%
diedM <- table(analysisData[analysisData$gender== "M" , c("death")]) # males die by 3.83%

recF <- table(analysisData[analysisData$gender== "F" , c("recovered")]) # females recover by 7.45%
diedF <- table(analysisData[analysisData$gender== "F" , c("death")]) # Females die by 1.58%


#-------------------------------analysing dates --------------------------------------
analysisData$diff_ad_onset <- abs(analysisData$date_admission_hospital- analysisData$date_onset_symptoms)
analysisData$diff_con_ad <- abs(analysisData$date_confirmation- analysisData$date_admission_hospital)
analysisData$diff_con_onset <- abs(analysisData$date_confirmation- analysisData$date_onset_symptoms)

#-------------------------------------wuhan--------------------------------------------
barplot(table(analysisData$wuhan))

dWuhan <- table(analysisData[analysisData$wuhan == 1 , c("death")])# people in wuhan die by 6.1% 
barplot(dWuhan)
pie(dWuhan,col = c("blue","red"))

rWuhan <- table(analysisData[analysisData$wuhan == 1 , c("recovered")])# people in wuhan recover by 11% 
barplot(rWuhan)
pie(rWuhan,col = c("blue","red"))

#-------------------------countries------------------------------
c <- read.csv("countries.csv",header = TRUE, sep = ",")
c$country <- as.character(c$country)
c$dPerc <- (c$death/c$confirmed )*100
c$rPerc <- (c$recovered/c$confirmed )*100

plot(as.factor(c$country),c$confirmed)

hCount <- c[c$confirmed >= 100000,]
plot(as.factor(hCount$country),hCount$confirmed,)

fthCount <- c[c$confirmed <= 5000 &c$confirmed >= 2000 ,]
plot(as.factor(fthCount$country),fthCount$confirmed,)

tTHCount <- c[c$confirmed > 5000 & c$confirmed <= 10000,]
plot(as.factor(tTHCount$country),tTHCount$confirmed,)

mtTHCount <- c[c$confirmed > 10000 & c$confirmed < 100000,]
plot(as.factor(mtTHCount$country),mtTHCount$confirmed,)

thdCount <- c[c$death >= 1000,]
plot(as.factor(thdCount$country),thdCount$death,)

hdCount <- c[c$death < 1000 &c$death >= 100 ,]
plot(as.factor(hdCount$country),hdCount$death,)

fdCount <- c[c$death >= 50 & c$death < 100,]
plot(as.factor(fdCount$country),fdCount$death,)

zdCount <- c[c$death < 50,]
plot(as.factor(zdCount$country),zdCount$death,)


thrCount <- c[c$recovered >= 10000,]
plot(as.factor(thrCount$country),thrCount$recovered,)

hrCount <- c[c$recovered < 10000 &c$recovered >= 1000,]
plot(as.factor(hrCount$country),hrCount$recovered,)

frCount <- c[c$recovered >= 500 & c$recovered < 1000,]
plot(as.factor(frCount$country),frCount$recovered,)

zrCount <- c[c$recovered < 500,]
plot(as.factor(zrCount$country),zrCount$recovered,)

