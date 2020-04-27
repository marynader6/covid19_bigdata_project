setwd("E:\\4th Year\\Big data\\project")
rm(list = ls())

analysisData <- read.csv("finalData.csv",header = TRUE)
analysisData <- analysisData[!(duplicated(analysisData)),]
analysisData$age[analysisData$age == "N/A" | analysisData$age == ""] <- NA
analysisData$age <- as.numeric(analysisData$age)
myAge <- analysisData$age[ !(is.na(analysisData$age))] 

ageMean <- as.integer(mean(myAge))
analysisData$age[ (is.na(analysisData$age))] <- ageMean
analysisData$age <- cut(analysisData$age, c(0, 10,20,30,40,50,60,70,80,90,100,Inf), 
                labels=c('0-9', '10-19', '20-29', '30-39', '40-49',
                         '50-59','60-69','70-79','80-89','90-99','>=100'))

color <- c("red","green","blue","purple","pink","violet",
            "orange","yellow","grey","brown","white")
ages<-table(analysisData$age)
barplot(ages,xlab="age",ylab = "number")  
pie(ages , col = color)


analysisData$death[(analysisData$death != "0")] <- 1
analysisData$recovered[(analysisData$recovered != "0")] <- 1

dAges <- table(analysisData[analysisData$death == 1 , c("age")])
barplot(dAges)
pie(dAges , col = color)

rAges <- table(analysisData[analysisData$recovered == 1 , c("age")])
barplot(rAges)
pie(rAges , col = color)

iAges <- table(analysisData[analysisData$recovered == 0 & analysisData$death == 0 , c("age")])
barplot(iAges)
pie(iAges , col = color)
analysisData$gender <- as.character(analysisData$gender)
analysisData$gender[analysisData$gender == "N/A" | analysisData$gender == ""] <- NA

myG <- analysisData$gender[ !(is.na(analysisData$gender))]
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode <- (getmode(myG))
analysisData$gender[ (is.na(analysisData$gender))] <- "M"

fAges <- table(analysisData[analysisData$gender == "F" , c("age")])
barplot(fAges)
pie(fAges , col = color)

mAges <- table(analysisData[analysisData$gender == "M" , c("age")])
barplot(mAges)
pie(mAges , col = color)

gender<- table(analysisData$gender)
barplot(gender)
pie(gender,col = c("pink","blue"))

recGender <- table(analysisData[analysisData$recovered == 1 , c("gender")])
barplot(recGender,col = c("pink","blue"))
pie(recGender,col = c("pink","blue"))

dGender <- table(analysisData[analysisData$death == 1 , c("gender")])
barplot(dGender,col = c("pink","blue"))
pie(dGender,col = c("pink","blue"))


