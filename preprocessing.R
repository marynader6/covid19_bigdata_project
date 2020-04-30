setwd("E:\\4th Year\\Big data\\project\\covid19_bigdata_project")
rm(list = ls())
library("lubridate")

#-----------------------confirmed timeseries preprocessing------------
confDF <- read.csv("time_series_covid_19_confirmed.csv",header=TRUE,sep = ",")
Drops = c("Province.State","Country.Region","Lat","Long")
confDF <- confDF[,!(names(confDF) %in% Drops)]
conf <- confDF[99,]
conf <- unlist(c(conf),use.names = FALSE)
conf <- as.numeric(as.vector(conf))
dates <- seq.Date(as.Date("2020-1-22"),as.Date("2020-4-28"),"day")
#confTS <- ts(conf,start = c(2020,as.numeric(format(dates[1],"%j"))), frequency = 365)
cDF <- data.frame(dates,conf)
write.csv(cDF,file = "confirmed.csv",row.names = FALSE)

#-----------------------death timeseries preprocessing------------
deadDF <- read.csv("time_series_covid_19_deaths.csv",header=TRUE,sep = ",")
Drops = c("Province.State","Country.Region","Lat","Long")
deadDF <- deadDF[,!(names(deadDF) %in% Drops)]
dead <- deadDF[99,]
dead <- unlist(c(dead),use.names = FALSE)
dead <- as.numeric(as.vector(dead))
dDF <- data.frame(dates,dead)
write.csv(dDF,file = "deaths.csv",row.names = FALSE)

#-----------------------recovered timeseries preprocessing------------
recDF <- read.csv("time_series_covid_19_recovered.csv",header=TRUE,sep = ",")
Drops = c("Province.State","Country.Region","Lat","Long")
recDF <- recDF[,!(names(recDF)%in% Drops)]
rec <- recDF[91,]
rec <- unlist(c(rec),use.names = FALSE)
rec <- as.numeric(as.vector(rec))
rDF <- data.frame(dates,rec)
write.csv(rDF,file = "recovered.csv",row.names = FALSE)

#------------------------open preprocessing---------------------
openDF <- read.csv("COVID19_open_line_list.csv",header = TRUE,sep = ",")
openKeeps <- c("age","sex","date_onset_symptoms","date_admission_hospital","date_confirmation","lives_in_Wuhan")
openDF <- openDF[,(names(openDF) %in% openKeeps)]
names(openDF)[2] <- "gender"
names(openDF)[6] <- "wuhan"
openDF$wuhan <- as.character(openDF$wuhan)
openDF$wuhan[openDF$wuhan == "no" | openDF$wuhan == ""| is.na(openDF$wuhan)|openDF$wuhan == "No" |openDF$wuhan == "N/A"] <- 0
openDF$wuhan[openDF$wuhan != "0" ] <- 1
openDF$recovered <- 0
openDF$death <- 0
openDF$date_confirmation[openDF$date_confirmation == "20.02.220"] <- "20.02.2020"
#---------------------------line preprocessing----------------------
lineDF <- read.csv("COVID19_line_list_data.csv",header = TRUE)
lineKeeps <- c("age","gender","reporting.date","hosp_visit_date","death","recovered","symptom_onset","visiting.Wuhan","from.Wuhan")
lineDF <- lineDF[,(names(lineDF) %in% lineKeeps)]
names(lineDF)[1] <- "date_confirmation"
names(lineDF)[4] <- "date_onset_symptoms"
names(lineDF)[5] <- "date_admission_hospital"

lineDF$wuhan <- lineDF$visiting.Wuhan | lineDF$from.Wuhan
lineDF$wuhan[lineDF$wuhan == TRUE] <- 1

lineDF <- lineDF[-6]
lineDF <- lineDF[-6]

#-----------------------combining data sets--------------------------
finalSet <- rbind(openDF,lineDF)
finalSet <- finalSet[!(duplicated(finalSet)),]
write.csv(finalSet,file = "preprocessedfinalData.csv",row.names = FALSE)
#--------------------------------gender preprocessing---------------------------
finalSet <- read.csv("preprocessedfinalData.csv",header = TRUE,sep = ",")
finalSet$gender <- as.character(finalSet$gender)
finalSet$gender[finalSet$gender == "female" | finalSet$gender == "Female"] <- "F"
finalSet$gender[finalSet$gender == "male" | finalSet$gender == "Male"] <- "M"
finalSet$gender[finalSet$gender == 4000 |finalSet$gender == "N/A" | finalSet$gender == ""] <- NA
myG <- finalSet$gender[ !(is.na(finalSet$gender))]

#function to get mode 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode <- (getmode(myG))
finalSet$gender[ (is.na(finalSet$gender))] <- mode 

#-----------------------age preprocessing----------------------------
finalSet$age[finalSet$age == "N/A" | finalSet$age == ""] <- NA
finalSet$age <- as.numeric(finalSet$age)
myAge <- finalSet$age[ !(is.na(finalSet$age))] 

ageMean <- as.integer(mean(myAge))
finalSet$age[ (is.na(finalSet$age))] <- ageMean
finalSet$age <- cut(finalSet$age, c(0, 10,20,30,40,50,60,70,80,90,100,Inf), 
                        labels=c('0-9', '10-19', '20-29', '30-39', '40-49',
                                 '50-59','60-69','70-79','80-89','90-99','>=100'))


#--------------------------------death,recovered preprocessing----------------
finalSet$death <- as.character(finalSet$death)
finalSet$recovered <- as.character(finalSet$recovered)
finalSet$death[(finalSet$death != "0")] <- "1"
finalSet$recovered[(finalSet$recovered != "0")] <- "1"
#-------------preprocessing onset dates----------------------
finalSet$date_onset_symptoms[ finalSet$date_onset_symptoms == ""] <- NA
onSetDates<- as.character(finalSet$date_onset_symptoms[!(is.na(finalSet$date_onset_symptoms))])
a <- as.Date(onSetDates,format="%d.%m.%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(onSetDates,format="%m.%d.%Y") # Produces NA when format is not "%d.%m.%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
onSetDates <- a # Put it back in your dataframe
i = 1
while (i <= length(onSetDates) ){
  if(month(onSetDates[i]) >2 & year(onSetDates[i]) == 2020){
    onSetDates[i] <- format(onSetDates[i],"%Y-%d-%m")
  }
  
  i <- i+1
}
meanOnsetDate <-strftime(min(onSetDates)+((max(onSetDates)-min(onSetDates))/2),format = "%d.%m.%Y")

finalSet$date_onset_symptoms<- as.character(finalSet$date_onset_symptoms)
finalSet$date_onset_symptoms[(is.na(finalSet$date_onset_symptoms))] <- meanOnsetDate  
a <- as.Date(finalSet$date_onset_symptoms,format="%d.%m.%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(finalSet$date_onset_symptoms,format="%m.%d.%Y") # Produces NA when format is not "%d.%m.%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
finalSet$date_onset_symptoms <- a # Put it back in your dataframe
i = 1
while (i <= length(finalSet$date_onset_symptoms) ){
  if(month(finalSet$date_onset_symptoms[i]) >2 & year(finalSet$date_onset_symptoms[i]) == 2020){
    finalSet$date_onset_symptoms[i] <- format(finalSet$date_onset_symptoms[i],"%Y-%d-%m")
  }
  
  i <- i+1
}

#------------------preprocessing admission dates------------------------
finalSet$date_admission_hospital[ finalSet$date_admission_hospital == ""] <- NA
adDates<- as.character(finalSet$date_admission_hospital[!(is.na(finalSet$date_admission_hospital))])
a <- as.Date(adDates,format="%d.%m.%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(adDates,format="%m.%d.%Y") # Produces NA when format is not "%d.%m.%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
adDates <- a # Put it back in your dataframe
i = 1
while (i <= length(adDates) ){
  if(month(adDates[i]) >2 & year(adDates[i]) == 2020){
    adDates[i] <- format(adDates[i],"%Y-%d-%m")
  }
  
  i <- i+1
}

meanadDate <-strftime(min(adDates)+((max(adDates)-min(adDates))/2),format = "%d.%m.%Y")
finalSet$date_admission_hospital<- as.character(finalSet$date_admission_hospital)
finalSet$date_admission_hospital[(is.na(finalSet$date_admission_hospital))] <- meanadDate  
a <- as.Date(finalSet$date_admission_hospital,format="%d.%m.%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(finalSet$date_admission_hospital,format="%m.%d.%Y") # Produces NA when format is not "%d.%m.%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
finalSet$date_admission_hospital <- a # Put it back in your dataframe
i = 1
while (i <= length(finalSet$date_admission_hospital) ){
  if(month(finalSet$date_admission_hospital[i]) >2 & year(finalSet$date_admission_hospital[i]) == 2020){
    finalSet$date_admission_hospital[i] <- format(finalSet$date_admission_hospital[i],"%Y-%d-%m")
  }
  
  i <- i+1
}

#------------------preprocessing confirmation dates---------------------
finalSet$date_confirmation[ finalSet$date_confirmation == ""] <- NA
conDates<- as.character(finalSet$date_confirmation[!(is.na(finalSet$date_confirmation))])
a <- as.Date(conDates,format="%d.%m.%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(conDates,format="%m.%d.%Y") # Produces NA when format is not "%d.%m.%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
conDates <- a # Put it back in your dataframe
i = 1
while (i <= length(conDates) ){
  if(month(conDates[i]) >2 & year(conDates[i]) == 2020){
    conDates[i] <- format(conDates[i],"%Y-%d-%m")
  }
  
  i <- i+1
}

meanconDate <-strftime(min(conDates)+((max(conDates)-min(conDates))/2),format = "%d.%m.%Y")
finalSet$date_confirmation<- as.character(finalSet$date_confirmation)
finalSet$date_confirmation[(is.na(finalSet$date_confirmation))] <- meanconDate  
a <- as.Date(finalSet$date_confirmation,format="%d.%m.%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(finalSet$date_confirmation,format="%m.%d.%Y") # Produces NA when format is not "%d.%m.%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
finalSet$date_confirmation <- a # Put it back in your dataframe
i = 1
while (i <= length(finalSet$date_confirmation) ){
  if(month(finalSet$date_confirmation[i]) >2 & year(finalSet$date_confirmation[i]) == 2020){
    finalSet$date_confirmation[i] <- format(finalSet$date_confirmation[i],"%Y-%d-%m")
  }
  
  i <- i+1
}


#-------------------------wuhan------------------------------------
finalSet$wuhan[is.na(finalSet$wuhan)] <- 0

#--------------------writing preprocessed data-------------------
write.csv(finalSet,file = "preprocessedfinalData.csv",row.names = FALSE)

#----------------------------countries vs numbers-------------------------
countConf <- read.csv("time_series_covid_19_confirmed.csv",header=TRUE,sep = ",")
countConf <- countConf[,c(2,102)]
names(countConf) <- c("country","confirmed") 
countConf <- aggregate(confirmed~country,data=countConf,FUN=sum)

countdeath <- read.csv("time_series_covid_19_deaths.csv",header=TRUE,sep = ",")
countdeath <- countdeath[,c(2,87)]
names(countdeath) <- c("country","deaths") 
countdeath <- aggregate(deaths~country,data=countdeath,FUN=sum)

countRec <- read.csv("time_series_covid_19_recovered.csv",header=TRUE,sep = ",")
countRec <- countRec[,c(2,97)]
names(countRec) <- c("country","recovered") 
countRec <- aggregate(recovered~country,data=countRec,FUN=sum)

countConf$death <- countdeath$deaths
countConf$recovered <- countRec$recovered

countConf <- countConf[countConf$confirmed > 1000,]
write.csv(countConf,file = "countries.csv",row.names = FALSE)
