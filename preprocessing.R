#library("xts")
setwd("E:\\4th Year\\Big data\\project")


confDF <- read.csv("time_series_covid_19_confirmed.csv",header=TRUE,sep = ",")
confDF <- confDF[-1]
confDF <- confDF[-1]
confDF <- confDF[-1]
confDF <- confDF[-1]
conf <- confDF[99,]
conf <- unlist(c(conf),use.names = FALSE)
conf <- as.numeric(as.vector(conf))
dates <- seq.Date(as.Date("2020-1-22"),as.Date("2020-4-23"),"day")
#confTS <- ts(conf,start = c(2020,as.numeric(format(dates[1],"%j"))), frequency = 365)
cDF <- data.frame(dates,conf)
write.csv(cDF,file = "confirmed2.csv",row.names = FALSE)


deadDF <- read.csv("time_series_covid_19_deaths.csv",header=TRUE,sep = ",")
deadDF <- deadDF[-1]
deadDF <- deadDF[-1]
deadDF <- deadDF[-1]
deadDF <- deadDF[-1]
dead <- deadDF[99,]
dead <- unlist(c(dead),use.names = FALSE)
dead <- as.numeric(as.vector(dead))
dDF <- data.frame(dates,dead)
write.csv(dDF,file = "deaths.csv",row.names = FALSE)


recDF <- read.csv("time_series_covid_19_recovered.csv",header=TRUE,sep = ",")
recDF <- recDF[-1]
recDF <- recDF[-1]
recDF <- recDF[-1]
recDF <- recDF[-1]
rec <- recDF[91,]
rec <- unlist(c(rec),use.names = FALSE)
rec <- as.numeric(as.vector(rec))
rDF <- data.frame(dates,rec)
write.csv(rDF,file = "recovered.csv",row.names = FALSE)


openDF <- read.csv("COVID19_open_line_list.csv",header = TRUE)
openDrops <- c("ï..ID","X","X.1","X.2","X.3","X.4",
           "X.5","X.6","X.7","X.8","X.9","X.10","X.11",
           "latitude","longitude","city","province","country","geo_resolution",
           "symptoms","reported_market_exposure","chronic_disease_binary",
           "chronic_disease","source","sequence_available","outcome",
           "notes_for_discussion","location","admin1","admin2","admin3",
           "country_new","admin_id","data_moderator_initials",
           "date_death_or_discharge","wuhan.0._not_wuhan.1.",
           "travel_history_dates","travel_history_location","additional_information")
openDF <- openDF[,!(names(openDF) %in% openDrops)]
names(openDF)[2] <- "gender"
names(openDF)[6] <- "wuhan"
openDF$wuhan[openDF$wuhan == "no"] <- 0
openDF$wuhan[openDF$wuhan == ""] <- -1
openDF$wuhan[openDF$wuhan != 0 | openDF$wuhan != NA] <- 1
openDF$recovered <- 0
openDF$death <- 0


lineDF <- read.csv("COVID19_line_list_data.csv",header = TRUE)
lineDrops <- c("ï..id","X","X.1","X.2","X.3","X.4",
               "X.5","X.6","source","link","symptom",
               "case_in_country","summary","exposure_end",
               "exposure_start","If_onset_approximated",
               "location","country")
lineDF <- lineDF[,!(names(lineDF) %in% lineDrops)]
names(lineDF)[1] <- "date_confirmation"
names(lineDF)[4] <- "date_onset_symptoms"
names(lineDF)[5] <- "date_admission_hospital"

lineDF$wuhan <- lineDF$visiting.Wuhan | lineDF$from.Wuhan
lineDF$wuhan[lineDF$wuhan == TRUE] <- 1

lineDF <- lineDF[-6]
lineDF <- lineDF[-6]

finalSet <- rbind(openDF,lineDF)
finalSet$gender <- as.character(finalSet$gender) 
finalSet$gender[finalSet$gender == "female" | finalSet$gender == "Female"] <- "F"
finalSet$gender[finalSet$gender == "male" | finalSet$gender == "Male"] <- "M"
finalSet$gender[finalSet$gender == 4000] <- NA
write.csv(finalSet,file = "finalData.csv",row.names = FALSE)
