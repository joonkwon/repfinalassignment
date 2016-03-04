storm.data <- read.csv("repdata-data-StormData.csv", stringsAsFactors = FALSE)
str(storm.data)

columns <- c(2,8,23:28)
storm <- storm.data[,columns]
str(storm)

storm$date <- as.Date(storm$BGN_DATE, "%m/%d/%Y %H:%M:%S")
storm$year <- strftime(storm$date, "%Y")
storm$EVTYPE <- as.factor(storm$EVTYPE)

storm <- transform(storm, PROPDMG = ifelse(PROPDMGEXP == "K", PROPDMG * 1E3, 
                                           ifelse(PROPDMGEXP == "M", PROPDMG * 1E6, 
                                                  ifelse(PROPDMGEXP == "B", 
                                                         PROPDMG * 1E9, PROPDMG))))
storm <- transform(storm, CROPDMG = ifelse(CROPDMGEXP == "K", CROPDMG * 1E3, 
                                           ifelse(CROPDMGEXP == "M", CROPDMG * 1E6, 
                                                  ifelse(CROPDMGEXP == "B", 
                                                         CROPDMG * 1E9, CROPDMG))))
storm <- subset(storm, select = c("date", "year", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "CROPDMG"))
storm$total.dmg <- storm$PROPDMG + storm$CROPDMG
str(storm)

library(dplyr)
library(lattice)
storm.by.year.evt <- group_by(storm, year, EVTYPE)
sum.dmg.by.year.evt <- summarise(storm.by.year.evt, sum.dmg = sum(total.dmg))
head(sum.dmg.by.year.evt)
