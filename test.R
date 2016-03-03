storm.data <- read.csv("repdata-data-StormData.csv", stringsAsFactors = FALSE)
str(storm.data)

columns <- c(2,8,23:28)
storm <- storm.data[,columns]
str(storm)

storm$date <- as.Date(storm$BGN_DATE, "%m/%d/%Y %H:%M:%S")
storm$EVTYPE <- as.factor(storm$EVTYPE)

storm <- transform(storm, PROPDMG = ifelse(PROPDMGEXP == "K", PROPDMG * 1E3, 
                                           ifelse(PROPDMGEXP == "M", PROPDMG * 1E6, 
                                                  ifelse(PROPDMGEXP == "B", 
                                                         PROPDMG * 1E9, PROPDMG))))
storm <- transform(storm, CROPDMG = ifelse(CROPDMGEXP == "K", CROPDMG * 1E3, 
                                           ifelse(CROPDMGEXP == "M", CROPDMG * 1E6, 
                                                  ifelse(CROPDMGEXP == "B", 
                                                         CROPDMG * 1E9, CROPDMG))))
storm <- subset(storm, select = c("date", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "CROPDMG"))
str(storm)