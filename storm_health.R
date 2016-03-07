## ----cache=TRUE----------------------------------------------------------
storm.data <- read.csv("repdata-data-StormData.csv", stringsAsFactors = FALSE)
str(storm.data)

## ----cache=TRUE----------------------------------------------------------
columns <- c(2,8,23:28)
storm <- storm.data[,columns]
str(storm)

storm$date <- as.Date(storm$BGN_DATE, "%m/%d/%Y %H:%M:%S")
storm$year <- strftime(storm$date, "%Y")
storm$EVTYPE <- as.factor(storm$EVTYPE)

## ----cache=TRUE----------------------------------------------------------
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

## ----message=FALSE-------------------------------------------------------
library(dplyr)

## ----cache=TRUE----------------------------------------------------------
storm.by.year.evt <- group_by(storm, year, EVTYPE)
storm.by.year.evt

## ----cache=TRUE----------------------------------------------------------
sum.dmg<- summarise(storm.by.year.evt, sum.dmg = sum(total.dmg))
head(sum.dmg)
dim(sum.dmg)
length(unique(sum.dmg$EVTYPE))

## ----cache=TRUE----------------------------------------------------------
sum.dmg.by.evt <- group_by(sum.dmg, EVTYPE)
mean.dmg.by.evt <- summarise(sum.dmg.by.evt, dmg = mean(sum.dmg))
mean.dmg.by.evt <- mean.dmg.by.evt[order(mean.dmg.by.evt$dmg, decreasing = TRUE),]
mean.dmg.by.evt

## ----message=FALSE-------------------------------------------------------
library(ggplot2)

## ------------------------------------------------------------------------
mean.dmg.top <- mean.dmg.by.evt[1:10,]
mean.dmg.top$dmg <- mean.dmg.top$dmg / 1e6
g <- ggplot(mean.dmg.top, aes(x=EVTYPE, y=dmg))
g + geom_bar(stat="identity", fill="green") +  
    theme(axis.text.x = element_text(angle = 90, hjust=1)) +
    geom_text(aes(label=paste(sprintf( "%1.0f", dmg), "M" ), y=dmg + 700, colour = "pink" ),
              size = 3, show.legend = FALSE) +
    xlab("Event Type") +ylab("Cost of Damage (Million)")

## ----cache=TRUE----------------------------------------------------------
sum.inj.fat <- summarise(storm.by.year.evt, total.injuries=sum(INJURIES), total.fatalities = sum(FATALITIES))
sum.inj.fat

## ----cache=TRUE----------------------------------------------------------
inj.fat.by.event <- group_by(sum.inj.fat, EVTYPE)
inj.fat.by.event
mean.inj.fat <- summarise(inj.fat.by.event, mean.inj = mean(total.injuries), 
                                   mean.fat = mean(total.fatalities))
mean.inj.fat.sort.by.fat <- mean.inj.fat[order(mean.inj.fat$mean.fat, decreasing = TRUE),]
mean.inj.fat.sort.by.inj <- mean.inj.fat[order(mean.inj.fat$mean.inj, decreasing = TRUE),]
 
mean.inj.fat.sort.by.inj
mean.inj.fat.sort.by.fat


## ----cache=TRUE----------------------------------------------------------
top.inj <- mean.inj.fat.sort.by.inj[1:10,]$EVTYPE
top.fat <- mean.inj.fat.sort.by.fat[1:10,]$EVTYPE
top.inj.fat.evt <- union(top.inj, top.fat)
top.inj.fat.evt

## ----cache=TRUE----------------------------------------------------------
top.inj.fat <- mean.inj.fat[mean.inj.fat$EVTYPE %in% top.inj.fat.evt,]
top.inj.fat

## ----message=FALSE-------------------------------------------------------
library(tidyr)
library(plyr)

## ----cache=TRUE----------------------------------------------------------
top.evt <- gather(top.inj.fat, "type", "count", 2:3)
top.evt$type <- as.factor(top.evt$type)
top.evt

## ----cache=TRUE----------------------------------------------------------
g <- ggplot(top.evt, aes(EVTYPE,count,fill=type))
g + theme(axis.text.x = element_text(angle = 90, hjust=1)) +
    geom_bar(stat="identity", position="dodge") +
    geom_text(aes(label=sprintf( "%1.0f", count),y = count + 50),position=position_dodge(width=1), size=2.5) +
    scale_fill_discrete(name="", breaks=c("mean.fat", "mean.inj"), labels=c("fatalities","injuries")) +
    xlab("Event") + ylab("Mean Count")

## ----cache=TRUE----------------------------------------------------------
top.evt <- ddply(top.evt, "EVTYPE", transform, label_height = cumsum(count) - count / 2 )

## ----cache=TRUE----------------------------------------------------------
g <- ggplot(top.evt, aes(EVTYPE,count,fill=type))
g + theme(axis.text.x = element_text(angle = 90, hjust=1)) +
    geom_bar(stat="identity", position="stack") +
    geom_text(aes(label=sprintf( "%1.0f", count), y = label_height), size=2.2) +
    scale_fill_discrete(name="", breaks=c("mean.fat", "mean.inj"), labels=c("fatalities","injuries")) +
    xlab("Event") + ylab("Mean Count")

## ------------------------------------------------------------------------
sessionInfo()

