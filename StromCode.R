download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FstormData.csv.bz2", 
              "repdata_data_stormData.csv.bz2")

baseData <- bzfile("repdata_data_stormData.csv.bz2", "repdata_data_stormData.csv")
stormData <- read.csv2(baseData, sep = ",", stringsAsFactors = FALSE)
unlink(baseData)

str(stormData)

summary(stormData)

head(stormData)

stormData$FATALITIES <- as.numeric(stormData$FATALITIES)

stormData$INJURIES <- as.numeric(stormData$INJURIES)

stormData$EVTYPE <- toupper(stormData$EVTYPE)
eventType <- sort(unique(stormData$EVTYPE))
## Show first 50 event types
eventType[1:50]

library(data.table)

stormData$EVTYPE <- as.factor(stormData$EVTYPE)

fatalities <- as.data.table(subset(aggregate(FATALITIES ~ EVTYPE, data = stormData, 
                                             FUN = "sum"), FATALITIES > 0))
fatalities <- fatalities[order(-FATALITIES), ]

top20Fatalities <- fatalities[1:20, ]
library(ggplot2)
ggplot(data = top20Fatalities, aes(EVTYPE, FATALITIES, fill = FATALITIES)) + geom_bar(stat = "identity") + 
  xlab("Event") + ylab("Fatalities") + coord_flip() + ggtitle("Fatalities caused by Events (top 20) ") + 
  theme(legend.position = "none")

injuries <- as.data.table(subset(aggregate(INJURIES ~ EVTYPE, data = stormData, 
                                           FUN = "sum"), INJURIES > 0))
injuries <- injuries[order(-INJURIES), ]

top20Injuries <- injuries[1:20, ]
ggplot(data = top20Injuries, aes(EVTYPE, INJURIES, fill = INJURIES)) + geom_bar(stat = "identity") + 
  xlab("Event") + ylab("Injuries") + coord_flip() + ggtitle("Injuries caused by Events (top 20) ") + 
  theme(legend.position = "none")

unique(stormData$PROPDMGEXP)

stormData$PROPDMGEXP <- toupper(stormData$PROPDMGEXP)
unique(stormData$PROPDMGEXP)

table(stormData$PROPDMGEXP)

calcExp <- function(x, exp = "") {
  switch(exp, `-` = x * -1, `?` = x, `+` = x, `1` = x, `2` = x * (10^2), `3` = x * 
           (10^3), `4` = x * (10^4), `5` = x * (10^5), `6` = x * (10^6), `7` = x * 
           (10^7), `8` = x * (10^8), H = x * 100, K = x * 1000, M = x * 1e+06, 
         B = x * 1e+09, x)
}

applyCalcExp <- function(vx, vexp) {
  if (length(vx) != length(vexp)) 
    stop("Not same size")
  result <- rep(0, length(vx))
  for (i in 1:length(vx)) {
    result[i] <- calcExp(vx[i], vexp[i])
  }
  result
}

stormData$EconomicCosts <- applyCalcExp(as.numeric(stormData$PROPDMG), stormData$PROPDMGEXP)
summary(stormData$EconomicCosts)

costs <- as.data.table(subset(aggregate(EconomicCosts ~ EVTYPE, data = stormData, 
                                        FUN = "sum"), EconomicCosts > 0))
costs <- costs[order(-EconomicCosts), ]

library(scales)
top20Costs <- costs[1:20, ]
ggplot(data = top20Costs, aes(EVTYPE, EconomicCosts, fill = EconomicCosts)) + geom_bar(stat = "identity") + 
  scale_y_continuous(labels = comma) + xlab("Event") + ylab("Economic costs in $") + coord_flip() +
  ggtitle("Economic costs caused by Events (top 20) ") +  theme(legend.position = "none")
