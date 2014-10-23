download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
              "repdata_data_StormData.csv.bz2")

csv <- bzfile("repdata_data_StormData.csv.bz2", "repdata_data_StormData.csv")
stormdata <- read.csv2(csv, sep = ",", stringsAsFactors = FALSE)
unlink(csv)

str(stormdata)

summary(stormdata)

head(stormdata)

stormdata$FATALITIES <- as.numeric(stormdata$FATALITIES)

stormdata$INJURIES <- as.numeric(stormdata$INJURIES)

stormdata$EVTYPE <- toupper(stormdata$EVTYPE)
eventtype <- sort(unique(stormdata$EVTYPE))
## Show first 50 event types
eventtype[1:50]

library(data.table)

stormdata$EVTYPE <- as.factor(stormdata$EVTYPE)

fatalities <- as.data.table(subset(aggregate(FATALITIES ~ EVTYPE, data = stormdata, 
                                             FUN = "sum"), FATALITIES > 0))
fatalities <- fatalities[order(-FATALITIES), ]

top20 <- fatalities[1:20, ]
library(ggplot2)
ggplot(data = top20, aes(EVTYPE, FATALITIES, fill = FATALITIES)) + geom_bar(stat = "identity") + 
  xlab("Event") + ylab("Fatalities") + ggtitle("Fatalities caused by Events (top 20) ") + 
   theme(legend.position = "none")

injuries <- as.data.table(subset(aggregate(INJURIES ~ EVTYPE, data = stormdata, 
                                           FUN = "sum"), INJURIES > 0))
injuries <- injuries[order(-INJURIES), ]

top20i <- injuries[1:20, ]
ggplot(data = top20i, aes(EVTYPE, INJURIES, fill = INJURIES)) + geom_bar(stat = "identity") + 
  xlab("Event") + ylab("Injuries") + ggtitle("Injuries caused by Events (top 20) ") + 
   theme(legend.position = "none")

unique(stormdata$PROPDMGEXP)

stormdata$PROPDMGEXP <- toupper(stormdata$PROPDMGEXP)
unique(stormdata$PROPDMGEXP)

table(stormdata$PROPDMGEXP)

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

stormdata$EconomicCosts <- applyCalcExp(as.numeric(stormdata$PROPDMG), stormdata$PROPDMGEXP)
summary(stormdata$EconomicCosts)

costs <- as.data.table(subset(aggregate(EconomicCosts ~ EVTYPE, data = stormdata, 
                                        FUN = "sum"), EconomicCosts > 0))
costs <- costs[order(-EconomicCosts), ]

library(scales)
top20c <- costs[1:20, ]
ggplot(data = top20c, aes(EVTYPE, EconomicCosts, fill = EconomicCosts)) + geom_bar(stat = "identity") + 
  scale_y_continuous(labels = comma) + xlab("Event") + ylab("Economic costs in $") + 
  ggtitle("Economic costs caused by Events (top 20) ") +  theme(legend.position = "none")
