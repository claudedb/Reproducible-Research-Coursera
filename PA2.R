library(ggplot2)
library(plyr)

## Download and load data
URL<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
filename<-'P2StormData.csv.bz2'

if (!file.exists(filename)){
  download.file(URL,filename)
  
}
# Load Data
raw.data<-read.csv(filename)

## We can create a subset of the data to use only the desired columns

storm.data<-subset(raw.data,select=c('STATE__','BGN_DATE','COUNTY','COUNTYNAME','STATE','EVTYPE','FATALITIES','INJURIES','PROPDMG','PROPDMGEXP','CROPDMG','CROPDMGEXP'))
storm.data$BGN_DATE<-as.Date(storm.data$BGN_DATE,'%m/%d/%Y')
storm.data$EVTYPE<-as.factor(storm.data$EVTYPE)

## For economic effect, we subset per multiplier and multiply the DMG col by the multiplier to have to total amount
storm.data$PROPDMGTOT<-0
storm.data[storm.data$PROPDMGEXP=='h'| storm.data$PROPDMGEXP=='H',]$PROPDMGTOT<-storm.data[storm.data$PROPDMGEXP=='h'| storm.data$PROPDMGEXP=='H',]$PROPDMG*100
storm.data[storm.data$PROPDMGEXP=='k'| storm.data$PROPDMGEXP=='K',]$PROPDMGTOT<-storm.data[storm.data$PROPDMGEXP=='k'| storm.data$PROPDMGEXP=='K',]$PROPDMG*10^3
storm.data[storm.data$PROPDMGEXP=='m'| storm.data$PROPDMGEXP=='M',]$PROPDMGTOT<-storm.data[storm.data$PROPDMGEXP=='m'| storm.data$PROPDMGEXP=='M',]$PROPDMG*10^6
storm.data[storm.data$PROPDMGEXP=='b'| storm.data$PROPDMGEXP=='B',]$PROPDMGTOT<-storm.data[storm.data$PROPDMGEXP=='b'| storm.data$PROPDMGEXP=='B',]$PROPDMG*10^9

storm.data$CROPDMGTOT<-0
storm.data[storm.data$CROPDMGEXP=='h'| storm.data$CROPDMGEXP=='H',]$CROPDMGTOT<-storm.data[storm.data$CROPDMGEXP=='h'| storm.data$CROPDMGEXP=='H',]$CROPDMG*100
storm.data[storm.data$CROPDMGEXP=='k'| storm.data$CROPDMGEXP=='K',]$CROPDMGTOT<-storm.data[storm.data$CROPDMGEXP=='k'| storm.data$CROPDMGEXP=='K',]$CROPDMG*10^3
storm.data[storm.data$CROPDMGEXP=='m'| storm.data$CROPDMGEXP=='M',]$CROPDMGTOT<-storm.data[storm.data$CROPDMGEXP=='m'| storm.data$CROPDMGEXP=='M',]$CROPDMG*10^6
storm.data[storm.data$CROPDMGEXP=='b'| storm.data$CROPDMGEXP=='B',]$CROPDMGTOT<-storm.data[storm.data$CROPDMGEXP=='b'| storm.data$CROPDMGEXP=='B',]$CROPDMG*10^9


## Aggregate all impacts in one data frame
total.impact<-aggregate(cbind(FATALITIES,INJURIES,PROPDMGTOT+CROPDMGTOT)~EVTYPE,storm.data,sum)
names(total.impact)[4]<-paste('DAMAGETOT')

## Find top 5 for Fatalities, Injuries and Damage


top.fatalities<-arrange(total.impact,-FATALITIES)
top.fatalities$EVTYPE<-factor(top5.fatalities$EVTYPE,levels=top.fatalities$EVTYPE)

top.injuries<-arrange(total.impact,-INJURIES)
top.injuries$EVTYPE<-factor(top5.injuries$EVTYPE,levels=top.injuries$EVTYPE)

top.damage<-arrange(total.impact,-DAMAGETOT)
top.damage$EVTYPE<-factor(top.damage$EVTYPE,levels=top.damage$EVTYPE)


## The next part of code will be used to generate graphs

g<-ggplot(top.injuries[1:5,],aes(EVTYPE,INJURIES))
g2<-ggplot(top.fatalities[1:10,],aes(EVTYPE,FATALITIES))
g3<-ggplot(top.damage[1:10,],aes(EVTYPE,DAMAGETOT))

g+geom_bar(stat='identity',col='black',fill='orange',lwd=1)+theme(axis.text.x=element_text(angle=90,hjust=1))
g2+geom_bar(stat='identity',col='black',fill='red',lwd=1)+theme(axis.text.x=element_text(angle=90,hjust=1))

g3+geom_bar(stat='identity',col='black', fill='green', lwd=1)+theme(axis.text.x=element_text(angle=90,hjust=1))
