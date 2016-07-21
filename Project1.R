#Code to analyze data for 1st project of reproducible researd

activity.data<-read.csv('~/Documents/R Programming/Reproducible Research/activity.csv')

activity.data$date<-as.Date(as.character(activity.data$date),'%Y-%m-%d')

#Question 1
steps.perday<-aggregate(steps~date,activity.data,sum,na.rm=F)
steps.perday.mean<-mean(steps.perday$steps)
steps.perday.median<-median(steps.perday$steps)
hist(steps.perday$steps,breaks=10,main='Total number of steps taken daily',xlab='Number of steps',ylab='Number of Days')

#Question 2

steps.interval<-aggregate(steps~interval,activity.data,na.rm=F,mean)
with(steps.interval,plot(interval,steps,type='l',xlab='Interval',ylab='Number of steps',main='Average number of steps (all days) per interval'))
max.interval<-steps.interval[which.max(steps.interval$steps),1]

#Question 3

number.NA<-sum(is.na(activity.data))
mean.NA<-mean(is.na(activity.data))

imputed.activity.data<-merge(activity.data,steps.interval, by='interval')
NA.steps<-which(is.na(imputed.activity.data$steps.x))

imputed.activity.data$steps.x[NA.steps]<-round(imputed.activity.data$steps.y[NA.steps])
imputed.activity.data<-imputed.activity.data[,1:3]

imputed.daily.steps<-aggregate(steps.x~date,imputed.activity.data,sum)
mean.daily.steps<-mean(imputed.daily.steps$steps.x)
median.daily.steps<-median(imputed.daily.steps$steps.x)
hist(imputed.daily.steps$steps.x,col='red',breaks=10,main='Total number of steps taken daily (NA replaced by interval mean)',xlab='Number of steps',ylab='Number of Days')

#Question 4
imputed.activity.data$weekday<-as.factor(weekdays(imputed.activity.data$date))
weekday.name<-c('Monday','Tuesday','Wednesday','Thursday','Friday')
imputed.activity.data$weekend<-ifelse(imputed.activity.data$weekday %in% weekday.name,'Weekday','Weekend')
imputed.interval.mean<-aggregate(steps.x~interval+weekend,imputed.activity.data,mean)
library(lattice)
with(imputed.interval.mean,xyplot(steps.x~interval|weekend,type='l',ylab='Avg number of steps',xlab='Interval',layout=c(1,2)))
