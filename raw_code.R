unzip("activity.zip")
activityData<-read.csv("activity.csv", header = T)

activityData$interval<-as.character(activityData$interval)

library(lubridate)


for (i in 1:length(activityData$interval)) {
        while (nchar(activityData$interval[i])<4) {
                activityData$interval[i]<-paste("0", 
                                               activityData$interval[i], 
                                               sep = ""); 
                if(nchar(activityData$interval[i])==4) break}
}

##no se necesita?
splits<-strsplit(activityData$interval, "")
for (i in 1:length(activityData$interval)){
        activityData$interval[i]<-paste(splits[[i]][1], 
                                        splits[[i]][2], 
                                        ":", 
                                        splits[[i]][3], 
                                        splits[[i]][4], 
                                        sep = "")
}

mean(nchar(activityData$interval))

#plot a histogram and report the mean and the median
library(dplyr)
q1data<-activityData %>% 
        group_by(date) %>% 
        summarise (total = sum(steps), 
                   mean = mean(steps, na.rm = T), 
                   median = (median(steps, na.rm = T)))


nrow(activityData[activityData$date == "2012-10-02" & activityData$steps == 0,])

table(activityData$steps == 0)
hist(q1data$total)
totalmean<-mean(q1data$total, na.rm = T)
totalmedian<-median(q1data$total, na.rm = T)
abline(v = totalmean)
abline(v = totalmedian, col = "red")

library(ggplot2)
q<-qplot(q1data$total)
q + geom_histogram() + theme_bw()

bydayhist<-qplot(stepsbyday$total)
bydayhist + geom_histogram() + theme_bw()

maxsteps<-activityData %>% 
        group_by(date) %>% 
        summarise(max = max(steps))

##########
#What is the average daily activity pattern?
#
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all days (y-axis)
#
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
#################
names(activityData)
intervalsteps<- activityData %>%
        group_by(interval) %>%
        summarise(average = mean(steps, na.rm =T))

#change format to something readable

intervalsteps$interval<-as.POSIXlt(intervalsteps$interval, format = "%H:%M")

class(intervalsteps$interval)
q2<-ggplot(intervalsteps, aes(x = interval, y = average))
library(scales)
q2+geom_line() + scale_x_datetime(labels = date_format("%H:%M"))



###############
#
#Imputing missing values
#
#Note that there are a number of days/intervals where there are missing values (coded as NA). 
#The presence of missing days may introduce bias into some calculations or summaries of the data.
#
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
#
#Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. 
#For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
#
#Make a histogram of the total number of steps taken each day and 
#Calculate and report the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?
#######################

##are medians better estimators than means?
medians<-with(activityData, tapply(steps, interval, FUN = function(x) median(x, na.rm = T)))
means<-with(activityData, tapply(steps, interval, FUN = function(x) mean(x, na.rm = T)))
summary(medians)
plot(medians)
plot(medians, type = "l")
plot(means, type = "l")
dim(activityData)
mean(means>medians)
mean(intervalsteps>medians)
differences<-means-medians
plot(differences, type = "l")
mean(medians>intervalsteps)
mean(medians == intervalsteps)
which(intervalsteps == medians)
plot((intervalsteps + medians )/2)
plot((intervalsteps + medians )/2, type = "l")
which(medians == 0)
length(which(medians == 0))
sum(medians == 0)
medians == 0
sum(medians == 0)
mean(medians == 0)

#number of nas
sum(is.na(activityData$steps))

table(is.na(activityData$steps))
#quick check of the distribution of na's in the dataset

table(with(activityData, tapply(steps, date, FUN = function(x) sum(is.na(x)))))

#the chosen strategy will be to apply medians for 5 min interval
medians<-activityData %>%
        group_by(interval) %>%
        summarise(median = median (steps, na.rm = T))

activityImpute<-activityData

nas<-which(is.na(activityImpute$steps))

for (i in 1:length(nas)){
        activityImpute[nas[i], "steps"]<- 
                medians$median[medians$interval == activityImpute[nas[i], "interval"]]
}

actImputedDay<-activityImpute %>%
        group_by(date) %>%
        summarise (total = sum(steps))

hist(actImputedDay$total)
mean<-mean(activityImpute$steps)
median<-median(activityImpute$steps)

#bymeans
means<-activityData %>%
        group_by(interval) %>%
        summarise(mean = mean(steps, na.rm = T))

actImpbyMean<-activityData
nasmean<-which(is.na(actImpbyMean$steps))

for (i in 1:length(nasmean)){
        actImpbyMean[nasmean[i], "steps"]<- 
                means$mean[means$interval == actImpbyMean[nasmean[i], "interval"]]
}

meanImpDay<-actImpbyMean %>%
        group_by(date) %>%
        summarise (total = sum(steps))
hist(meanImpDay$total)
meanm<-mean(actImpbyMean$steps)


###############
#
#Are there differences in activity patterns between weekdays and weekends?
#
#For this part the weekdays() function may be of some help here. 
#Use the dataset with the filled-in missing values for this part.
#
#Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.
#
#Make a panel plot containing a time series plot (i.e. type = "l") 
#of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all weekday days or weekend days (y-axis). 
###########################
library(lubridate)
activityImpute$weekday<-ifelse(wday(activityImpute$date)>1 & 
                               wday(activityImpute$date)<7, 
                               "weekday", 
                               "weekend")

wdaydata<-activityImpute %>%
        group_by(interval, weekday) %>%
        summarise(mean = mean(steps))

wdaydata$interval<-as.POSIXlt(wdaydata$interval, format = "%H%M")

wdaydata$weekday<-as.factor(wdaydata$weekday)

graph<-ggplot(wdaydata, aes(x = interval, y = mean))
graph + geom_line() + facet_grid(weekday~.) + scale_x_datetime(labels = date_format("%H:%M")) + theme_bw()


