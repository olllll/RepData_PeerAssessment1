---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

## Loading and preprocessing the data

First, we set the global options for the report. We set that in a way that code will always be printed (echo), but messages and warnings thrown by the console (like the ones when you load a library) don't show.

```r
opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
```

Assuming data is already in the right directory, it can be read with a simple read.csv.


```r
activityData<-read.csv("activity.csv", header = T)
```

Once the data is read, there are a number of questions we must ask using it. 


```r
sapply(activityData, class)
```

```
##     steps      date  interval 
## "integer"  "factor" "integer"
```

One thing we are gonna need for sure is to manipulate the interval variable, so we can work with the dates on a proper way. First we convert the variable to character, then we get attach some zeros to the ones that are less than 4 characters long. This way, we make sure we can later parse them in the correct format (hours:minutes). 


```r
activityData$interval<-as.character(activityData$interval)

for (i in 1:length(activityData$interval)) {
        while (nchar(activityData$interval[i])<4) {
                activityData$interval[i]<-paste("0", 
                                               activityData$interval[i], 
                                               sep = ""); 
                if(nchar(activityData$interval[i])==4) break}
}

sum(nchar(activityData$interval)!=4)
```

```
## [1] 0
```

Now that we have "reasonably clean" data, we can start investigating.

## What is mean total number of steps taken per day?


```r
library(dplyr)
stepsbyday<-activityData %>% 
        group_by(date) %>% 
        summarise (total = sum(steps))
options(scipen = 6)
totalmean<-mean(stepsbyday$total, na.rm = T)
totalmedian<-median(stepsbyday$total, na.rm = T)
```

Mean steps taken by day is 10766.1887.

Median steps taken by day is 10765.


```r
library(ggplot2)
bydayhist<-qplot(stepsbyday$total)
bydayhist + geom_histogram() + 
        labs(x = "sum of steps by day",
             title = "histogram of steps by day")
```

![plot of chunk by day histogram](figure/by day histogram.png) 


## What is the average daily activity pattern?


```r
intervalsteps<- activityData %>%
        group_by(interval) %>%
        summarise(average = mean(steps, na.rm =T))

maxinterval<-filter(intervalsteps, average == max(average))
#re-format the time interval to make it "pretty"
split<-strsplit(maxinterval$interval, "")
maxinterval$interval<-paste(split[[1]][1], 
                            split[[1]][2], 
                            ":", 
                            split[[1]][3], 
                            split[[1]][4], sep = "")
```

The 5-minute interval with the maximum number of steps, averaged across all days, is 08:35, with 206.1698 steps.

Now, let's plot the data.


```r
#change format to something readable
intervalsteps$interval<-as.POSIXlt(intervalsteps$interval, format = "%H%M")


q2<-ggplot(intervalsteps, aes(x = interval, y = average))
library(scales)
q2+geom_line() + scale_x_datetime(labels = date_format("%H:%M")) +
        labs(x = "5-min interval", y = "average steps",
             title = "Avg steps by time interval")
```

![plot of chunk steps plot](figure/steps plot.png) 


## Imputing missing values


```r
totalnas<-sum(is.na(activityData$steps))
```

There is a total of 2304 na values in the dataset.

Here is a quick check to understand how the na values are distributed.


```r
table(with(activityData, tapply(steps, date, FUN = function(x) sum(is.na(x)))))
```

```
## 
##   0 288 
##  53   8
```

This shows clearly that there are just two types of days: the ones on which every interval was monitored and the ones on which no interval was monitored. Hence, we can not impute the missing values using the mean/median for the day, but instead we must use the center value of the 5-minute interval.

A quick check on the differences between mean and median for each interval reveals that, in some of the intervals, differences are quite high between the 2 values:

```r
#difference between mean and median for each averaged interval
medians<-with(activityData, tapply(steps, interval, FUN = function(x) median(x, na.rm = T)))
means<-with(activityData, tapply(steps, interval, FUN = function(x) mean(x, na.rm = T)))
differences<-means-medians

#histogram of the distribution of the data
distrInt<- activityData %>%
        group_by(interval) %>%
        summarise(total = sum(steps, na.rm = T))
par(mfrow = c(1,2))
plot(differences, 
     type = "l", 
     main = "(Mean-median) for each interval", 
     xlab = "interval", 
     ylab = "difference (mean-median)")
hist(distrInt$total, 
     main = "Total steps taken each interval", 
     xlab = "total steps")
```

![plot of chunk check differences](figure/check differences.png) 

Considering that the max value in the means is 206.1698, a maximum difference of 187.1698 between the mean and the median seems to imply that there is a skew on the data, which is showed on the histogram. When data is skewed, median is usually a better indicator of the center of the distribution, since it is more robust to outliers, so we decide to impute based on that.


```r
#copy of the dataset
activityImpute<-activityData
#compute medians for each interval
medians<-activityData %>%
        group_by(interval) %>%
        summarise(median = median (steps, na.rm = T))
#get a vector of rows that have na values
nas<-which(is.na(activityImpute$steps))
#impute based on that
for (i in 1:length(nas)){
        activityImpute[nas[i], "steps"]<- 
                medians$median[medians$interval == activityImpute[nas[i], "interval"]]
}
```

One consequence of imputation using the median in this case is that, since it is (almost always) lower than the mean, general values for the distribution should have "dropped". Let's check with an histogram of steps by day (the same as the first question) but with the new data.


```r
actImputedDay<-activityImpute %>%
        group_by(date) %>%
        summarise (total = sum(steps))
meanImp<-mean(actImputedDay$total)
medianImp<-median(actImputedDay$total)
```

The mean of the new dataset with the imputed data is 9503.8689 while the mean of the original dataset before the imputation was 10766.1887.

The median of the imputed data is 10395 while the median of the original dataset before the imputation was 10765.

Now here's a paradox: we wanted to impute according to the median for each interval because it was, arguably, the best indicator for the center. But in doing so, we have skewed the distribution by day. Another imputation method (maybe a more subtle one) would give different results.

Now the histogram:


```r
impdayhist<-qplot(actImputedDay$total)
impdayhist + geom_histogram() + 
          labs(x = "sum of steps by day",
             title = "Steps by day after imputation")
```

![plot of chunk imputed histogram](figure/imputed histogram.png) 


## Are there differences in activity patterns between weekdays and weekends?

First we use the wday function (in the lubridate package) to classify the dates in weekdays and weekends.
It is worth reminding that wday(date) returns an integer with values in the range 1:7, starting on sunday.

Then we group the dataset by two variables, the weekday and the 5-minute interval, and get a mean for each possible combination of the two.



```r
library(lubridate)
activityImpute$weekday<-ifelse(wday(activityImpute$date)>1 & #1 = sunday
                               wday(activityImpute$date)<7,  #7 = saturday
                               "weekday", 
                               "weekend")

wdaydata<-activityImpute %>%
        group_by(interval, weekday) %>%
        summarise(mean = mean(steps))

wdaydata$interval<-as.POSIXlt(wdaydata$interval, format = "%H%M")

wdaydata$weekday<-as.factor(wdaydata$weekday)
```

Then we pass those values to ggplot (the interval on the x axis, the computed mean on the y axis), using the weekday factor to facet the plot. It seems that there are different patterns for weekdays and weekends, and while, in the former, the steps seem to concentrate around some time ranges, in the latter we can see a much more regular distribution of steps taken through the day.


```r
graph<-ggplot(wdaydata, aes(x = interval, y = mean))
graph + geom_line() + facet_grid(weekday~.) + 
        scale_x_datetime(labels = date_format("%H:%M")) +
        labs(x = "time interval", y = "average steps",
             title = "Avg steps by interval (weekday vs weekend)")
```

![plot of chunk weekday plot](figure/weekday plot.png) 
