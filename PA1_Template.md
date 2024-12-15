---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
First we will load the data into r using read.csv

``` r
activity <- read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
From the sample of the first few rows in the activity dataset we can see that ther are NA values in the steps columns. We will remove the NA values. 


``` r
activity_clean = na.omit(activity)
head(activity_clean)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```
from the first few rows headed from the activity_cleandataset we can see the NA values have been removed.
## What is mean total number of steps taken per day?
to answer the question 'What is mean total number of steps taken per day?' I will first calculate the the total number of steps for each day
**Calculate the total number of steps per day

``` r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.3.3
```

``` r
totaldailysteps = aggregate(steps~date, activity_clean, sum)
head(totaldailysteps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
Create a histogram of the total steps per day

``` r
hist(totaldailysteps$steps, main = "Total Daily Steps Taken Each Day", xlab = "Number of Steps", col = "blue")
```

![](PA1_Template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
Calculate and report the mean and median of the total steps taken per day

``` r
Mean_Steps = mean(totaldailysteps$steps)
print(Mean_Steps)
```

```
## [1] 10766.19
```
from the output we can see the mean is 10766.19

``` r
Median_Steps = median(totaldailysteps$steps)
Median_Steps
```

```
## [1] 10765
```
from the output we can see the median is 10765
## What is the average daily activity pattern?
Next, is to make a time series plot of the 5 min intervals and average steps taken per day. 

``` r
AverageSteps = aggregate(steps ~ interval, activity_clean, mean)

plot(y= AverageSteps$steps, x= AverageSteps$interval,type = "l",  main = "Time Series of Step over intervals", xlab = "Intervals (5 mins)", ylab = "Amount of Steps", col = "blue" )
```

![](PA1_Template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
Next, we will calculate the interval with the maximum number of steps

``` r
AverageSteps[grep(max(AverageSteps$steps), AverageSteps$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```
## Imputing missing values
Calculate and report the total number of missing values in the dataset.

``` r
sum(is.na(activity))
```

```
## [1] 2304
```
now lets find where these na values are in the dataset

``` r
na_steps = sum(is.na(activity$steps))
na_interval = sum(is.na(activity$interval))
na_date = sum(is.na(activity$date))

print(na_steps)
```

```
## [1] 2304
```

``` r
print(na_interval)
```

```
## [1] 0
```

``` r
print(na_date)
```

```
## [1] 0
```
we can see that all the na values are in the steps column of the dataset.

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.
- to fill these missing of the steps we will use the mean of each interval


``` r
fill_activity <- activity
fill_activity$steps <- ifelse(is.na(fill_activity$steps) == TRUE, AverageSteps$steps[AverageSteps$interval %in% fill_activity$interval], fill_activity$steps)
head(fill_activity)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.
- the filling process we created a new dataset called "fill_activity"

``` r
head(fill_activity)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

``` r
totaldailysteps2 = aggregate(steps~date, fill_activity, sum)

hist(totaldailysteps2$steps, main = "Total Daily Steps Taken Each Day", xlab = "Number of Steps", col = "blue")
```

![](PA1_Template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

next we will calculate the mean and median

``` r
Mean_Steps2 = mean(totaldailysteps2$steps)
print(Mean_Steps2)
```

```
## [1] 10766.19
```


``` r
Median_Steps2 = median(totaldailysteps2$steps)
Median_Steps2
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment?
- the mean stayed the same while the median differed to be the same as the mean

What is the impact of imputing missing data on the estimates of the total daily number of steps?
- more days with higher steps compared to the first histogram.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


``` r
fill_activity$date = as.Date(strptime(activity$date, format="%Y-%m-%d"))
fill_activity$day = weekdays(fill_activity$date)
fill_activity$weekday = as.character(rep(0, times=17568))
for(x in 1:17568) {
    if(fill_activity[x, 4] %in% c("Saturday", "Sunday")) {
        fill_activity[x, 5] <- "weekend"
    } else {
        fill_activity[x, 5] <- "weekday"
    }
}
fill_activity$weekday <- factor(fill_activity$weekday)
head(fill_activity)
```

```
##       steps       date interval    day weekday
## 1 1.7169811 2012-10-01        0 Monday weekday
## 2 0.3396226 2012-10-01        5 Monday weekday
## 3 0.1320755 2012-10-01       10 Monday weekday
## 4 0.1509434 2012-10-01       15 Monday weekday
## 5 0.0754717 2012-10-01       20 Monday weekday
## 6 2.0943396 2012-10-01       25 Monday weekday
```
Finally, we will create the panel plot for the weekdays and weekends

``` r
Weekday = fill_activity[fill_activity$weekday == "weekday",]
Weekend = fill_activity[fill_activity$weekday == "weekend",]

Average_weekday = aggregate(steps~interval, Weekday, mean)
Average_weekend = aggregate(steps~interval, Weekend, mean)

par(mfrow=c(2, 1), mar=c(4, 4.1, 3, 2.1))
plot(Average_weekday$interval, Average_weekday$steps, type = "l", main= "Weekday Time Series", xlab="Interval(5 mins)", ylab = "Steps", col = "green")
plot(Average_weekend$interval, Average_weekend$steps, type = "l", main= "Weekend Time Series", xlab="Interval(5 mins)", ylab = "Steps", col = "blue")
```

![](PA1_Template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


