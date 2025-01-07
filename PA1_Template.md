---
title: "Reproducible Research: Peer Assessment 1"
author: "Nirvana Kistow"
output: 
  html_document:
    keep_md: true
---

``` r
knitr::opts_chunk$set(fig.path = "figures/")
```

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
activity_cleaned = na.omit(activity)
head(activity_cleaned)
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
from the first few rows headed from the activity_cleaned dataset we can see the NA values have been removed.
## What is mean total number of steps taken per day?
to answer the question 'What is mean total number of steps taken per day?' I will first calculate the the total number of steps for each day
**Calculate the number of steps per day

``` r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.3.3
```

``` r
stepsperday = aggregate(steps~date, activity_cleaned, sum)
head(stepsperday)
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
hist(stepsperday$steps, main = "Total Daily Steps Taken Each Day", xlab = "Number of Steps", col = "green")
```

![](figures/unnamed-chunk-6-1.png)<!-- -->
Calculate and report the mean and median of the total steps taken per day

``` r
Mean_Steps = mean(stepsperday$steps)
print(Mean_Steps)
```

```
## [1] 10766.19
```
from the output we can see the mean is 10766.19

``` r
Median_Steps = median(stepsperday$steps)
Median_Steps
```

```
## [1] 10765
```
from the output we can see the median is 10765
## What is the average daily activity pattern?
Next, is to make a time series plot of the 5 min intervals and average steps taken per day. 

``` r
AverageSteps = aggregate(steps ~ interval, activity_cleaned, mean)

plot(y= AverageSteps$steps, x= AverageSteps$interval,type = "l",  main = "Time Series of Step over intervals", xlab = "Intervals (5 mins)", ylab = "Amount of Steps", col = "green" )
```

![](figures/unnamed-chunk-9-1.png)<!-- -->
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
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.3.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
# Fill missing steps with average steps for the corresponding interval
fill_activity <- activity %>%
  left_join(AverageSteps, by = "interval") %>%
  mutate(
    steps = ifelse(is.na(steps.x), steps.y, steps.x) # Fill missing steps
  ) %>%
  select(-steps.x, -steps.y) # Remove unnecessary column

head(fill_activity)
```

```
##         date interval     steps
## 1 2012-10-01        0 1.7169811
## 2 2012-10-01        5 0.3396226
## 3 2012-10-01       10 0.1320755
## 4 2012-10-01       15 0.1509434
## 5 2012-10-01       20 0.0754717
## 6 2012-10-01       25 2.0943396
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.
- the filling process we created a new dataset called "fill_activity"

``` r
head(fill_activity)
```

```
##         date interval     steps
## 1 2012-10-01        0 1.7169811
## 2 2012-10-01        5 0.3396226
## 3 2012-10-01       10 0.1320755
## 4 2012-10-01       15 0.1509434
## 5 2012-10-01       20 0.0754717
## 6 2012-10-01       25 2.0943396
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

``` r
totaldailysteps = aggregate(steps~date, fill_activity, sum)

hist(totaldailysteps$steps, main = "Total Daily Steps Taken Each Day", xlab = "Number of Steps", col = "green")
```

![](figures/unnamed-chunk-15-1.png)<!-- -->

next we will calculate the mean and median

``` r
Mean_Steps2 = mean(totaldailysteps$steps)
print(Mean_Steps2)
```

```
## [1] 10766.19
```


``` r
Median_Steps2 = median(totaldailysteps$steps)
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
fill_activity <- activity_cleaned %>%
  mutate(
    date = as.Date(date, format = "%Y-%m-%d"), # Convert date to Date format
    day = weekdays(date),                     # Get weekday name
    weekday = ifelse(day %in% c("Saturday", "Sunday"), "weekend", "weekday") # Classify as weekend or weekday
  ) %>%
  mutate(weekday = factor(weekday)) # Convert to factor

head(fill_activity)
```

```
##     steps       date interval     day weekday
## 289     0 2012-10-02        0 Tuesday weekday
## 290     0 2012-10-02        5 Tuesday weekday
## 291     0 2012-10-02       10 Tuesday weekday
## 292     0 2012-10-02       15 Tuesday weekday
## 293     0 2012-10-02       20 Tuesday weekday
## 294     0 2012-10-02       25 Tuesday weekday
```
Finally, we will create the panel plot for the weekdays and weekends

``` r
# Split data into weekday and weekend
Weekday <- fill_activity[fill_activity$weekday == "weekday", ]
Weekend <- fill_activity[fill_activity$weekday == "weekend", ]

# Calculate average steps by interval for weekdays and weekends
Average_weekday <- aggregate(steps ~ interval, data = Weekday, FUN = mean)
Average_weekend <- aggregate(steps ~ interval, data = Weekend, FUN = mean)

# Set up the panel plot layout
par(mfrow = c(2, 1), mar = c(4, 4, 3, 2)) # Adjust margins for better spacing

# Weekday time series plot
plot(
  Average_weekday$interval, Average_weekday$steps, 
  type = "l", col = "green",
  main = "Weekday Time Series",
  xlab = "Interval (5 mins)", 
  ylab = "Average Steps"
)

# Weekend time series plot
plot(
  Average_weekend$interval, Average_weekend$steps, 
  type = "l", col = "blue",
  main = "Weekend Time Series",
  xlab = "Interval (5 mins)", 
  ylab = "Average Steps"
)
```

![](figures/unnamed-chunk-19-1.png)<!-- -->


