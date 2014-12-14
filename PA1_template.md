# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.2
```

```r
unzip("activity.zip")
df <- read.csv("activity.csv")
df$date <- as.Date(df$date)
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

```r
df.completecases <- na.omit(df)
steps.by.day <- aggregate(steps ~ date, data=df.completecases, FUN=sum)
ggplot(steps.by.day, aes(x=steps)) + geom_histogram(binwidth=2500, colour="black", fill="white")
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
mean(steps.by.day$steps)
```

```
## [1] 10766.19
```

```r
median(steps.by.day$steps)
```

```
## [1] 10765
```
## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps.by.interval <- aggregate(steps ~ interval, data=df.completecases, FUN=mean)
ggplot(steps.by.interval, aes(x=interval, y=steps)) + geom_line(stat="identity")
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps.by.interval$interval[which.max(steps.by.interval$steps)]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs

```r
length(which(is.na(df))) 
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The means of the 5-minute intervals are used in place of missing values


```r
mean(na.omit(df$steps))
```

```
## [1] 37.3826
```

```r
median(na.omit(df$steps))
```

```
## [1] 0
```

3. Create a new dataset which contains missing values from original dataset

```r
df.new <- df
df.new$steps[is.na(df.new$steps)] <- mean(na.omit(df$steps))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps.by.day.new <- aggregate(steps ~ date, data=df.new, FUN=sum)
ggplot(steps.by.day.new, aes(x=steps)) +
    geom_histogram(binwidth=2500, colour="black", fill="white")
```

![](./PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
mean(steps.by.day.new$steps)
```

```
## [1] 10766.19
```

```r
median(steps.by.day.new$steps)
```

```
## [1] 10766.19
```

There is no change in the mean value but there is slight change in median value


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
df.new$day[weekdays(as.Date(df.new$date)) %in% c("Saturday", "Sunday")] <- "weekend"
df.new$day[!weekdays(as.Date(df.new$date)) %in% c("Saturday", "Sunday")] <- "weekday"
df.new[, 4] <- as.factor(df.new[, 4])
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).



```r
steps.new <- aggregate(steps ~ interval + day, data=df.new, FUN=mean)
ggplot(steps.new, aes(x=interval, y=steps, group=1)) + geom_line() +  facet_wrap(~ day, ncol=1)
```

![](./PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
