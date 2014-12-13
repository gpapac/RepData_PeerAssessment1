# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# load data
activity_data <- read.csv(unz("activity.zip", filename="activity.csv"))
# convert date column to Date data type
activity_data$date <- as.Date(activity_data$date)
```
## What is mean total number of steps taken per day?


```r
steps_per_day <- with(activity_data, aggregate(steps, by=list(date), FUN="sum"))
names(steps_per_day) <- c("date", "total_steps")
hist(steps_per_day$total_steps, main="Histogram of total steps per day", xlab="Total steps per day", col="orange", breaks=10)
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean_steps <- as.integer(mean(steps_per_day$total_steps, na.rm = TRUE))
median_steps <- as.integer(median(steps_per_day$total_steps, na.rm = TRUE))
```
The mean total number of steps taken per day is: **10766** and the median is: **10765**


## What is the average daily activity pattern?

```r
steps_per_interval <- aggregate(steps ~ interval, activity_data, FUN="mean", na.action = na.omit)
plot(steps_per_interval, type="l")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
interval_with_max_mean_steps <- steps_per_interval[which.max(steps_per_interval$steps),]$interval
max_mean_steps <- round(max(steps_per_interval$steps), 0)
```

The 5-minute interval than contains the maximum number of steps on average (**206**) accross all the days is: **835**


## Imputing missing values

```r
rows_with_na <- sum(is.na(activity_data$steps))
```


**2304** rows have NA values in the "steps" column



```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.2
```

```r
# create a copy of the data
complete_data <-activity_data
# replace the NA values, with the mean of the corresponding interval (already calculated in: steps_per_interval)
# We use join in order to keep the original sorting of the data set
complete_data[is.na(complete_data$steps),]$steps <- join(complete_data, steps_per_interval, by="interval")[is.na(complete_data$steps),4]


complete_steps_per_day <- with(complete_data, aggregate(steps, by=list(date), FUN="sum"))
names(complete_steps_per_day) <- c("date", "total_steps")
hist(complete_steps_per_day$total_steps, main="Histogram of total steps per day (NAs repleaced by interval mean steps)", xlab="Total steps per day", col="orange", breaks=10)
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
mean_steps <- as.integer(mean(complete_steps_per_day$total_steps, na.rm = TRUE))
median_steps <- as.integer(median(complete_steps_per_day$total_steps, na.rm = TRUE))
```

The mean total number of steps taken per day after imputing missing data with the mean of the corresponding interval for all days, is: **10766** and the median is: **10766**

As we can see the impact of imputing missing data with this method, on the mean and median values, is extremelly small. Only the frequency of the column corresponding to the mean, has increased.


## Are there differences in activity patterns between weekdays and weekends?
