Loading and preprocessing the data
==================================


```r
data = read.csv('activity.csv', 
                colClasses = c('numeric','character','numeric')  )
```



What is mean total number of steps taken per day?
========================================================

1. Make a histogram of the total number of steps taken each day



```r
totalSteps = aggregate(steps~date, data=data, sum)
hist(totalSteps$steps, main='Total Steps Per Day',
     xlab='Total # of steps',
     ylab='Frequency (# of days)' )
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

2. Calculate and report the **mean** and **median** total number of steps taken per day



```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```

What is the average daily activity pattern?
=============================================

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
ts <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(row.names(ts), ts, type='l', col='blue' , main='Daily Activity Pattern' ,ylab='avg # of steps across all Days', xlab='5-min Interval')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

