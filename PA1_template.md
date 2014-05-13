# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Load the activity data into a data.table (we like data.tables as they're fast), and convert the date column from characters to Dates.


```r
library(data.table)
actdata <- fread("activity/activity.csv")
actdata[, `:=`(date, as.Date(date, format = "%Y-%m-%d"))]
```

```
##        steps       date interval
##     1:    NA 2012-10-01        0
##     2:    NA 2012-10-01        5
##     3:    NA 2012-10-01       10
##     4:    NA 2012-10-01       15
##     5:    NA 2012-10-01       20
##    ---                          
## 17564:    NA 2012-11-30     2335
## 17565:    NA 2012-11-30     2340
## 17566:    NA 2012-11-30     2345
## 17567:    NA 2012-11-30     2350
## 17568:    NA 2012-11-30     2355
```


## What is mean total number of steps taken per day?

Sum the steps by day, excluding any intervals missing step data.

```r
dailySteps <- actdata[!is.na(steps), sum(steps), by = date]
setnames(dailySteps, "V1", "daily.steps")
```

<!---
knitr won't preview dailySteps based on a setnames() call, so we do it ourselves
-->

```
##           date daily.steps
##  1: 2012-10-02         126
##  2: 2012-10-03       11352
##  3: 2012-10-04       12116
##  4: 2012-10-05       13294
##  5: 2012-10-06       15420
## ---                       
## 49: 2012-11-25       11834
## 50: 2012-11-26       11162
## 51: 2012-11-27       13646
## 52: 2012-11-28       10183
## 53: 2012-11-29        7047
```


### *Make a histogram of the total number of steps taken each day*

```r
hist(dailySteps$daily.steps, 
      breaks = 5,
      col = "cornflowerblue",
      main = "Histogram of Steps Per Day",
      xlab = "Steps Per Day",
      ylab = "Frequency (Days)")
```

![plot of chunk dailystephist](figure/dailystephist.png) 

### *Calculate and report the mean and median total number of steps taken per day*

```r
mean(dailySteps$daily.steps)
```

```
## [1] 10766
```

```r
median(dailySteps$daily.steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
