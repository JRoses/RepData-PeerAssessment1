# Reproducible Research: Peer Assessment 1
==========================================
Created by JORDI ROSES on October 19, 2014


### Basic settings

```r
echo = TRUE  # Always make code visible

options( scipen = 1 )  # Turn off scientific notations for numbers
```


### Loading and processing the data

* Load the data.
* Process/transform the data (if necessary) into a format suitable for your analysis.

```r
Current_Folder <- "C:/Users/jordi/Desktop/JORDI/COURSERA/REPRODUCIBLE RESEARCH/Peer Assessment 1/"

File_Name <- paste( Current_Folder , "activity.csv" , sep="" )

DATA <- read.csv( File_Name , colClasses = c( "integer" , "Date" , "factor" ) )

DATA$month <- as.numeric( format( DATA$date , "%m" ) )

DATA_noNA <- na.omit( DATA )

rownames( DATA_noNA ) <- 1 : nrow( DATA_noNA )

head( DATA_noNA )
```

```
##   steps       date interval month
## 1     0 2012-10-02        0    10
## 2     0 2012-10-02        5    10
## 3     0 2012-10-02       10    10
## 4     0 2012-10-02       15    10
## 5     0 2012-10-02       20    10
## 6     0 2012-10-02       25    10
```

```r
dim( DATA_noNA )
```

```
## [1] 15264     4
```


### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

* Make a histogram of the total number of steps taken each day

```r
library( ggplot2 )

ggplot( DATA_noNA , aes( date , steps ) ) + geom_bar( stat = "identity" , colour = "black" , fill = "black" , width = 0.5 ) + facet_grid(. ~ month , scales = "free" ) + labs( title = "Total Number of Steps Per Day" , x = "Date" , y = "Total Number of Steps" )
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

* Calculate and report the mean and median total number of steps taken per day

MEAN / Total number of steps taken per day:

```r
totalSteps <- aggregate( noNA$steps , list( Date = noNA$date ) , FUN = "sum" )$x
mean( totalSteps )
```

```
## [1] 10766
```
MEDIAN / Total number of steps taken per day:

```r
median( totalSteps )
```

```
## [1] 10765
```

### What is the average daily activity pattern?
* Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgSteps <- aggregate( noNA$steps , list( interval = as.numeric( as.character( noNA$interval ) ) ) , FUN = "mean" )
names( avgSteps )[2] <- "meanOfSteps"

ggplot( avgSteps , aes( interval , meanOfSteps ) ) + geom_line( color = "black" , size = 0.5 ) + labs( title = "Time Series Plot of the 5-Minute Interval" , x = "5-Minute Intervals" , y = "Average Number of Steps Taken" )
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgSteps[ avgSteps$meanOfSteps == max( avgSteps$meanOfSteps ) , ]
```

```
##     interval meanOfSteps
## 104      835       206.17
```


### Imputing missing values

Note that there are a number of days/intervals where there are missing values (NAs). The presence of missing days introduce bias into some calculations or summaries of the data.

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):


```r
sum( is.na( data ) )
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

We replace the NAs with the mean.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
DATA_new <- DATA

for ( i in 1 : nrow( DATA_new ) ) {
    if ( is.na( DATA_new$steps[i] ) ) {
        
        DATA_new$steps[i] <- avgSteps[ which( DATA_new$interval[i] == avgSteps$interval ) , ]$meanOfSteps
        
    }
}

head( DATA_new )
```

```
##     steps       date interval month
## 1 1.71698 2012-10-01        0    10
## 2 0.33962 2012-10-01        5    10
## 3 0.13208 2012-10-01       10    10
## 4 0.15094 2012-10-01       15    10
## 5 0.07547 2012-10-01       20    10
## 6 2.09434 2012-10-01       25    10
```

```r
sum( is.na( DATA_new ) )
```

```
## [1] 0
```

* Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 


```r
ggplot( DATA_new , aes( date , steps ) ) + geom_bar( stat = "identity" ,
                                             colour = "black" ,
                                             fill = "black" ,
                                             width = 0.5 ) + facet_grid(. ~ month, scales = "free" ) + labs( title = "Histogram of Total Number of Steps Taken Each Day (no Missing Data)" , x = "Date" , y = "Total Number of Steps" )
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

* Do these values differ from the estimates from the first part of the assignment? 
* What is the impact of imputing missing data on the estimates of the total daily number of steps?

MEAN / Total number of steps taken per day:

```r
TotalSteps_new <- aggregate( DATA_new$steps , 
                           list( Date = DATA_new$date ) , 
                           FUN = "sum" )$x

Mean_new <- mean( TotalSteps_new )

Mean_new
```

```
## [1] 10766.19
```

MEDIAN / Total number of steps taken per day:

```r
Median_new <- median( TotalSteps_new )

Median_new
```

```
## [1] 10766.19
```

Comparative:

```r
Mean_old <- mean( totalSteps )

Median_old <- median( totalSteps )

Mean_new - Mean_old
```

```
## [1] 0
```

```r
Median_new - Median_old
```

```
## [1] 1.188
```

There is no change in the means.
New median (with no NAs) is greater.


### Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
head( DATA_new )
```

```
##     steps       date interval month
## 1 1.71698 2012-10-01        0    10
## 2 0.33962 2012-10-01        5    10
## 3 0.13208 2012-10-01       10    10
## 4 0.15094 2012-10-01       15    10
## 5 0.07547 2012-10-01       20    10
## 6 2.09434 2012-10-01       25    10
```

```r
newData$weekdays <- factor( format( DATA_new$date , "%A" ) )

levels( DATA_new$weekdays )
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels( DATA_new$weekdays ) <- list( weekday = c( "Monday", "Tuesday" , "Wednesday" , "Thursday" , "Friday" ) , weekend = c( "Saturday" , "Sunday" ) )

levels( DATA_new$weekdays )
```

```
## [1] "weekday" "weekend"
```

```r
table( DATA_new$weekdays )
```

```
## 
## weekday weekend 
##   12960    4608
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
Steps_average <- aggregate( DATA_new$steps , 
                      list( interval = as.numeric( as.character( DATA_new$interval ) ) , 
                      weekdays = DATA_new$weekdays ) ,
                      FUN = "mean" )

names( Steps_average )[3] <- "MeanOfSteps"

library( lattice )

xyplot( Steps_average$MeanOfSteps ~ Steps_average$interval | Steps_average$weekdays , 
       layout = c( 1 , 2 ) , 
       type = "l" ,     
       xlab = "Interval" , 
       ylab = "Number of steps" )
```

![plot of chunk figure1](figure1.png) 




