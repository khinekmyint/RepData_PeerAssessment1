##Loading and preprocessing the data
```{r, echo=FALSE, out.width='800'}```
1.Load the data (i.e. read.csv())


```r
activity<-read.csv("activity.csv")
```

##What is mean total number of steps taken per day?
1.Calculate the total number of steps taken per day


```r
totalsteps<-aggregate(activity$steps,list(activity$date),FUN=sum,na.rm=TRUE)
```

2.Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
hist(totalsteps$x,     main="histogram of the total number of steps taken each day")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totalsteps$x,na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(totalsteps$x,na.rm = TRUE)
```

```
## [1] 10395
```

##What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
averagesteps<-aggregate(activity$steps,list(activity$interval),FUN=mean,na.rm=TRUE)
plot(averagesteps$Group.1,averagesteps$x,type="l",xlab = "interval",ylab = "steps",main ="Average number of step taken")
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png)

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max<-subset(averagesteps,x==max(averagesteps$x))
max$Group.1
```

```
## [1] 835
```
##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. I use the mean for that 5-minute interval, etc.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
for (i in 1:nrow(activity)) {
   if (is.na(activity$steps[i])) {
       activity$steps[i] <- averagesteps[which(activity$interval[i] == averagesteps$Group.1), ]$x
   }
}
```
4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
nafilltotalsteps<-aggregate(activity$steps,list(activity$date),FUN=sum,na.rm=TRUE)

hist(nafilltotalsteps$x,     main="histogram of the total number of steps taken each day")
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png)

```r
mean(nafilltotalsteps$x)
```

```
## [1] 10766.19
```

```r
median(nafilltotalsteps$x)
```

```
## [1] 10766.19
```
Putting missing data makes the mean of total number of steps taken per day changed.

##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity$day<-weekdays(as.Date(activity$date))
activity$day[activity$day == "Saturday"] <- "Weekend"
activity$day[activity$day == "Sunday"] <- "Weekend"
activity$day[activity$day != "Weekend"] <- "Weekday"
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
stepsByDay <- aggregate(steps ~ interval + day, data = activity, mean)

g<-ggplot(stepsByDay,aes(stepsByDay$interval,stepsByDay$steps))+geom_line()
g+facet_grid(stepsByDay$day~.)+labs(title = "Average number of steps taken by Weekend and Weekday", x = "Interval", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26-1.png)
