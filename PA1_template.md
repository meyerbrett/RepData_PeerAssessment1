---
title: "Investigating Personal Step Counts"
author: "Brett Meyer"
date: "12-30-2025"
output: 
  html_document:
    number_sections: true
---



The tasks in this assignment involve investigating step counts from an anonymous person's wearable throughout the course of a day and between days of the week over a period of several weeks. After generating some basic summary statistics and plots for the number of steps by day, we investigate how steps varied by 5-minute intervals throughout the day, impute missing values

## Downloading and Inspecting the Data

After setting the appropriate working directory, we download the data with the following code and give a basic summary of the data. As we can see, the dataset consists of three variables: 'steps,' 'date,' and 'interval.' The former two are units of time, denoting the date over a two month period (October-November 2012) and the each five-minute interval of every day. 'steps' records the number of steps taken in each day-interval. There are 17,568 total observations in the dataset.


``` r
data<-read.csv("activity.csv")
summary(data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

``` r
nrow(data)
```

```
## [1] 17568
```

## Basic Summary Statistics and Visualizations

Then we generate some basic summary statistics and visualizations including a total count of steps, the mean and median of steps taken per day, and a histogram of the frequency of daily step counts.

To generate a sum of steps for the entire dataset, the following code uses ymd() from the package Lubridate to covert the date variable to date format and sums the variable 'steps,' ignoring missing values. As we can see, this person took 570,608 steps during this time period.

``` r
library(lubridate)
data$date<-ymd(data$date)
steps_day<-aggregate(steps~date, data=data, FUN=sum, na.rm=TRUE)
steps_day$steps<-as.numeric(steps_day$steps)
print(sum(steps_day$steps))
```

```
## [1] 570608
```

Then we generate a histogram showing the frequency of daily step counts in five bins. As we can see, this person most frequently had step counts between 10,000 and 15,000 per day:

``` r
hist(steps_day$steps, 
     xlab="Steps Taken", 
     ylab="Number of Days", 
     main="Frequency (Days) of Step Count")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

Finally, we generate the mean and median step counts per day. These are very similar (10766.19 and 10765 respectively), confirming what we saw in the histogram: that the data are close to normally distributed.

``` r
print(mean(steps_day$steps))
```

```
## [1] 10766.19
```

``` r
print(median(steps_day$steps))
```

```
## [1] 10765
```

## Daily Activity Pattern

Next we investigate the number of steps across the 288 5-minute intervals throughout the day. First, we generate a line plot of steps per 5-minute interval, summing each 5-minute interval across all days in the dataset. As we can see, there's a spike around 8 or 9, likely when this person starts their work day. Investigating this further, we find that the interval with the highest average number of steps is 8:35, during which this person averaged about 206 steps throughout the two-month period.


``` r
steps_int<-aggregate(steps~interval, data=data, FUN=mean, na.rm=TRUE)
plot(steps_int$interval, steps_int$steps, 
     type = "l", lty = 1,
     xlab="Time of Day (Minutes)",
     ylab="Average Number of Steps in Interval",
     main="Average Number of Steps at Each 5-Minute Inverval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

``` r
steps_int[which.max(steps_int$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing Missing Values

Our main variable 'steps' has a substantial amount of missing data--2304 out of a total of 17,568 observations. In this section, we impute missing data and regenerate some of our key statistics and figures. 


``` r
sum(is.na(data$steps))
```

```
## [1] 2304
```

To impute the missing data, we replace missings with the average number of steps for each 5-minute interval across all days in the dataset, which we had previously generated in steps_int.


``` r
new_data<-data
na_idx<-is.na(new_data$steps)
new_data$steps[na_idx] <- steps_int$steps[
  match(new_data$interval[na_idx], steps_int$interval)
]
```
Then we generate a new dataset that sums the number of steps per day and recreate the histogram and mean/median daily statistics from section 2. As we can see, the histogram looks similar, except that there are now 35 days in the modal bar (10,000-15,000), several more than in the non-imputed data. 


``` r
new_steps_day<-aggregate(steps~date, data=new_data, FUN=sum, na.rm=TRUE)

hist(new_steps_day$steps, 
     xlab="Steps Taken", 
     ylab="Number of Days", 
     main="Frequency (Days) of Step Count")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

The mean and median of the two datasets are almost identical:


``` r
print(mean(new_steps_day$steps))
```

```
## [1] 10766.19
```

``` r
print(median(new_steps_day$steps))
```

```
## [1] 10766.19
```

Although this is consistent with the histogram, it's a bit surprising considering what we did. We might think that replacing 2300 observations would change the mean and median of the data, but it did not. The puzzle is perhaps even greater when we compare total step counts in the two datasets. As we can see below, the total step count is over 85,000 steps *higher* in the imputed data.


``` r
print(sum(new_steps_day$steps))
```

```
## [1] 656737.5
```

``` r
print(sum(steps_day$steps))
```

```
## [1] 570608
```

## Steps on Weekdays and Weekends

Finally we use our imputed dataset to investigate the number of steps at 5-minute intervals for weekdays and weekends. First, we use R's weekdays() function to generate a new factor variable denoting whether each date was a weekday (weekday=="weekday") or weekend (weekday=="weekend").


``` r
new_steps_day$weekday<-weekdays(new_steps_day$date)
new_steps_day$weekday[new_steps_day$weekday=="Monday" | 
                      new_steps_day$weekday=="Tuesday" | 
                      new_steps_day$weekday=="Wednesday" | 
                      new_steps_day$weekday=="Thursday" | 
                      new_steps_day$weekday=="Friday" ]<-"weekday"
new_steps_day$weekday[new_steps_day$weekday=="Saturday" | 
                      new_steps_day$weekday=="Sunday"]<-"weekend"
new_steps_day$weekday<-as.factor(new_steps_day$weekday)
```

Then we recreate our line graph of steps per 5-minute interval summed across all days from section 3. We use the Lattice package to create this plot separately for weekdays and weekends.


``` r
new_data_int<-new_data
new_data_int$weekday<-weekdays(new_data_int$date)
new_data_int$weekday[new_data_int$weekday=="Monday" | 
                       new_data_int$weekday=="Tuesday" | 
                       new_data_int$weekday=="Wednesday" | 
                       new_data_int$weekday=="Thursday" | 
                       new_data_int$weekday=="Friday" ]<-"Weekday"
new_data_int$weekday[new_data_int$weekday=="Saturday" | 
                       new_data_int$weekday=="Sunday"]<-"Weekend"
new_data_int$weekday<-as.factor(new_data_int$weekday)
new_steps_int<-aggregate(steps~interval + weekday, data=new_data_int, FUN=mean, na.rm=TRUE)

library(lattice)
xyplot(steps ~ interval | weekday, data = new_steps_int,
       type = "l",
       layout = c(1,2), # Optional: arranges the 5 days in a single row
       main = "Average Number of Steps at Each 5-Minute Inverval",
       xlab = "Time of Day (Minutes)",
       ylab = "Average Number of Steps in Interval")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

There are some interesting, but sensible differences. While both plots show a spike in steps before 10 AM, the average number of steps between roughly 5 and 8 AM on weekends is lower, suggesting that this person gets up for work during this period on weekdays but sleeps in on weekends. On weekdays, there are spikes and lulls throughout the day, suggesting that this person works a desk job. On weekends, the spikes are a bit more random. Finally, the step count around 8 PM is almost 0 on weekdays but around 100 on weekends, suggesting that this person stays in on weekday evenings but often goes on on weekend evenings.


