---
title: "Coursera's Reproducible Research Course Project 1"
author: "Norberto Hernandez"
date: "12/1/2021"
output: 
    html_document:
        keep_md: true
---



# Introduction

According with the Coursera's [Reproducible Research](https://www.coursera.org/learn/reproducible-research/home/welcome) "Course Project 1"  now we have the possibility to reach a large amount of data about personal movement using activity monitoring devices.  

These type of devices are part of a movement (who is called the **“quantified self”** movement) wants to take measurements about themselves regularly to:  

- improve their health.  
- find patterns in their behavior.  
- curiosity (because they are tech geeks).  

But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This exercise will address this problem.


# Loading and preprocessing the data  
## 1. Load the data  

First, we load the libraries necessaries for the analisys.


```r
library(ggplot2)
```

Read the dataset from the web <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>. The file is a compressed .zip file, and the data is stored as a comma sepparated values (CSV).  


```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
actdata <- read.csv(unz(temp, "activity.csv"), header = TRUE)
unlink(temp)
head(actdata)
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
As we can notice, the dataset contains variables:  

1. steps (the number of steps in a 5-minute interval).  
2. date (the date of the register).  
3. interval (the 5-minute interval measured by the activity device).  
 
## Process/transform the data (if necessary) into a format suitable for your analysis  

We notice that the variable `date` is a *character* type, so it will convert it into a *Date* type.  


```r
class(actdata$steps)
```

```
## [1] "integer"
```

```r
class(actdata$date)
```

```
## [1] "character"
```

```r
class(actdata$interval)
```

```
## [1] "integer"
```

```r
actdata$date <- as.Date(as.character(actdata$date), format = "%Y-%m-%d")
class(actdata$date)
```

```
## [1] "Date"
```
# What is mean total number of steps taken per day?  

For this part of the assignment, we do the following:  

## Calculate the total number of steps taken per day


```r
stepdays <- tapply(actdata$steps, actdata$date, sum)
head(stepdays)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420
```

## Make a histogram of the total number of steps taken each day  

![](PA1_template_files/figure-html/plot1-1.png)<!-- -->

The histogram suggest that the data may present an unimodal distribution, around 10,000 steps. We also notice that the dataaset has many `NaN` values (showed as empty cols in the histogram).  

## Calculate and report the mean and median of the total number of steps taken per day


```r
mean(stepdays, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(stepdays, na.rm = TRUE)
```

```
## [1] 10765
```

As we can notice, mean and median of the total number of steps taken per day are around 10,765.

# What is the average daily activity pattern?


```r
daily <- tapply(actdata$steps, actdata$interval, mean, na.rm = TRUE)
summary(daily, na.rm = TRUE)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.486  34.113  37.383  52.835 206.170
```

```r
head(daily)
```

```
##         0         5        10        15        20        25 
## 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396
```

![](PA1_template_files/figure-html/plot2-1.png)<!-- -->

The graph shows that most of the daily activity occurs between interval 500 and 1000 (aprox. between 8:00 AM to halfday). We also notice that before interval 5000 activity is low (probably because people are on sleep time).  

# Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as `NaN`). The presence of missing days may introduce bias into some calculations or summaries of the data.

## 1. Calculate and report the total number of missing values in the dataset.


```r
sum(is.na(actdata$step))
```

```
## [1] 2304
```

```r
sum(is.na(actdata$date))
```

```
## [1] 0
```

```r
sum(is.na(actdata$interval))
```

```
## [1] 0
```

## Devise a strategy for filling in all of the missing values in the dataset.  

The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

WE'll use the median for the interval for impute data.  


```r
step_median <- tapply(actdata$step, actdata$interval, median, na.rm = TRUE)
sum(is.na(step_median))
```

```
## [1] 0
```

## Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
actdata2 <- actdata
for(i in 1:nrow(actdata2)){
    if(is.na(actdata2$steps[i])) actdata2$steps[i] <- step_median[as.character(actdata2$interval[i])]}
sum(is.na(actdata2$steps))
```

```
## [1] 0
```

```r
sum(is.na(actdata$steps))
```

```
## [1] 2304
```

# Make a histogram of the total number of steps taken each day and  

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##       1141        126      11352      12116      13294      15420
```

![](PA1_template_files/figure-html/plot3-1.png)<!-- -->

## Calculate and report the mean and median total number of steps taken per day.


```r
#Dataframe with missings
mean(stepdays, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(stepdays, na.rm = TRUE)
```

```
## [1] 10765
```

```r
#dataframe without missings
mean(stepdays2)
```

```
## [1] 9503.869
```

```r
median(stepdays2)
```

```
## [1] 10395
```

As we noticed, mean and median differs when impute data.

#Are there differences in activity patterns between weekdays and weekends?

For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
actdata$dow <- weekdays(actdata$date)
actdata$weekend <- 0

for(i in 1:nrow(actdata)){
  if(actdata$dow[i] == 'sábado') actdata$weekend[i] <- 1
  if(actdata$dow[i] == 'domingo') actdata$weekend[i] <- 1}

tapply(actdata$weekend,actdata$dow, sum)
```

```
##   domingo    jueves     lunes    martes miércoles    sábado   viernes 
##      2304         0         0         0         0      2304         0
```

## Make a panel plot containing a time series plot (i.e. ype = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

![](PA1_template_files/figure-html/plot4-1.png)<!-- -->
