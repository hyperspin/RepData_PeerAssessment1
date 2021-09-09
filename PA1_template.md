---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## 1. Loading and preprocessing the data

- Activity data is downloaded as a zip file from the following source:

"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"


```r
setwd("/Users/kumarnv/Rprojects/reproducibledata/project1")
activityUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

setwd("/Users/kumarnv/Rprojects/reproducibledata/project1")
zipFile <- "activity.zip"
activityFile <- "./activity.csv"

if (!file.exists("./data")) {
  dir.create("./data")
}

if (!file.exists(zipFile)) {
  download.file(activityUrl, destfile = zipFile, method = "curl")
}
activityData <- read.csv(activityFile)
```

## 2. Histogram of  total number of steps taken each day

```r
stepsDaily <- aggregate(. ~ date, data = activityData, sum)$steps
hist(stepsDaily, breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## 3. What is mean total number of steps taken per day?

```r
print(paste("daily steps mean", round(mean(stepsDaily))))
```

```
## [1] "daily steps mean 10766"
```

```r
print(paste("daily steps median", round(median(stepsDaily))))
```

```
## [1] "daily steps median 10765"
```


## 4. What is the average daily activity pattern?


```r
plot(stepsDaily, type ="l", xlab = "Day#", main = "Daily activity pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
stepsByInterval <- aggregate(steps ~ interval, activityData, mean)
plot(stepsByInterval, type="l", main= "Daily steps pattern by interval", ylab = "steps  mean")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
## 5-miniute interval with max number of steps- without imputation

```r
activityMax <- max(stepsByInterval$steps)
# interval corresponding to the max number of steps
intervalMax <- stepsByInterval[stepsByInterval$steps == activityMax, 1]
print(paste("Interval corresponding to maximum activity", intervalMax))
```

```
## [1] "Interval corresponding to maximum activity 835"
```

## 6. Imputing missing values

- Used Misc package to impute missing values with mean number of steps


```r
#install.packages("Hmisc")
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```r
totalNa <- sum(!complete.cases(activityData))
totalNa
```

```
## [1] 2304
```

```r
activityDataImp <- activityData
activityDataImp$steps <- with(activityData, impute(steps, mean))
activityDataImp$steps <- round(activityDataImp$steps)
stepsDailyImp <- aggregate(. ~ date, data = activityDataImp, sum)$steps
print(paste("daily mean", round(mean(stepsDailyImp))))
```

```
## [1] "daily mean 10752"
```

```r
#mean(stepsDailyImp)
#median((stepsDailyImp))
#plot(stepsDailyImp, type = "l", xlab = "day#", main = "Daily activity after data imputation")
```
## 7. Histogram of the total number of steps taken daily after data imputation. 


```r
hist(stepsDailyImp, breaks = 10, main = "daily steps after data imputation", xlab = "Number of steps taken daily")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## 8. Are there differences in activity patterns between weekdays and weekends?


```r
par(mfrow = c(2,1))
hist(stepsDaily, breaks = 10, main = "daily steps before data imputation", xlab = "Number of steps taken daily")
hist(stepsDailyImp, breaks = 10, main = "daily steps after data imputation", xlab = "Number of steps taken daily")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
- Differences between weekday and weekends aren't obvious from histograms

## 8. Panel plot comparing average number of steps taken per 5-minute interval.


```r
par(mfrow = c(2,1))
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:Hmisc':
## 
##     src, summarize
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

```r
library(lattice)
# Impute steps in activitydata
activityDataImp <- activityData
activityDataImp$steps <- with(activityData, impute(steps, mean))
activityDataImp$steps <- round(activityDataImp$steps)
imputedData <- mutate(activityDataImp, weekdays(as.Date(activityDataImp$date)))
colnames(imputedData)[4] <- "day"

imputedData$Daytype <- factor(imputedData$day, levels = c("Monday", "Tuesday","Wednesday", "Thursday", "Friday","Saturday","Sunday"), labels = c("weekday", "weekday", "weekday", "weekday", "weekday", "weekend","weekend"))
aggregateImputedData <- aggregate(steps~interval+Daytype, imputedData, mean)
xyplot(steps~interval|Daytype,
       data=aggregateImputedData,
       main="Interval Averages, Weekday v. Weekend",
       xlab="Interval",
       layout=c(1,2),
       type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
- daily activity pattern on weekdays appears to be different  from that on weekends.
