---
title: "Course Project 1"
author: "Jerry Harber"
output: html_document
---


### Date Mon May  22  10:43:05 AM 2017  

# Introduction  
## This assignment makes use of data from a personal activity monitoring device.
This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day  

## The variables included in this dataset are:
-- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
- date: The date on which the measurement was taken in YYYY-MM-DD format  
- interval: Identifier for the 5-minute interval in which measurement was taken  

## The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset

## 1. Code for reading in the dataset and/or processing the data. Also make a dataset of only complete cases for later use.

```r
act <- read.csv("activity.csv")
act_cc <- act[complete.cases(act), ]
```
## 2.	Histogram of the total number of steps taken each day

```r
p1 <- aggregate(act[,"steps"],by=list(act$date),FUN=sum)
p1 <- rename(p1, Date = Group.1)
p1 <- rename(p1, Total.Steps = x)
p1[1:5,]
```

```
##         Date Total.Steps
## 1 2012-10-01          NA
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
```

```r
hist(p1$Total.Steps, xlab = "Total number of steps", main = paste("Histogram of total number of steps taken per day"))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
dev.off()
```

```
## null device 
##           1
```
# See plot1.png

## 3.	Mean and median number of steps taken each day. Exclude values that are NA.

```r
mean(as.numeric(act$steps), na.rm = TRUE)
```

```
## [1] 37.3826
```

```r
median(as.numeric(act$steps), na.rm = TRUE)
```

```
## [1] 0
```

## 4. Time series plot of the average number of steps.  
## Number of rows which have NA values are: 

```r
nrow(act) - nrow(act_cc)
```

```
## [1] 2304
```
## I will use the dataset with complete cases (i.e., act_cc) for further analysis.

```r
p2 <- aggregate(act_cc[,"steps"],by=list(act_cc$interval),FUN=mean)
p2 <- rename(p2, Time.Interval = Group.1)
p2 <- rename(p2, Mean.Steps = x)
plot(p2, type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
dev.off()
```

```
## null device 
##           1
```
# See plot2.png

## Determine the time interval for the maximum for the average number of steps. The interval with maximum number of steps is:  835 

```r
a <- p2[order(p2$Mean.Steps), ]
a[nrow(a),]
```

```
##     Time.Interval Mean.Steps
## 104           835   206.1698
```
## Add a new column to the act data.frame for type of date , i.e., weekday or weekend

```r
act_cc["DayOfWeek"] <- weekdays(as.Date(act_cc$date), abbreviate = TRUE)
act_cc$DayOfWeek[act_cc$DayOfWeek != "Sun" & act_cc$DayOfWeek != "Sat"] <- "weekday"
act_cc$DayOfWeek[act_cc$DayOfWeek == "Sun" | act_cc$DayOfWeek == "Sat"] <- "weekend"
act_cc$DayOfWeek <- as.factor(act_cc$DayOfWeek)
p2 <- aggregate(act_cc[,"steps"],by=list(act_cc$DayOfWeek, act_cc$interval),FUN=mean)
p2 <- rename(p2, Day.Of.Week = Group.1)
p2 <- rename(p2, Interval = Group.2)
p2 <- rename(p2, Mean.Steps = x)
xyplot(Mean.Steps ~ Interval | Day.Of.Week, data = p2, type = "l", layout = c(2, 1), main = "Comparison of time intervals by day of the week")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
dev.off()
```

```
## null device 
##           1
```
# See plot3.png

