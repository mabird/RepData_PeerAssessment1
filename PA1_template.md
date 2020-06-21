---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

&nbsp;

## Loading and preprocessing the data 

**1. Load the data and the required packages:** 



```r
data <- read.csv("activity.csv")
library(tidyverse)
```

&nbsp;

**2. Prepocess the data: calculate total number of steps per day** 



```r
day_total1 <- data %>% 
    group_by(date) %>% 
    summarise(total = sum(steps, na.rm = TRUE)) %>% 
    mutate(NA_status = "NA_present")
```

&nbsp;

## What is mean total number of steps taken per day?


**1. Total number of steps taken each day:**


```r
ggplot(day_total1, aes(date, total)) +
    geom_bar(stat = "identity", fill = "steelblue3") +
    labs(y = "steps", title = "Total Steps per Day") +
    theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5, face = "bold"))
```

![](PA1_template_files/figure-html/total steps histogram-1.png)<!-- -->


**2. Mean and median total number of steps taken per day**



```r
mean_steps1 <- mean(day_total1$total, na.rm = TRUE)
median_steps1 <- median(day_total1$total, na.rm = TRUE)
```

Mean number of steps taken per day is **9354** and median is **10395**

&nbsp;

## What is the average daily activity pattern?


**1. Average number of steps taken per interval**



```r
interval_av <- data %>% 
    group_by(interval) %>% 
    summarise(interval_steps = mean(steps, na.rm = TRUE))

with(interval_av, plot(interval, interval_steps, type = "l", ylab = "steps", main = "Average number of steps per time interval"))
```

![](PA1_template_files/figure-html/average activity pattern-1.png)<!-- -->

```r
max_interval <- unlist(interval_av[interval_av$interval_steps == max(interval_av$interval_steps), "interval"])
```

The interval with maximum average number of steps is **835**

&nbsp;

## Imputing missing values


**1. Calculate total number of missing values in the dataset** 



```r
nas <- colSums(is.na(data))
nas <- t(as.data.frame(nas))
rownames(nas) <- NULL

knitr::kable(nas, format = "html", table.attr = "style='width:30%;'")
```

<table style='width:30%;'>
 <thead>
  <tr>
   <th style="text-align:right;"> steps </th>
   <th style="text-align:right;"> date </th>
   <th style="text-align:right;"> interval </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2304 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

&nbsp;

**2. Fill in the missing values in the dataset**  


NAs in the data set were replaced with the mean value of the correspoding interval


```r
data_na_rm <- data
for(i in 1:nrow(data_na_rm)) {
    if (is.na(data_na_rm[i, "steps"])) {
        data_na_rm[i, "steps"] <- interval_av[interval_av$interval == data_na_rm[i, "interval"], "interval_steps"]
    }
}
```

**3. Compare total number of steps taken each day in data with NAs present and NAs imputed**  


```r
day_total2 <- data_na_rm %>% 
    group_by(date) %>% 
    summarise(total = sum(steps, na.rm = TRUE)) %>% 
    mutate(NA_status = "NA_imputed")

mean_steps2 <- round(mean(day_total2$total), 0)
median_steps2 <- round(median(day_total2$total), 0)

day_total <- rbind(day_total1, day_total2)
ggplot(day_total, aes(date, total)) +
    geom_bar(aes(fill = NA_status), stat = "identity", position = "dodge") +
    theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5, face = "bold")) +
    labs(y = "steps", title = "Total Number of Steps per Day")
```

![](PA1_template_files/figure-html/compare data with NA present and imputed-1.png)<!-- -->

Mean number of steps taken per day is **10766** and median is **10766**.
These numbers are higher than the mean and median calculated without imputing missing values.

&nbsp;


## Are there differences in activity patterns between weekdays and weekends?  

**1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day**  


```r
data_na_rm$date <- as.Date(data_na_rm$date, "%Y-%m-%d")
data_na_rm$weekdays <- weekdays(data_na_rm$date)
data_na_rm$days <- ifelse(data_na_rm$weekdays %in% c("Saturday", "Sunday"), "weekend", "weekday")
data_na_rm_week <- data_na_rm %>% 
    group_by(days, interval) %>% 
    summarise(total = mean(steps))

data_na_rm_week$days <- as.factor(data_na_rm_week$days)
```

**2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)**  


```r
ggplot(data_na_rm_week, aes(interval, total)) +
    geom_line(color = "steelblue3") +
    facet_grid(days~.) +
    labs(y = "steps", title = "Total Number of Steps in Weekdays and Weekends") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

![](PA1_template_files/figure-html/plot of weekday activity-1.png)<!-- -->

