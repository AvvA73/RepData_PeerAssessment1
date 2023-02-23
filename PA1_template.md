---
title: "PA1_template"
output: html_document
date: "2023-02-21"
output: md_document
---

# Reproducible Research:  Project 1

## Loading libraries and data
```{r}
library(ggplot2)
library(dplyr)
activity <- read.csv("activity.csv")
```

## 1. What is mean total number of steps taken per day?
```{r}
stepsPerDay <- activity %>%
        group_by(date) %>%
        summarize(sumsteps = sum(steps, na.rm = TRUE)) 
hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps", 
     col="lightblue", xlab="Steps", ylim = c(0,30))

meanPreNA <- round(mean(stepsPerDay$sumsteps))
medianPreNA <- round(median(stepsPerDay$sumsteps))
print(paste("The mean is: ", meanPreNA))
print(paste("The median is: ", medianPreNA))
```

## 2. What is the average daily activity pattern?
```{r}
stepsPerInterval <- activity %>%
        group_by(interval) %>%
        summarize(meansteps = mean(steps, na.rm = TRUE))
plot(stepsPerInterval$meansteps ~ stepsPerInterval$interval,
     col="red", type="l", xlab = "5 Minute Intervals", ylab = "Average Number of Steps",
     main = "Steps By Time Interval")

print(paste("5-Minute Interval containing the most steps on average: ",stepsPerInterval$interval[which.max(stepsPerInterval$meansteps)]))
print(paste("Average steps for that interval: ",round(max(stepsPerInterval$meansteps))))
```

## 3. Imputing missing values
### - Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
print(paste("The total number of rows with NA is: ",sum(is.na(activity$steps))))
```

### - Devise a strategy for filling in all of the missing values in the dataset. 
### - Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
activityNoNA <- activity  
for (i in 1:nrow(activity)){
        if(is.na(activity$steps[i])){
                activityNoNA$steps[i]<- stepsPerInterval$meansteps[activityNoNA$interval[i] == stepsPerInterval$interval]
        }
}
```

### - Make a histogram of the total number of steps taken each day.
```{r}
stepsPerDay <- activityNoNA %>%
        group_by(date) %>%
        summarize(sumsteps = sum(steps, na.rm = TRUE)) 
hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps", 
     col="lightblue", xlab="Steps")

meanPostNA <- round(mean(stepsPerDay$sumsteps), digits = 2)
medianPostNA <- round(median(stepsPerDay$sumsteps), digits = 2)
print(paste("The mean is: ", mean(meanPostNA)))
print(paste("The median is: ", median(medianPostNA)))
```

### 4. Are there differences in activity patterns between weekdays and weekends?
```{r}
activityDoW <- activityNoNA
activityDoW$date <- as.Date(activityDoW$date)
activityDoW$day <- ifelse(weekdays(activityDoW$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityDoW$day <- as.factor(activityDoW$day)
activityWeekday <- filter(activityDoW, activityDoW$day == "weekday")
activityWeekend <- filter(activityDoW, activityDoW$day == "weekend")

activityWeekday <- activityWeekday %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
activityWeekday$day <- "weekday"

activityWeekend <- activityWeekend %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
activityWeekend$day <- "weekend"

wkdayWkend <- rbind(activityWeekday, activityWeekend)
wkdayWkend$day <- as.factor(wkdayWkend$day)


g <- ggplot (wkdayWkend, aes (interval, steps))
g + geom_line() + facet_grid (day~.) + 
        theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14)) + 
        labs(y = "Number of Steps") + labs(x = "Interval") + 
        ggtitle("Average Number of Steps: Weekday vs. Weekend") + 
        theme(plot.title = element_text(hjust = 0.5))
```

# END OF THE FILE
