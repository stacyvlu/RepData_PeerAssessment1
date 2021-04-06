---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
library("knitr")
require("ggplot2")
library("dplyr")
opts_chunk$set(echo = TRUE)
df <- as.data.frame(read.csv("activity.csv",na.strings = "NA"))
head(df)
totsteps_df <- aggregate(df$steps, list(activityDate = df$date), sum, na.rm=TRUE)
head(totsteps_df)

## What is mean total number of steps taken per day?
qplot1 <- qplot(x, data=totsteps_df, geom="histogram", xlab="Total Steps")
qplot1 <- qplot1 + stat_bin(binwidth=500,breaks=(seq(0,25000, by=500)),origin=0)
qplot1 <- qplot1 + ggtitle("Frequency of Steps per Day")
print(qplot1)
dev.copy(png, file = "meanplot.png")
dev.off()
mean(totsteps_df$x)
# 9354.23
median(totsteps_df$x)
# 10395

## What is the average daily activity pattern?
daily_df <- aggregate(df$steps, list(intervals = df$interval), sum, na.rm = TRUE)
head(daily_def)
qplot2 <- qplot(intervals, x, data = daily_df, geom = c("line", "point"), xlab = "Steps per Interval", ylab = "Total Steps")
qplot2 <- qplot2 + ggtitle("Frequency of Steps per Interval")
print(qplot2)
dev.copy(png, file = "avgdaily.png")
dev.off()
top <- head(daily_df[with(daily_df, order(-x, intervals)),],1)
top
# interval 835 has the most steps with 10927 steps

## Imputing missing values
miss_df <- subset(df,is.na(steps))
nrow(miss_df)
# 2304
mean_int <- aggregate(df$steps, list(intervals = df$interval), mean, na.rm=TRUE)
dim(mean_int)
head(mean_int)
new_df <- df
for(i in 1:nrow(new_df)){
  interval <- new_df$interval[i]
  steps <-  new_df$steps[i]
  if(is.na(steps)){
    mean_val <- subset(mean_int, mean_int$interval == interval)$x
    new_df$steps[i] <- mean_val
  }
}
new_df <- transform(new_df, steps = as.integer(steps))
head(new_df)
new_tot <- aggregate(new_df$steps, list(date = new_df$date), sum, na.rm=TRUE)
head(new_tot)
qplot3 <- qplot(x, data=new_tot, geom="histogram", xlab="Total Steps")
qplot3 <- qplot3 + stat_bin(binwidth = 500, boundary=0)
qplot3 <- qplot3 + ggtitle("Frequency of Steps per Day")
print(qplot3)
dev.copy(png, file = "freqsteps.png")
dev.off()
mean(new_tot$x)
# 10749.77
median(new_tot$x)
# 10641

## Are there differences in activity patterns between weekdays and weekends?
days_df <- new_df
days_df$day <- weekdays(as.Date(days_df$date))
head(days_df)
qplot4 <- qplot(interval,steps, data=days_df, facets= day~., geom="line", xlab="Interval", ylab="Total Steps")
qplot4 <- qplot4 + ggtitle("Frequency of Steps by Weekdays and Weekends")
print(qplot4)
dev.copy(png, file = "freqbyday.png")
dev.off()
