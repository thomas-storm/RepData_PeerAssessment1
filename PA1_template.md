Reproducible Research: Peer Assessment 1
================
Thomas Storm
2024-02-12

## Set knitr options

Set knitr options to not show warnings and messages to make the document
less verbose.

``` r
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
```

## Loading required packages

Data processing will be done with dplyr and lubridate, therefore loading
the tidyverse package. Plot are done with lattice package.

``` r
library(tidyverse)
library(lattice)
```

## Loading and preprocessing the data

### Download data

Data are downloaded using link provided in instructions. Code checks
whether data sub-directory exists, and if not, downloads and unzips file
in data directory.

``` r
main_dir <- getwd()
sub_dir <- "data"

## check if "data" sub-directory exists, if not download and unzip files in data folder
if (!dir.exists(sub_dir)){
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  temp <- tempfile()
  dir.create(paste(main_dir, sub_dir, sep ="/"))
  download.file(url, temp)
  unzip(zipfile = temp, exdir = file.path(paste(main_dir, sub_dir, sep ="/")))
  unlink(c(temp))
  rm(temp)
} else {print("data available")}
```

    ## [1] "data available"

Data are downloaded and in the data folder.

### Read data

Read data with read.csv

``` r
activity <- read.csv("./data/activity.csv", header = TRUE)

str(activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

### Pre-process data

Data pre-processing to mutate date from chr to date. The rest of the
data processing will be done specific for each question in a pipe.

``` r
activity <- activity %>% 
                mutate(date = ymd(date))

str(activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

## What is mean total number of steps taken per day?

Steps are summed up per date and displayed in a histogram. Missing
values are not removed, and show up in the first bin.

``` r
steps.day <- activity %>%
                group_by(date) %>% 
                summarize(sum = sum(steps, na.rm = TRUE))

histogram(~sum, data = steps.day,
                main = "Histogram of total steps per day",
                xlab = "Steps per day",
                breaks = 12)
```

![](PA1_template_files/figure-gfm/activty%20per%20day-1.png)<!-- -->

``` r
summary(steps.day$sum)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0    6778   10395    9354   12811   21194

The mean total number of steps taken per day is **mean = 9354**, while
the median total number of steps taken per day is **median = 10395**.
The distribution is a bit skewed by the missing values (i.e. the NAs).

## What is the average daily activity pattern?

The steps are grouped by interval and then summarized as mean.

``` r
daily.activity <- activity %>% 
                        group_by(interval) %>%
                        summarize(mean = mean(steps, na.rm = TRUE))

xyplot(mean ~ interval, 
                                data = daily.activity,
                                type = "l",
                                main = "Average daily activity pattern",
                                xlab = "Time interval",
                                ylab = "Average steps per Time Interval",
                                scales = list(x=list(
                                        at = c(0, 600, 1200, 1800, 2400),
                                        labels = c("0:00", "6:00", "12:00", "18:00", "24:00")))
)
```

![](PA1_template_files/figure-gfm/daily.activity-1.png)<!-- -->

``` r
daily.activity$interval[daily.activity$mean == max(daily.activity$mean)]
```

    ## [1] 835

There is a distinct activity patter, with low activity during the night,
an activity peak at **interval = 835**, indicating a repeating activity
pattern in the morning (e.g. walking to the office or a regular sports
activity in the morning). Activity remains at an elevated level until it
drops in the evenings after approximately 19:00 pm.

## Imputing missing values

The number of NAs in the file were calculated.

``` r
sum(is.na(activity))
```

    ## [1] 2304

The number of NAs is **Number NAs = 2304**. The NA values are all in the
steps column. The percentage of NAs was also calculated.

``` r
sum(is.na(activity$steps))/(sum(is.na(activity$steps)) + sum(!is.na(activity$steps)))*100
```

    ## [1] 13.11475

Overall, there are approximately **NAs \~ 13%** missing values in the
steps column.

Missing values (i.e. NAs) were imputed by calculating the mean number of
steps for each interval and replacing missing values in an interval with
the mean number of steps in the same interval. This should preserve the
main trends in the data.

``` r
steps.day.imputed <- activity %>% 
                        group_by(interval) %>% 
                        mutate(daily_mean = mean(steps, na.rm = TRUE)) %>%
                        mutate(steps.imputed = coalesce(steps, daily_mean)) %>%
                        group_by(date) %>% 
                        summarize(sum = sum(steps.imputed, na.rm = TRUE))

histogram(~sum, data = steps.day.imputed,
                main = "Histogram of total steps per day (NAs imputed)",
                xlab = "Steps per day",
                breaks = 12)
```

![](PA1_template_files/figure-gfm/imputation%20of%20missing%20values-1.png)<!-- -->

``` r
summary(steps.day.imputed$sum)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    9819   10766   10766   12811   21194

After imputation of NAs the mean total number of steps taken per day is
**mean = 10766**, and the median total number of steps taken per day is
also **median = 10766**. The mean and the median are identical,
indicating that the imputation of NAs effectively removed the skew in
the distribution.

## Are there differences in activity patterns between weekdays and weekends?

The daily activity pattern on working days (Mo - Fr) and on Weekeds (Sa,
Su) are compared.

``` r
weekend.weekday <- activity %>% 
                        group_by(interval) %>% 
                        mutate(daily_mean = mean(steps, na.rm = TRUE)) %>%
                        mutate(steps.imputed = coalesce(steps, daily_mean)) %>%
                        mutate(week.day = weekdays(date)) %>% 
                        mutate(weekend = week.day %in% c("Saturday", "Sunday")) %>%
                        group_by(interval, weekend) %>%
                        summarise(mean = mean(steps.imputed))

xyplot(mean ~ interval | weekend, 
                                data = weekend.weekday,
                                type = "l",
                                layout = c(1,2),
                                main = "Daily activity pattern",
                                xlab = "Time interval",
                                ylab = "Average steps per Time Interval",
                                strip = strip.custom(factor.levels = c("Working days", "Weekends")),
                                scales = list(x=list(
                                        at = c(0, 600, 1200, 1800, 2400),
                                        labels = c("0:00", "6:00", "12:00", "18:00", "24:00")))
)
```

![](PA1_template_files/figure-gfm/weekend-1.png)<!-- -->

There are different activity pattern on working days as compared to
weekends. The activity picks up later in the morning on weekends, and
the distinct activity peak in the morning is less pronounced on the
weekends as well. In general, activity on weekends has slightly higher
average activity throughout the day.
