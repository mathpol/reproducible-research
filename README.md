# reproducible-research

Dataset: Activity monitoring data 
The variables included in this dataset are:
steps: Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
-----
##q1
#download
 library(dplyr)
library(lubridate)
library(ggplot2)
library(httpuv)
#load data
setwd("C:\\Users\\vantr\\OneDrive\\Documents\\R\\tu hoc\\Reproducible Research")
activity <- read.csv("activity.csv", header=TRUE, na.strings = "NA")
 activity$date <- ymd(activity$date)
 #remove NA
 activity1 <- na.omit(activity)
#create new variables
activity$monthly <- as.numeric(format(activity$date, "%m"))
 activity$daily <- as.numeric(format(activity$date, "%d"))
 #check 
summary(activity1 )
     steps             date               interval     
 Min.   :  0.00   Min.   :2012-10-02   Min.   :   0.0  
 1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
 Median :  0.00   Median :2012-10-29   Median :1177.5  
 Mean   : 37.38   Mean   :2012-10-30   Mean   :1177.5  
 3rd Qu.: 12.00   3rd Qu.:2012-11-16   3rd Qu.:1766.2  
 Max.   :806.00   Max.   :2012-11-29   Max.   :2355.0  
 str(activity1)
'data.frame':	15264 obs. of  3 variables:
 $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
 $ date    : Date, format: "2012-10-02" "2012-10-02" "2012-10-02" ...
 $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
 - attr(*, "na.action")= 'omit' Named int [1:2304] 1 2 3 4 5 6 7 8 9 10 ...
  ..- attr(*, "names")= chr [1:2304] "1" "2" "3" "4" ...
 #head
 head(activity1)
 
    steps       date interval
289     0 2012-10-02        0
290     0 2012-10-02        5
291     0 2012-10-02       10
292     0 2012-10-02       15
293     0 2012-10-02       20
294     0 2012-10-02       25
#tail
tail(activity1)
      steps       date interval
17275     0 2012-11-29     2330
17276     0 2012-11-29     2335
17277     0 2012-11-29     2340
17278     0 2012-11-29     2345
17279     0 2012-11-29     2350
17280     0 2012-11-29     2355
 ##q2
 #make histrogram
 activity2 <- summarize(group_by(activity1,date),daily.step=sum(steps))
`summarise()` ungrouping output (override with `.groups` argument)
 mean.activity <- as.integer(mean(activity2$daily.step))
median.activity <- as.integer(median(activity2$daily.step))
#plot
plot.steps.day <- ggplot(activity2, aes(x=daily.step)) + 
 geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..)) + 
 geom_vline(xintercept=mean.activity, colour="blue", linetype="dashed", size=1) +
geom_vline(xintercept=median.activity, colour="green" , linetype="dotted", size=1) +
labs(title="Histogram of Number of Steps taken each day", y="Frequency", x="Daily Steps") 
 plot.steps.day
 dev.copy(png)
png 
  4 
dev.off()
RStudioGD 
        2 
 #mean
 mean.activity
>[1] 10766
 #median
 median.activity
>[1] 10765
 ##q3
 activity3 <- activity1 %>% group_by(interval) %>% summarize(mean.step=mean(steps))
>`summarise()` ungrouping output (override with `.groups` argument)
plot.step.interval <- ggplot(activity3, aes(x=interval,y=mean.step)) + 
 geom_line(color="yellow") + 
 labs(title="Average Number of Steps Taken vs 5-min Interval", y="Average Number of Steps", x="5-min Interval Times Series")
plot.step.interval
dev.copy(png)
>png 
  4 
dev.off()
>RStudioGD 
        2 
dev.copy(png)
>png 
  4 
dev.off()
>RStudioGD 
        2 
 dev.copy(png,"plot.step.interval")
>png 
  4 
 dev.off()
>RStudioGD 
        2 
 #5-minute interval
 optimal <- which.max(activity3$mean.step)
 optimal.step <- activity3$interval[optimal]
sprintf("Maximum number of steps is coming from %gth 5-min interval", optimal.step)
[1] "Maximum number of steps is coming from 835th 5-min interval"
 ##q4
 #total number of missing values
sum(is.na(activity))
[1] 2304
imp.activity <- activity
 imp.activity$steps[is.na(imp.activity$steps)] <- mean(imp.activity$steps,na.rm=TRUE)
imp.activity$steps <- as.numeric(imp.activity$steps)
 imp.activity$interval <- as.numeric(imp.activity$interval)
colSums(is.na(imp.activity))
   steps     date interval  monthly    daily 
       0        0        0        0        0 
 #new dataset
summary(imp.activity)
     steps             date               interval         monthly          daily      
 Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Min.   :10.00   Min.   : 1.00  
 1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   1st Qu.:10.00   1st Qu.: 8.00  
 Median :  0.00   Median :2012-10-31   Median :1177.5   Median :10.00   Median :16.00  
 Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Mean   :10.49   Mean   :15.75  
 3rd Qu.: 37.38   3rd Qu.:2012-11-15   3rd Qu.:1766.2   3rd Qu.:11.00   3rd Qu.:23.00  
 Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Max.   :11.00   Max.   :31.00  
 #histogram
 imp.activity2 <- summarize(group_by(imp.activity,date),daily.step=sum(steps))
>`summarise()` ungrouping output (override with `.groups` argument)
 mean.imp   <- as.integer(mean(imp.activity2$daily.step))
 plot.steps.day <- ggplot(imp.activity2, aes(x=daily.step)) + 
+  geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..))  
+   geom_vline(xintercept=mean.imp, colour="pink", linetype="dashed", size=1) 
+   geom_vline(xintercept=median.imp, colour="blue" , linetype="dotted", size=1) 
+   labs(title="Histogram of Number of Steps taken each day (impute)", y="Frequency", x="Daily Steps")
 plot.steps.day
dev.copy(png,"plot.steps.day")
>png 
  4 
 dev.off()
>RStudioGD 
        2 
 ##q5
imp.activity$day <- ifelse(weekdays(imp.activity$date) %in% c("Saturday","Sunday"), "weekday", "weekend")
imp.df <- imp.activity %>% group_by(interval,day) %>% summarise(mean.step=mean(steps))
`summarise()` regrouping output by 'interval' (override with `.groups` argument)
 plot.weekday.interval <- ggplot(imp.df, aes(x=interval, y=mean.step, color=day)) 
+   facet_grid(day~.) 
+   geom_line() 
+   labs(title="Average Number of Steps Taken vs 5-min Interval on Weekday/Weekend", y="Average Number of Steps", x="5-min Interval Times Series")
plot.weekday.interval
dev.copy(png,"plot.weekday.interval")
>png 
  4 
dev.off()
>RStudioGD 
        2 
