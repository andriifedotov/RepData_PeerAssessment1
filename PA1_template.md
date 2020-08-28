RepData_PeerAssessment1
=======================
Load files:
```{r,echo=FALSE}
unzip("activity.zip")
activity<-read.csv("activity.csv")

```

Transform dates to Date format:
```{r,echo=FALSE}
activity$date<-as.Date(activity$date,format = "%Y-%m-%d",optional = F)

```

**What is mean total number of steps taken per day?
Calculate total steps per day:
```{r,echo=FALSE}
library(dplyr)
total_steps_day<-activity %>% group_by(date) %>% summarise(steps = sum(steps))

```

Build histogram for total steps per day:
```{r,echo=FALSE}
hist(total_steps_day$steps)

```
Mean of steps:
```{r,echo=FALSE}
mean(total_steps_day$steps,na.rm = T)

```
Median of steps:
```{r,echo=FALSE}
median(total_steps_day$steps,na.rm = T)

```
**What is the average daily activity pattern?
Calculate steps per interval:
```{r,echo=FALSE}
steps_per_interval <- activity %>% group_by(interval) %>% summarise(steps = mean(steps, na.rm=TRUE))

```
Plot time series:
```{r,echo=FALSE}
plot(steps_per_interval$interval,steps_per_interval$steps,type="l")

```
Interval with max. steps:
```{r,echo = FALSE}
steps_per_interval[which.max(steps_per_interval$steps),]

```

**Imputing missing values

Compute the number of missing values in the dataset:
```{r,echo=FALSE}
number_of_nas  <-sum(!complete.cases(activity))
number_of_nas
```
A strategy for filling in the missing values:
Fill the NA value with the mean value of steps across all days

Create a new data set with missing values filled in:

```{r,echo=FALSE}

imputed_steps<-steps_per_interval$steps[match(activity$interval, steps_per_interval$interval)]
activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))


```
Plot a histogram of the total number of steps per day
```{r, echo=FALSE}

steps_per_day2<-activity_imputed%>%group_by(date) %>% summarise(steps = sum(steps))
hist(steps_per_day2$steps)

```
Mean:
```{r}
mean(steps_per_day2$steps)

```
Median:
```{r}
median(steps_per_day2$steps)

```

**Are there differences in activity patterns between weekdays and weekends?
Compute the average weekday and weekend activity patterns

```{r}
library(ggplot2)
activity$datetype <- sapply(activity$date, function(x) {
        if (weekdays(x) == "Sábado" | weekdays(x) =="Domingo") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })

activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```



