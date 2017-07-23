# QUESTION 1
## Load and process data
raw_data <- read.csv("C:/Users/Ranjan/Documents/Week 2 assignment/repdata%2Fdata%2Factivity/activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character", "integer"))
head(raw_data)
library(lubridate)
raw_data$date <- ymd(raw_data$date)
str(raw_data)
library(dplyr)
install.packages("deplyr")
## Total number of steps perday
total_steps_per_day <- raw_data %>% filter(!is.na(raw_data$steps)) %>% group_by(date) %>% summarize(steps = sum(steps)) %>% print
## Mean and median of total number of steps per day
mean_total_steps_per_day <- mean(total_steps_per_day$steps)
median_total_steps_per_day <- median(total_steps_per_day$steps)
mean_total_steps_per_day
median_total_steps_per_day
## Histogram of total number of steps taken per day
hist(total_steps_per_day$steps)

#Question 2
install.packages("deplyr")
## compute the mean steps per five minute interval
mean_steps_per_interval <- raw_data %>% filter(!is.na(raw_data$steps)) %>% group_by(interval) %>% summarize(steps = mean(steps)) %>% print
## Identify which five minute interval has the maximum steps
mean_steps_per_interval[which.max(mean_steps_per_interval$steps),]
## Plot a time series
library(ggplot2)
ggplot(mean_steps_per_interval, aes(x=mean_steps_per_interval$interval, y=steps)) + geom_line(color = "steelblue") + labs(x = expression("Mean Steps Per Interval"), y = "Steps")

#Question 3
## Calculate and report the total number of missing values
sum(is.na(raw_data$steps))
## Devise a strategy for filling in all of the missing values in the dataset
### Replace the missing with the grouped mean
data_duplicate <- raw_data
identify_missing <- is.na(data_duplicate$steps)
igonore_missing_steps_to_compute_mean <- tapply(data_duplicate$steps, data_duplicate$interval, mean, na.rm=TRUE, simplify=TRUE)
data_duplicate$steps[identify_missing] <- igonore_missing_steps_to_compute_mean[as.character(data_duplicate$interval[identify_missing])]
sum(is.na(data_duplicate$steps))
head(data_duplicate)
## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median
new_total_steps_per_day <- data_duplicate %>% group_by(date) %>% summarize(steps = sum(steps)) %>% print
## Mean and median of total number of steps per day
new_mean_total_steps_per_day <- mean(new_total_steps_per_day$steps)
new_median_total_steps_per_day <- median(new_total_steps_per_day$steps)
new_mean_total_steps_per_day
new_median_total_steps_per_day
### observe that mean and median are same (10766.19) - a strong case for normality
## Histogram of total number of steps taken per day
hist(new_total_steps_per_day$steps)
ggplot(new_total_steps_per_day, aes(x = steps)) +
        geom_histogram(fill = "steelblue", binwidth = 1000) +
        labs(title = "New Histogram", x = "Steps per day", y = "Frequency")

# Question 4
##Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
library(dplyr)
data_duplicate <- mutate(data_duplicate, weektype = ifelse(weekdays(data_duplicate$date) == "Saturday" | weekdays(data_duplicate$date) == "Sunday", "weekend", "weekday"))
head(data_duplicate)
data_duplicate$weektype <- as.factor(data_duplicate$weektype)
transform(data_duplicate, weektype = factor(weektype))
str(data_duplicate)


# Question 5
## Calculate the average steps in the 5-minute interval
head(data_duplicate)
library(dplyr)
new_average_steps <- data_duplicate %>% group_by(interval, weektype) %>% summarise(steps = mean(steps))
new_average_steps
library(ggplot2)
ggplot(new_average_steps, aes(x=interval, y=steps, color = weektype)) + geom_line() + facet_wrap(~weektype, ncol = 1, nrow=2) + theme_bw() + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) 
+ theme(strip.background = element_rect(fill="wheat"))
print(s)
## Clearly the weekend activites are slightly more consistent then the weekday activities.





