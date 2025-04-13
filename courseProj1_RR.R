# Unzip and load the data
unzip("repdata_data_activity.zip")
activity_data <- read.csv("activity.csv")

# Preview the data
head(activity_data)
str(activity_data)


# Total steps per day (ignoring NA values)
total_steps_per_day <- aggregate(steps ~ date, data = activity_data, sum, na.rm = TRUE)

# Histogram
hist(total_steps_per_day$steps, main = "Total Steps Per Day", xlab = "Steps", col = "skyblue")

# Mean and Median
mean_steps <- mean(total_steps_per_day$steps)
median_steps <- median(total_steps_per_day$steps)
mean_steps
median_steps

# Average steps per interval
avg_steps_interval <- aggregate(steps ~ interval, data = activity_data, mean, na.rm = TRUE)

# Time series plot
plot(avg_steps_interval$interval, avg_steps_interval$steps, type = "l",
     main = "Average Daily Activity Pattern",
     xlab = "Interval", ylab = "Average Steps")

# Interval with max average steps
max_interval <- avg_steps_interval[which.max(avg_steps_interval$steps), ]
max_interval


# Number of missing values
num_missing <- sum(is.na(activity_data$steps))

# Impute using mean for that interval
impute_steps <- function(steps, interval) {
  if (is.na(steps)) {
    return(avg_steps_interval[avg_steps_interval$interval == interval, "steps"])
  } else {
    return(steps)
  }
}

activity_filled <- activity_data
activity_filled$steps <- mapply(impute_steps, activity_filled$steps, activity_filled$interval)


total_steps_filled <- aggregate(steps ~ date, data = activity_filled, sum)
hist(total_steps_filled$steps, main = "Total Steps Per Day (Imputed Data)", xlab = "Steps", col = "lightgreen")

# Mean and Median
mean_filled <- mean(total_steps_filled$steps)
median_filled <- median(total_steps_filled$steps)
mean_filled
median_filled


# Label weekday/weekend
activity_filled$date <- as.Date(activity_filled$date)
activity_filled$day_type <- ifelse(weekdays(activity_filled$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activity_filled$day_type <- as.factor(activity_filled$day_type)

# Average steps by interval and day type
avg_steps_day_type <- aggregate(steps ~ interval + day_type, data = activity_filled, mean)

# Panel plot using ggplot2
library(ggplot2)
ggplot(avg_steps_day_type, aes(x = interval, y = steps)) +
  geom_line() +
  facet_wrap(~ day_type, nrow = 2) +
  labs(title = "Average Steps per Interval: Weekday vs Weekend",
       x = "5-minute Interval", y = "Average Steps")


library(knitr)
knit2html("PA1_template.Rmd")
