# Load packages
library(nycflights13)
library(dplyr)
library(ggplot2)
library(scales)

# Step 1: Prepare data
flights_clean <- flights %>%
  filter(!is.na(arr_delay), !is.na(dep_delay), !is.na(sched_dep_time), !is.na(time_hour)) %>%
  mutate(
    total_delay = arr_delay + dep_delay,
    dep_hour = sched_dep_time %/% 100,
    time_of_day = case_when(
      dep_hour >= 0  & dep_hour < 6  ~ "Night",
      dep_hour >= 6  & dep_hour < 12 ~ "Morning",
      dep_hour >= 12 & dep_hour < 18 ~ "Afternoon",
      dep_hour >= 18 & dep_hour <= 23 ~ "Evening"
    ),
    day_of_week = weekdays(as.Date(time_hour))
  )

# Step 2: Remove outliers
remove_outliers <- function(x) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  x[x < (qnt[1] - H) | x > (qnt[2] + H)] <- NA
  return(x)
}

flights_clean <- flights_clean %>%
  mutate(total_delay_no_outliers = remove_outliers(total_delay)) %>%
  filter(!is.na(total_delay_no_outliers))

# Step 3: Order time and days
flights_clean$time_of_day <- factor(flights_clean$time_of_day,
                                    levels = c("Night", "Morning", "Afternoon", "Evening"))

flights_clean$day_of_week <- factor(flights_clean$day_of_week,
                                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                               "Friday", "Saturday", "Sunday"))

# Step 4: Summarize
delay_summary <- flights_clean %>%
  group_by(origin, day_of_week, time_of_day) %>%
  summarize(avg_delay = mean(total_delay_no_outliers), .groups = "drop")

# Step 5: Plot heatmap faceted by origin
ggplot(delay_summary, aes(x = time_of_day, y = day_of_week, fill = avg_delay)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "Avg Delay (min)"
  ) +
  labs(
    title = "Average Flight Delays by Origin, Day of Week, and Time of Day",
    x = "Time of Day", y = "Day of Week"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~origin)
