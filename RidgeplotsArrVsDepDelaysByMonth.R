#notes: what on earth is up with these distributions? 
#some have double peaks, some are normally distributed, some look like they need transforming using exponentials 
#average delays in a month are usually shown through horizontal bar charts or line charts, but its usually in a delay averages per month, not showing the daily averages over a month
#this did not provide the level of detail i wanted, with a ridge plot i can see the distributions over each month as well, giving a view into how delays change over months for both arrival and departure delays
#example of the above (add more): https://github.com/manishkr1754/Airline_Delay_Dashboard_PowerBI
#slide 6, lecture 5 -- exploratory data analysis for distributions and implications
#i got rid of the outliers because it split the distributions into two instead of being one solid block, plus in the comparison plot, it would take up a lot of space for not a lot of benefit. This reduction increases the aesthetic appeal of the plot and narrows it all down to key info.

#why did i use a ridgeplot? 
#I saw it on this website here:
#This website shows an example where it showed temperatures over a year in months, which confirmed to me that I could use the plot in the way I envisioned. https://r-graph-gallery.com/294-basic-ridgeline-plot.html
#It is not commonly used for flight visualisations (I saw it in none of the actual dashboards/visalisatons that I researched), although I believe it should be, it makes the delays over time very easy to compare and shows the distributions over time as well. In addition it could be split up into smaller plots as small multiples to put next to a month's summary statistics so that someone would be able to see how all months are doing all at once without much interpretation required.

#why did I use ggridges?
#the website where I saw the example ridge plots also had an example using ggridges that looked perfect aesthetically, plus I could ensure the compatibility of the package, since it is an extension of ggplot2 and thus uses the same syntax (source: https://r-graph-gallery.com/294-basic-ridgeline-plot.html#color) 

install.packages("nycflights13")
install.packages("ggplot2")
install.packages("ggridges")
install.packages("dplyr")

library(nycflights13)
library(ggplot2)
library(ggridges)
library(dplyr)

#Helper function to remove outliers
remove_outliers <- function(df, delay_col) {
  Q1 <- quantile(df[[delay_col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[delay_col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  upper <- Q3 + 1.5 * IQR_val
  lower <- Q1 - 1.5 * IQR_val
  df %>% filter(.data[[delay_col]] >= lower & .data[[delay_col]] <= upper)
}

# Compute average daily arrival delays
arr_delays <- flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(month, day) %>%
  summarise(delay = mean(arr_delay), .groups = "drop") %>%
  mutate(type = "Arrival")

# Compute average daily departure delays
dep_delays <- flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(month, day) %>%
  summarise(delay = mean(dep_delay), .groups = "drop") %>%
  mutate(type = "Departure")

# Combine and clean
all_delays <- bind_rows(arr_delays, dep_delays) %>%
  remove_outliers("delay")

# Convert month to ordered factor
all_delays$month <- factor(all_delays$month, levels = 1:12, labels = month.name)
all_delays$type <- factor(all_delays$type, levels = c("Arrival", "Departure"))

# Plot side-by-side facet
ggplot(all_delays, aes(x = delay, y = month, fill = month)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.8) +
  facet_wrap(~ type, nrow = 1) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(title = "Arrival vs. Departure Delays by Month (Outliers Removed)",
       x = "Average Daily Delay (minutes)",
       y = "Month")
