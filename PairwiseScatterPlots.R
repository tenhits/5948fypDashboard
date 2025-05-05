#Pairwise Scatter Plots
#notes:
#is this easy to read? NO.
#is this easy to understand even after explanation? not really. 
#is it a quick summary? YES.
#does this warrant use? Probably not.
#am i gonna use it anyway? probably.

#it works out nicely since it is possible to create this plot in base R, without the need of any packages.
#saw this in tutorial 5? 
#add arrival delays (DONE)

# Select the relevant columns from the flights dataset
flights_subset <- flights[, c("distance", "air_time", "dep_delay", "arr_delay", "day")]

# Set up the plotting area to hold 2 rows and 2 columns
par(mfrow = c(2, 2))

# Scatter plot: Distance vs Air Time
plot(flights_subset$distance, flights_subset$air_time, 
     main = "Distance vs Air Time", 
     xlab = "Distance", 
     ylab = "Air Time", 
     pch = 20, col = rgb(0.1, 0.2, 0.5, 0.5))

# Scatter plot: Departure Delay vs Arrival Delay
plot(flights_subset$dep_delay, flights_subset$arr_delay, 
     main = "Departure Delay vs Arrival Delay", 
     xlab = "Departure Delay", ylab = "Arrival Delay", 
     pch = 20, col = rgb(0.8, 0.3, 0.3, 0.5))

# Scatter plot: Day vs Distance
plot(flights_subset$day, flights_subset$distance, 
     main = "Day vs Distance", 
     xlab = "Day", 
     ylab = "Distance", 
     pch = 20, col = rgb(0.3, 0.5, 0.7, 0.6))

# Pairwise scatter plot matrix
pairs(flights_subset[, c("distance", "air_time", "dep_delay", "arr_delay", "day")], 
      main = "Pairwise Scatter Plots", 
      pch = 20, col = rgb(0.3, 0.6, 0.4, 0.4))