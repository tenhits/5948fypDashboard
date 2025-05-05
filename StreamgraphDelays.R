#note1: need to add titles to these graphs!! (DONE.)
#note2: change colours to make more colourblind friendly
#note3: change airport codes to actually say the name and codes of the airports (DONE.)
#note4: the y-axis isn't actually accurate at all, since the plot starts with the middle being zero and going outwards from that = 1

#installing and loading packages
#source to install devtools and streamgraphs respectively: https://www.displayr.com/installing-r-packages-from-github/, https://hrbrmstr.github.io/streamgraph/
install.packages("devtools")
library(devtools)

install.packages("nycflights13")
library(nycflights13) # origin of data

install.packages("lubridate") # to manipulate dates to facilitate plot making
library(lubridate)

install.packages("dplyr")
library(dplyr)

devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph) #to create the steamgraph plot

install.packages("grid")
library(grid)

install.packages("htmltools")
library(htmltools)  # For adding title to HTML output


# Join with airports to get full airport names
origin_names <- airports %>%
  select(faa, name)

#AVERAGE ARRIVAL DELAY STEAMGRAPH

# Prepare the data: average arrival delay per day by full airport name
daily_arr_delays <- flights %>%
  filter(!is.na(arr_delay), arr_delay >= 0) %>%
  mutate(date = as.Date(time_hour)) %>%
  group_by(date, origin) %>%
  summarise(avg_arr_delay = mean(arr_delay), .groups = "drop") %>%
  left_join(origin_names, by = c("origin" = "faa")) %>%
  mutate(airport_label = paste(name, "(", origin, ")", sep = ""))  # Combine name and code

# Create the streamgraph using the combined airport name and code
sg_plot <- streamgraph(daily_arr_delays, key = "airport_label", value = "avg_arr_delay", date = "date") %>%
  sg_axis_x(tick_format = "%b %d") %>%
  sg_fill_brewer(palette = "Set3") %>%
  sg_legend(show = TRUE)

# Add title using HTML tools, changed title
html_title <- tags$h3("Average Arrival Delay per day throughout 2013 by Airport", style = "text-align: center; font-size: 24px; font-weight: bold; font-family: Arial;")

# Display the title and plot together
browsable(tagList(html_title, sg_plot))

#AVERAGE DEPARTURE DELAY STEAMGRAPH

# Prepare the data: average departure delay per day by full airport name

daily_dep_delays <- flights %>%
  filter(!is.na(dep_delay), dep_delay >= 0) %>%
  mutate(date = as.Date(time_hour)) %>%
  group_by(date, origin) %>%
  summarise(avg_dep_delay = mean(dep_delay), .groups = "drop") %>%
  left_join(origin_names, by = c("origin" = "faa")) %>%
  mutate(airport_label = paste(name, "(", origin, ")", sep = ""))  # Combine name and code

# Create the streamgraph using the combined airport name and code
sg_plot <- streamgraph(daily_dep_delays, key = "airport_label", value = "avg_dep_delay", date = "date") %>%
  sg_axis_x(tick_format = "%b %d") %>%
  sg_fill_brewer(palette = "Set3") %>%
  sg_legend(show = TRUE)

# Add title using HTML tools, changed title
html_title <- tags$h3("Average Departure Delay per day throughout 2013 by Airport", style = "text-align: center; font-size: 24px; font-weight: bold; font-family: Arial;")

# Display the title and plot together
browsable(tagList(html_title, sg_plot))

#INSIGHTS -- ARRIVAL
#created the graph initially with delays under zero minutes but the delays were overlapping and it was difficult to read.
#instead, here's the graph but with only average delays above or equal to 0 mins, it shows that the JFK airport has the least variance in delay times and that the EWR and LGA airports are much more likely to be volatile throughout the year.
#There is a peak in delays in early March for both EWR and LGA, which JFK does not share?
#Late June to mid August looks to be a peak for all three airlines with early September being another peak for delays.
#Another smaller peak occurs at the beginning of December, Christmas?
#Late October to Mid November is the quietest time for arrival delays above zero.
#Late November also has a smaller and shorter peak around Thanksgiving time.
#would be interested to see if the flight delay increases around after 1st of January 2014, since people would assumedly be going back to work
#since the delays go down after 1st of December, seems that people stay with their families throughout the Christmas holiday into the new year instead of only Christmas and Christmas Eve

#INSIGHTS -- DEPARTURE

#departure delays above or equal to 0 mins actually seems to have less variance over all, though the trends from the arrival plot still remain
#higher peaks in March and September for departure delays, though delay peaks in the summer are actually lower than arrival
#departure delay peak in early December has a higher maximum delay than average arrival delays