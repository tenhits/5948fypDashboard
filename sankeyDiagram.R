#notes: this might already account for daylight savings (i just wasted like 3 hours on this)
#remake with state function from flow maps
#UPDATE: BAD IDEA!! it was more just so much harder to read, with the time zones, its at least easier to read and you can have an approximate idea of where these flights are going. 
#add that key!!!!!


# Load libraries
library(nycflights13)
library(dplyr)
library(plotly)

# Prepare airport metadata: full name and timezone
airports_meta <- airports %>% 
  select(faa, name, tz)

# Join origin airport names
flights_named <- flights %>%
  left_join(airports_meta, by = c("origin" = "faa")) %>%
  rename(origin_name = name, origin_tz = tz)

# Join destination airport timezones
flights_named <- flights_named %>%
  left_join(airports_meta, by = c("dest" = "faa")) %>%
  rename(dest_name = name, dest_tz = tz)

# Optional: Label timezones (for readability)
tz_labels <- c(
  "-10" = "Hawaii",
  "-9" = "Alaska",
  "-8" = "Pacific",
  "-7" = "Mountain",
  "-6" = "Central",
  "-5" = "Eastern",
  "-4" = "Atlantic"
)

flights_named$dest_tz_label <- tz_labels[as.character(flights_named$dest_tz)]

# Count flights from origin airport to destination timezone
route_counts <- flights_named %>%
  filter(!is.na(origin_name), !is.na(dest_tz_label)) %>%
  count(origin_name, dest_tz_label)

# Create unique nodes: origin airport names + destination timezones
nodes <- data.frame(name = unique(c(route_counts$origin_name, route_counts$dest_tz_label)))

# Match names to indices
route_counts <- route_counts %>%
  mutate(
    source = match(origin_name, nodes$name) - 1,
    target = match(dest_tz_label, nodes$name) - 1
  )

# Build Sankey diagram
fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = nodes$name,
    pad = 15,
    thickness = 20,
    line = list(color = "black", width = 0.5)
  ),
  link = list(
    source = route_counts$source,
    target = route_counts$target,
    value = route_counts$n
  )
)

fig <- fig %>% layout(title = "NYC Flights: Origin Airports → Destination Timezones", font = list(size = 10))
fig
