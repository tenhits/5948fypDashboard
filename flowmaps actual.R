# Load libraries
install.packages("ggrepel")
install.packages("sf")
install.packages("ggplot2")
install.packages("dpylr")

library(ggrepel)
library(nycflights13)
library(dplyr)
library(ggplot2)
library(sf)
library(maps)

sf::sf_use_s2(FALSE)

# Step 1: Filter airports with lat/lon
airport_locs <- airports %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  select(faa, lat, lon)

# Step 2: Convert airports to sf points
airport_sf <- st_as_sf(airport_locs, coords = c("lon", "lat"), crs = 4326)

# Step 3: US state polygons as sf
us_states <- map("state", plot = FALSE, fill = TRUE)
us_states_sf <- st_as_sf(us_states)

# Step 4: Ensure CRS matches
airport_sf <- st_transform(airport_sf, crs = st_crs(us_states_sf))

# Step 5: Spatial join to get state for each airport
airport_states <- st_join(airport_sf, us_states_sf, join = st_within) %>%
  mutate(state = ID) %>%
  select(faa, state)

# Step 6: Merge flights with state info
flights_with_state <- flights %>%
  inner_join(airport_states, by = c("dest" = "faa")) %>%
  filter(origin %in% c("JFK", "LGA", "EWR")) %>%
  group_by(origin, state) %>%
  summarise(count = n(), .groups = "drop")

# Step 7: NYC airport coordinates
origin_coords <- airports %>%
  filter(faa %in% c("JFK", "LGA", "EWR")) %>%
  select(origin = faa, origin_lat = lat, origin_lon = lon)

# Step 8: State centroids
state_centroids <- us_states_sf %>%
  group_by(ID) %>%
  summarise(geom = st_centroid(st_union(geom))) %>%
  rename(state = ID) %>%
  mutate(state = tolower(state)) %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(state = us_states_sf$ID)

colnames(state_centroids) <- c("state_lon", "state_lat", "state")

# Step 9: Combine all for plotting
flights_geo <- flights_with_state %>%
  left_join(origin_coords, by = "origin") %>%
  left_join(state_centroids, by = "state") %>%
  filter(!is.na(origin_lon), !is.na(origin_lat),
         !is.na(state_lon), !is.na(state_lat),
         origin_lon != state_lon | origin_lat != state_lat)

# Step 10: US map for background
us_map <- map_data("state")

# ---- Plotting Function ----
plot_origin_map <- function(airport_code) {
  flights_subset <- filter(flights_geo, origin == airport_code)
  
  # Custom color gradients for JFK and LGA (with yellow for JFK and pink for LGA)
  color_palette <- switch(airport_code,
                          "JFK" = scale_color_gradientn(
                            colors = c("#648FFF","#567BDB","#4664B3","#364E8A","#263761"),
                            name = paste(airport_code, "Flight Count")
                          ),
                          "LGA" = scale_color_gradientn(
                            colors = c("#DC267F", "#B81F69", "#8F1852", "#66113A", "#3D0A23"),
                            name = paste(airport_code, "Flight Count")
                          ),
                          "EWR" = scale_color_gradientn(
                            colors = c("#FFB000", "#DB9600","#B37A00", "#8A5E00","#614200"),
                            name = paste(airport_code, "Flight Count")
                          ),
                          c("gray80", "gray60", "gray40", "gray20"))
  
  used_states <- unique(flights_subset$state)
  labels_data <- filter(state_centroids, state %in% used_states)
  
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
                 fill = "gray95", color = "white") +
    geom_curve(data = flights_subset,
               aes(x = origin_lon, y = origin_lat,
                   xend = state_lon, yend = state_lat,
                   color = count, size = count),
               curvature = 0.3, alpha = 0.6) +
    color_palette +
    scale_size(range = c(0.3, 2.5), name = "Flight Volume") +
    geom_text_repel(data = labels_data,
                    aes(x = state_lon, y = state_lat, label = tools::toTitleCase(state)),
                    size = 3, color = "black", fontface = "bold",
                    box.padding = 0.5, segment.color = "grey50", segment.size = 0.4,
                    direction = "both", nudge_y = 2, nudge_x = 2) +
    labs(title = paste("Flight Flows from", airport_code),
         subtitle = "Line color = Number of flights, Line width = Flight Volume",
         x = "", y = "") +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "right") +
    coord_fixed(ratio = 1.1)
}

# ---- Show Maps ----
print(plot_origin_map("JFK"))
print(plot_origin_map("LGA"))
print(plot_origin_map("EWR"))
