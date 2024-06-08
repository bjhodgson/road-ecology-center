# Load required libraries
library(sf)
library(ggplot2)

setwd("H:\\hotspots\\fences\\input_data")

# Read the shapefile containing lines
lines <- st_read(".//wcc_ESPG3857\\wcc_ESPG3857.shp")

# Define your distance threshold
distance_threshold <- 70  # Distance in meters

# Read the shapefile containing points
points <- st_read(".//State_Highway_Bridges_2021\\State_Highway_Bridges_2021\\State_Highway_Bridges.shp")

# Create a buffer around the lines
line_buffer <- st_buffer(lines, distance_threshold)

# Check if each point falls within the buffer
points_within_buffer <- st_intersection(points, line_buffer)






# Plot the points within the buffer
# plot(lines)
# plot(line_buffer, add = TRUE, col = "blue")
# plot(points_within_buffer, add = TRUE, pch = 20, col = "red")


# Plot the map
# ggplot() +
#   geom_sf(data = lines, color = "blue") +
#   geom_sf(data = line_buffer, fill = NA, color = "red") +
#   geom_sf(data = points, color = "green", size = 2) +
#   theme_minimal()
  