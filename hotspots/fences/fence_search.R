library(dplyr)

# Read in csv as dataframe
#df <- read.csv("C:\\Users\\Leo Hecht\\Documents\\Road Ecology\\Hotspot Report\\top_half.csv") # Leo path
df <- read.csv("H:\\hotspots\\bridges\\output_data\\points_table.csv") # Ben path

# Test subset
#df <- df[df$OBJECTID <= 500000, ]

# df1 <- df %>%
#   filter(!is.na(VCU))

# Initialize variables
bridge_indices <- which(!is.na(df$VCU))  # Indices of points with non-NA values of VCU
all_indices <- which(!is.na(df$OBJECTID))
distances <- numeric(length(all_indices) - 1)  # Vector to store distances

# Calculate distances between consecutive pairs of points with non-NA values of VCU
for (i in seq_along(bridge_indices)[-1]) {
  distances[i - 1] <- bridge_indices[i] - bridge_indices[i - 1]     # NOT WORKING --> NEED TO SEARCH OVER ALL POINTS AND FIND DIST BETWN POINTS WITH VCU != NA
}

# Print the distances
print(distances) 


# Find rows where distance is equal to 1
rows_distance_1 <- data.frame(index = fence_indices[-1], distance = distances) %>%
  filter(distance == 1)

# Print the resulting dataframe
print(rows_distance_1)

df[818        , "INTERSEC"]




# Initialize variables
all_indices <- which(!is.na(df$OBJECTID))  # Indices of all points
vcu_indices <- which(!is.na(df$VCU))  # Indices of points with non-NA values of VCU
num_points_between <- numeric(length(vcu_indices) - 1)  # Vector to store number of points between

# Find the corresponding indices of VCU not equal to NA for each OBJECTID index
vcu_indices_matched <- sapply(all_indices, function(idx) {
  min(vcu_indices[vcu_indices >= idx])
})

# Calculate the number of points between consecutive pairs of points with non-NA values of VCU
for (i in seq_along(vcu_indices_matched)[-1]) {
  num_points_between[i - 1] <- vcu_indices_matched[i] - vcu_indices_matched[i - 1] - 1
}

# Print the number of points between
print(num_points_between)