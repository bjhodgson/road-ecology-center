

# Fences



library(readxl)

# Load the dataset
points_100ft <- read.csv("C:\\Users\\Leo Hecht\\Documents\\Road Ecology\\Hotspot Report\\output_data\\points_table_fence.csv")

# Create a new column called "fence" and initialize it to 0
points_100ft$fence <- 0

# Identify bridge points with annl_nc >= 2
bridge_indices <- which(points_100ft$VCU == 0 & points_100ft$annl_nc >= 2)

# Loop through each bridge index and mark points for "fence"
for (i in seq_along(bridge_indices)) {
  bridge_idx <- bridge_indices[i]
  
  # Mark points sequentially on either side of the bridge until we hit another bridge
  # or until we reach 53 points (1 mile) in either direction
  start_idx <- max(1, bridge_idx - 53)
  end_idx <- min(nrow(points_100ft), bridge_idx + 53)
  
  # Initialize fence marking
  points_100ft$fence[bridge_idx] <- 1
  
  # Mark points before the bridge
  if (i == 1 || bridge_indices[i-1] < start_idx) {
    points_100ft$fence[start_idx:bridge_idx] <- 1
  } else {
    points_100ft$fence[(bridge_indices[i-1] + 1):bridge_idx] <- 1
  }
  
  # Mark points after the bridge
  if (i == length(bridge_indices) || bridge_indices[i+1] > end_idx) {
    points_100ft$fence[bridge_idx:end_idx] <- 1
  } else {
    points_100ft$fence[bridge_idx:(bridge_indices[i+1] - 1)] <- 1
  }
}

# Save the edited dataset
write.csv(points_100ft, "C:\\Users\\Leo Hecht\\Documents\\Road Ecology\\Hotspot Report\\edited_dataset_with_fence.csv", row.names = FALSE)


