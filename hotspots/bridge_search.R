
# Read in csv as dataframe
#df <- read.csv("C:\\Users\\Leo Hecht\\Documents\\Road Ecology\\Hotspot Report\\top_half.csv") # Leo path
df <- read.csv("H:\\hotspots\\fencing_crossings\\output_data\\points_table.csv") # Ben path

# Identify bridge points
bridge_indices <- which(df$VCU == 0)

# Mark points within 1 mile (53 points) of a bridge
df$near_bridge <- 0

for (bridge_idx in bridge_indices) {
  
  # Mark point at bridge 
  df$near_bridge[bridge_idx] <- 1
  
  # Mark points within 1 mile before the bridge
  start_idx <- max(1, bridge_idx - 53)
  df$near_bridge[start_idx:bridge_idx] <- 1
  
  # Mark points within 1 mile after the bridge
  end_idx <- min(nrow(df), bridge_idx + 53)
  df$near_bridge[bridge_idx:end_idx] <- 1
}

# Save the edited dataset
#write.csv(df, "C:\\Users\\Leo Hecht\\Documents\\Road Ecology\\Hotspot Report\\edited_dataset.csv", row.names = FALSE) # Leo path
write.csv(df, "H:\\hotspots\\fencing_crossings\\output_data\\updated_points_table.csv", row.names = FALSE) # Ben path


