library(readxl)
library(dplyr)

# Read the CSV file
#df <- read.csv("D:\\hotspots\\fences\\output_data\\wcc_bridges.csv")

# Define bridge points
bridge_indices <- which(df$VCU == 0)

# Calculate distance in points (assuming 53 points per mile)
bridge_distance <- 2 * 53

# Initialize a column to mark points with annl_nc > 2 within 2 miles
df$fences <- 0

# Loop through each bridge index
for (bridge_idx in bridge_indices) {
  
  # Calculate the range of indices within 2 miles of the current bridge
  start_idx <- max(1, bridge_idx - bridge_distance)
  end_idx <- min(nrow(df), bridge_idx + bridge_distance)
  
  # Check if any annl_nc > 2 within the specified range
  if (any(df$annl_nc[start_idx:end_idx] > 2)) {
    df$fences[start_idx:end_idx] <- 1
  }
}

# Filter df
fences_df <- df %>%
  filter(fences == 1)

# Export to csv
write.csv(fences_df, "D:\\hotspots\\fences\\output_data\\TwoMileFences.csv", row.names = FALSE) # Ben path

