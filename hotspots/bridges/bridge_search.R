library(dplyr)

# Read in csv as dataframe
#df <- read.csv("C:\\Users\\Leo Hecht\\Documents\\Road Ecology\\Hotspot Report\\top_half.csv") # Leo path
df <- read.csv("H:\\hotspots\\bridges\\output_data\\points_table.csv") # Ben path

# Test subset
#df <- df[df$OBJECTID <= 500000, ]


# Identify bridge points
bridge_indices <- which(df$VCU == 0)

# Mark points within 2 miles (2*53 points) of a bridge
df$near_bridge <- 0
bridge_distance <- 2*53

for (bridge_idx in bridge_indices) {
  
  # Mark point at bridge 
  df$near_bridge[bridge_idx] <- 1
  
  # Mark points within 2 miles before the bridge
  start_idx <- max(1, bridge_idx - bridge_distance)
  df$near_bridge[start_idx:bridge_idx] <- 1
  
  # Mark points within 1 mile after the bridge
  end_idx <- min(nrow(df), bridge_idx + bridge_distance)
  df$near_bridge[bridge_idx:end_idx] <- 1
}

# Filter df 
df_filtered <- df %>%
  filter(near_bridge == 0) %>% # Filter by points further than 2 miles from nearest bridge
  filter(annl_nc > 2) # Filter by high annual roadkill


# Reorder points to combine adjacent lines

# Initialize new_sequence with NA
df_filtered$new_sequence <- NA

# Initialize variable to keep track of the current sequence number
current_sequence <- 1

# Initialize variable to keep track of the previous ORIG_FID
previous_orig_fid <- df_filtered$ORIG_FID[1]

# Assign initial new_sequence value
df_filtered$new_sequence[1] <- current_sequence

# Iterate over the rows of the dataframe starting from the second row
for (i in 2:nrow(df_filtered)) {
  # Check if the current ORIG_FID is the same as the previous one
  if (df_filtered$ORIG_FID[i] == previous_orig_fid) {
    # Assign the same new_sequence value as the previous point
    df_filtered$new_sequence[i] <- current_sequence
  } else {
    # Check the difference between current and previous ORIG_FID
    if (df_filtered$ORIG_FID[i] - previous_orig_fid == 1) {
      # Assign the same new_sequence value as the previous point
      df_filtered$new_sequence[i] <- current_sequence
    } else {
      # Increment the current sequence number
      current_sequence <- current_sequence + 1
      # Assign a new new_sequence value
      df_filtered$new_sequence[i] <- current_sequence
    }
  }
  
  # Update the previous ORIG_FID for the next iteration
  previous_orig_fid <- df_filtered$ORIG_FID[i]
}



# Save the edited dataset
#write.csv(df, "C:\\Users\\Leo Hecht\\Documents\\Road Ecology\\Hotspot Report\\edited_dataset.csv", row.names = FALSE) # Leo path
#write.csv(df_filtered, "H:\\hotspots\\fencing_crossings\\output_data\\updated_points_table5.csv", row.names = FALSE) # Ben path




