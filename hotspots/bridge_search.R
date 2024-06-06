library(readxl)

points_100ft <- read.csv("C:\\Users\\Leo Hecht\\Documents\\Road Ecology\\Hotspot Report\\top_half.csv")
  

points_100ft$NearBridge <- 0

# Looping over the entire length of the data frame
gdf <- points_100ft
  
# df <- read.csv('path_to_your_file.csv')


# Step 2: Identify bridge points
bridge_indices <- which(gdf$VCU == 0)

# Step 3 & 4: Mark points within 1 mile of a bridge
gdf$near_bridge <- 0

for (bridge_idx in bridge_indices) {
  # Mark points within 1 mile before the bridge
  start_idx <- max(1, bridge_idx - 53)
  gdf$near_bridge[start_idx:bridge_idx] <- 1
  
  # Mark points within 1 mile after the bridge
  end_idx <- min(nrow(gdf), bridge_idx + 53)
  gdf$near_bridge[bridge_idx:end_idx] <- 1
}




head(gdf)

# Save the edited dataset
write.csv(gdf, "C:\\Users\\Leo Hecht\\Documents\\Road Ecology\\Hotspot Report\\edited_dataset.csv", row.names = FALSE)
  
 # else {df[i, 'NearBridge'] = 0
  
  }
}

