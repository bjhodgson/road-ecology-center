library(raster)
library(spatstat)
library(sf)
install.packages("ks")

# Check KDE difference 

# Load the raster file
raster_file <- "D:\\Median Barriers\\CROS Search\\validate_randomness\\KDE_diff.tif"
r <- raster(raster_file)

# Count number of zeros
num_zeros <- sum(values(r) == 0)
print(paste("Number of zero values in the raster:", num_zeros))

# Mask out zero values
r[r == 0] <- NA

# Count number of NA's
num_zeros <- sum(is.na(values(r)))
print(paste("Number of NA values in the raster:", num_zeros))

# Calculate the mean value of the raster
mean_value <- cellStats(r, stat = 'mean')
print(mean_value)

# Plot histogram of raster values
hist(as.vector(values(r)), main = "Histogram of Non-zero Raster Values",
     xlab = "Raster Values", ylab = "Frequency")


# Run Spatial K-S (MAD) Test

# Load points

path1 <- "D:\\Median Barriers\\CROS Search\\validate_randomness\\D9_random_all\\D9_random_all.shp"
points <- st_read(path1)
points1 <- st_transform(points, crs = 4326)
# Extract coordinates
coordinates <- st_coordinates(points1)
# Add coordinates to the data frame
points1$x <- coordinates[,1]
points1$y <- coordinates[,2]

path2 <- "D:\\Median Barriers\\CROS Search\\validate_randomness\\D9_random_S_P\\D9_random_S_P.shp"
points <- st_read(path2)
points2 <- st_transform(points, crs = 4326)
# Extract coordinates
coordinates <- st_coordinates(points2)
# Add coordinates to the data frame
points2$x <- coordinates[,1]
points2$y <- coordinates[,2]

# Transform to UTM Zone 10N (EPSG:32610) for spatial analysis
points1_proj <- st_transform(points1, crs = 32610)
points2_proj <- st_transform(points2, crs = 32610)

# Convert to ppp objects
ppp1 <- as.ppp(points1_proj)
ppp2 <- as.ppp(points2_proj)

# Compute KDEs for both point patterns
kde1 <- density.ppp(ppp1)
kde2 <- density.ppp(ppp2)

# Calculate the difference in KDE values
kde_diff <- kde1$fhat - kde2$fhat

# Optionally, perform bootstrap resampling to assess significance
nboot <- 1000  # Number of bootstrap replicates
boot_diff <- numeric(nboot)

for (i in 1:nboot) {
  # Sample with replacement from points1 and points2
  sample1 <- sample(ppp1, replace = TRUE)
  sample2 <- sample(ppp2, replace = TRUE)
  
  # Compute KDE for each sample
  kde_sample1 <- density.ppp(sample1)
  kde_sample2 <- density.ppp(sample2)
  
  # Compute difference in KDEs
  boot_diff[i] <- kde_sample1$fhat - kde_sample2$fhat
}

# Observed difference in KDEs
obs_diff <- kde1$fhat - kde2$fhat

# Calculate p-value
p_value <- sum(abs(boot_diff) >= abs(obs_diff)) / nboot

# Output results
print(paste("Observed difference in KDEs:", obs_diff))
print(paste("Bootstrap p-value:", p_value))