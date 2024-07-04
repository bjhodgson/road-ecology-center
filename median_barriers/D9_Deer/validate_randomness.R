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

# Determine the range of x and y coordinates
xrange <- range(c(points1$x, points2$x))
yrange <- range(c(points1$y, points2$y))

# Define the window to encompass all points
window <- owin(xrange = xrange, yrange = yrange)

# Convert to ppp objects with the updated window
ppp1 <- ppp(points1$x, points1$y, window = window)
ppp2 <- ppp(points2$x, points2$y, window = window)

# Calculate the K-function for both point patterns
K1 <- Kest(ppp1)
K2 <- Kest(ppp2)

# Plot the K-functions for visual comparison
plot(K1, main = "K-function comparison")
plot(K2, add = TRUE, col = "red")

# Perform a test to compare the K-functions
test_result <- ks.test(K1, K2)
print(test_result)
