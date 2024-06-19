# Load the raster package
library(raster)

# Step 1: Define paths to input and output files
tif_file <- "D:\\noise_modelling\\I8 Study Area\\input_data\\dem-hwy8-study-4326.tif"
ascii_file <- "D:\\noise_modelling\\I8 Study Area\\output_data\\DEM.asc"

# Step 2: Read the GeoTIFF file into a raster object
raster_data <- raster(tif_file)

# Step 3: Convert raster to ASCII format and write to file
writeRaster(raster_data, filename = ascii_file, format = "ascii")

# Print message upon successful conversion
cat(paste("GeoTIFF file", tif_file, "converted to ASCII format and saved as", ascii_file, "\n"))
