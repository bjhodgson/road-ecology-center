library(readxl)
library(sf)
library(dplyr)

# Set pathway to folder containing input data
setwd("C:\\Users\\HP\\Downloads")

# Pull function to process Excel files from RScript file
source("C:\\Users\\HP\\Documents\\GitHub\\road-ecology-center\\median_barriers\\District Analysis\\FunctionProcessXLSX.R")

# Define list of Excel file names and corresponding output names
files <- list(
  "Deer CROS Medians (6).xlsx" = "deer_excel",
  "Coyote CROS Medians (2).xlsx" = "coyote_excel",
  "Jackrabbit CROS Medians (2).xlsx" = "jackrabbit_excel",
  "Untreated Points Medians (6).xlsx" = "random_excel"
)

# Loop through the files and process each one
for (path in names(files)) {
  process_excel_sheets(path, files[[path]])
}


# Define a list of shapefile paths and corresponding output variables
shapefiles <- list(
  "D:\\Median Barriers\\D9 Shapefiles\\Mule Deer\\D9_deer_hwys.shp" = "deer_gdf"
  # "D:\\Median Barriers\\D9 Shapefiles\\Coyote\\D9_coyote_1_145.shp" = "coyote_gdf",
  # "D:\\Median Barriers\\D9 Shapefiles\\Jackrabbit\\D9_jackrabbit_1_138.shp" = "jackrabbit_gdf",
  # "D:\\Median Barriers\\D9 Shapefiles\\Random Points\\random_points.shp" = "random_gdf"
)

# Loop through the shapefiles and read each one
for (shp_path in names(shapefiles)) {
  assign(shapefiles[[shp_path]], st_read(shp_path))
}


# Remove stacked points from deer gdf
# Count occurrences of each latitude and longitude pair
grouped_deer_gdf <- deer_gdf %>%
  group_by(latitude, longitude) %>%
  mutate(group_number = cur_group_id()) %>%
  ungroup()  

# Slice first record in each group
cleaned_deer_gdf <- grouped_deer_gdf %>%
  group_by(latitude, longitude) %>%
  slice(1) %>%
  ungroup() %>%
  select(!"group_number")

# Bind dfs
df <- bind_rows(coyote_excel, jackrabbit_excel, deer_excel) %>%
  select(!Sheet) %>%
  rename(MedianNotes = Notes,
         nid = NID
  )

# Bind gdfs
gdf <- bind_rows(coyote_gdf, jackrabbit_gdf, cleaned_deer_gdf) #%>%
  select(!ORIG_FID)
  
# Merge attributes
merged_gdf <- inner_join(gdf, df, by = "nid") %>%
  select(!"ORIG_FID")

# Write merged gdf to shp
out_path <- "D:\\Median Barriers\\D9 Shapefiles\\Combined Dataset\\D9_CROS_Medians.shp"
st_write(merged_gdf, out_path)
  
# Bind random excel to random gdf
merged_random_gdf <- inner_join(random_gdf, random_excel, by = "CID") %>%
  select(!c("LATITUDE", "LONGITUDE", "Sheet")) %>%
  rename(MedianNotes = Notes,
         FID = CID
  )
  
# Write random merged gdf to shp  
out_path <- "D:\\Median Barriers\\D9 Shapefiles\\Random Points\\D9_Random_Medians.shp"
st_write(merged_random_gdf, out_path)  
