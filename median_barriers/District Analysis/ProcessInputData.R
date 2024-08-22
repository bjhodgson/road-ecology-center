library(readxl)
library(sf)
library(dplyr)

# Set paths to data directories
file_dir <- "C:\\Users\\HP\\Downloads" # Set pathway to folder containing input Excel data
shp_dir <- "D:\\Median Barriers\\D9 Shapefiles" # Set pathway to folder containing input shapefile data
script_dir <- "C://Users//HP//Documents//GitHub//road-ecology-center//median_barriers//District Analysis" # Set path to folder containing RScripts

# PROCESS EXCEL FILES

# Define list of Excel file names and corresponding output names
setwd(file_dir) # Set working directory to folder containing Excel files
files <- list(
  "D9 Deer CROS Medians (6).xlsx" = "deer_excel",
  "Coyote CROS Medians (2).xlsx" = "coyote_excel",
  "Jackrabbit CROS Medians (2).xlsx" = "jackrabbit_excel",
  "D9 Untreated Points Medians (6).xlsx" = "random_excel"
)

# Set path to function to process Excel files from Rscript file
function_path <- file.path(script_dir, "FunctionProcessXLSX.R")
# Pull function to process Excel files from RScript file
source(function_path)

# Loop through the files and process each one
setwd(file_dir) # Set working directory to folder containing Excel files
for (path in names(files)) {
  process_excel_sheets(path, files[[path]])
}

# PROCESS SHAPEFILES

# Define the shapefiles and their corresponding names
setwd(shp_dir) # Set working directory to folder containing shapefiles
shapefiles <- list(
  "Mule Deer\\D9_deer_hwys.shp" = "deer_gdf",
  "Coyote\\D9_coyote_1_145.shp" = "coyote_gdf",
  "Jackrabbit\\D9_jackrabbit_1_138.shp" = "jackrabbit_gdf",
  "Random Points\\random_points.shp" = "random_gdf"
)

# Loop through the shapefiles and process each one
setwd(shp_dir)  # Set working directory to folder containing shapefiles
for (shp_path in names(shapefiles)[1:3]) {
  # Read the shapefile
  gdf <- st_read(shp_path)
  
  # Clean the data by removing stacked points
  cleaned_gdf <- gdf %>%
    group_by(latitude, longitude, observatio) %>% # Group by same location and observation date
    slice(1) %>%
    ungroup()
  
  # Assign the cleaned dataframe to a variable
  assign(shapefiles[[shp_path]], cleaned_gdf)
}

# Process random points shapefile
random_gdf <- st_read(names(shapefiles[4]))


# WRANGLE AND MERGE DATAFRAMES

# Bind dfs
df <- bind_rows(coyote_excel, jackrabbit_excel, deer_excel) %>%
  select(!Sheet) %>%
  rename(MedianNotes = Notes,
         nid = NID
  )

# Bind gdfs
gdf <- bind_rows(coyote_gdf, jackrabbit_gdf, deer_gdf) %>%
  select(!"ORIG_FID")
  
# Merge attributes by nid
merged_gdf <- inner_join(gdf, df, by = "nid") #%>%
  #select(!"ORIG_FID")

#nid <- gdf$nid[!gdf$nid %in% df$nid]

sum(merged_gdf$animal == "Mule (or Black tailed) Deer") # 1386 vs 8077 (all stacked)
sum(merged_gdf$animal == "Coyote") # 132 vs 132 (all stacked)
sum(merged_gdf$animal == "Black-Tailed Jackrabbit") # 138 vs 138 (all stacked)


# Filter merged_gdf for observation dates after Jan 1, 2015
merged_gdf$observatio <- as.POSIXct(merged_gdf$observatio, format = "%Y/%m/%d %H:%M:%S") # Convert data type to date
filtered_gdf <- merged_gdf %>%
  filter(observatio >= as.POSIXct("2015-01-01 00:00:00"))

sum(filtered_gdf$animal == "Mule (or Black tailed) Deer") # 433 vs 1114 (all stacked)
sum(filtered_gdf$animal == "Coyote") # 90 vs 99 (all stacked)
sum(filtered_gdf$animal == "Black-Tailed Jackrabbit") # 21 vs 21 (all stacked)

# Filter by WVC observations
WVC_gdf <- filtered_gdf %>%
  filter(condition %in% c("Injured", "Dead") |
    chips_An_1 %in% c("Fatality, result of collision", "Fatality, result of dispatch", "Injury"))

sum(WVC_gdf$animal == "Mule (or Black tailed) Deer") # 433 vs 1114 (all stacked)
sum(WVC_gdf$animal == "Coyote") # 90 vs 99 (all stacked)
sum(WVC_gdf$animal == "Black-Tailed Jackrabbit") # 21 vs 21 (all stacked)

# Bind random excel to random gdf
merged_random_gdf <- inner_join(random_gdf, random_excel, by = "CID") %>%
  select(!c("LATITUDE", "LONGITUDE", "Sheet")) %>%
  rename(MedianNotes = Notes,
         FID = CID
  )
