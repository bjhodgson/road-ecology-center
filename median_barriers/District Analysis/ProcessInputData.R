library(readxl)
library(sf)
library(dplyr)

# Set paths to data directories
file_dir <- "C:\\Users\\HP\\Downloads" # Set pathway to folder containing input Excel data
shp_dir <- "D:\\Median Barriers\\D9 Shapefiles" # Set pathway to folder containing input shapefile data
script_dir <- "C://Users//HP//Documents//GitHub//road-ecology-center//median_barriers//District Analysis" # Set path to folder containing RScripts

# Define list of Excel file names and corresponding output names
setwd(file_dir)
files <- list(
  "Deer CROS Medians (6).xlsx" = "deer_excel",
  "Coyote CROS Medians (2).xlsx" = "coyote_excel",
  "Jackrabbit CROS Medians (2).xlsx" = "jackrabbit_excel",
  "D9 Untreated Points Medians.xlsx" = "random_excel"
)

# Set pathway to folder containing input shapefile data
#setwd("D:\\Median Barriers\\D9 Shapefiles")

# Define the shapefiles and their corresponding names
setwd(shp_dir)
shapefiles <- list(
  "Mule Deer\\D9_deer_hwys.shp" = "deer_gdf",
  "Coyote\\D9_coyote_1_145.shp" = "coyote_gdf",
  "Jackrabbit\\D9_jackrabbit_1_138.shp" = "jackrabbit_gdf",
  "Random Points\\random_points.shp" = "random_gdf"
)

# Set path to function to process Excel files from Rscript file
function_path <- file.path(script_dir, "FunctionProcessXLSX.R")
# Pull function to process Excel files from RScript file
source(function_path)

# Loop through the files and process each one
setwd(file_dir)
for (path in names(files)) {
  process_excel_sheets(path, files[[path]])
}

# Loop through the shapefiles and process each one
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



# #REMOVE STACKED POINTS FROM ALL GDFs
# # Define the shapefiles and their corresponding names
# shapefiles <- list(
#   "D:\\Median Barriers\\D9 Shapefiles\\Mule Deer\\D9_deer_hwys.shp" = "deer_gdf",
#   "D:\\Median Barriers\\D9 Shapefiles\\Coyote\\D9_coyote_1_145.shp" = "coyote_gdf",
#   "D:\\Median Barriers\\D9 Shapefiles\\Jackrabbit\\D9_jackrabbit_1_138.shp" = "jackrabbit_gdf"
#   #"D:\\Median Barriers\\D9 Shapefiles\\Random Points\\random_points.shp" = "random_gdf"
# )
# 
# # Loop through the shapefiles
# for (shp_path in names(shapefiles)) {
#   # Read the shapefile
#   gdf <- st_read(shp_path)
# 
#   # Clean the data by removing stacked points
#   cleaned_gdf <- gdf %>%
#     group_by(latitude, longitude, observatio) %>% # Group by same location and observation date
#     slice(1) %>%
#     ungroup()
# 
#   # Assign the cleaned dataframe to a variable
#   assign(shapefiles[[shp_path]], cleaned_gdf)
# }


# Bind dfs
df <- bind_rows(coyote_excel, jackrabbit_excel, deer_excel) %>%
  select(!Sheet) %>%
  rename(MedianNotes = Notes,
         nid = NID
  )

# Bind gdfs
gdf <- bind_rows(coyote_gdf, jackrabbit_gdf, deer_gdf) #%>%
  #select(!"ORIG_FID")
  
# Merge attributes by nid
merged_gdf <- inner_join(gdf, df, by = "nid") #%>%
  #select(!"ORIG_FID")

sum(merged_gdf$animal == "Mule (or Black tailed) Deer") # 1386 vs 8077 (all stacked)
sum(merged_gdf$animal == "Coyote") # 132 vs 132 (all stacked)
sum(merged_gdf$animal == "Black-Tailed Jackrabbit") # 138 vs 138 (all stacked)


# Filter gdf for observation dates after Jan 1, 2015
gdf$observatio <- as.POSIXct(gdf$observatio, format = "%Y/%m/%d %H:%M:%S") # Convert data type to date
filtered_gdf <- gdf %>%
  filter(observatio >= as.POSIXct("2015-01-01 00:00:00"))

sum(filtered_gdf$animal == "Mule (or Black tailed) Deer") # 433 vs 1114 (all stacked)
sum(filtered_gdf$animal == "Coyote") # 90 vs 99 (all stacked)
sum(filtered_gdf$animal == "Black-Tailed Jackrabbit") # 21 vs 21 (all stacked)

# PROBLEM: conditions (on highways, remove stacked points, Jan 1, 2015-present) reduce sample size down to 533 (433 deer), lower if remove stacked points from all species)

# Filter by WVC observations

WVC_gdf <- filtered_gdf %>%
  filter(condition %in% c("Injured", "Dead"))

sum(WVC_gdf$animal == "Mule (or Black tailed) Deer") # 433 vs 1114 (all stacked)
sum(WVC_gdf$animal == "Coyote") # 90 vs 99 (all stacked)
sum(WVC_gdf$animal == "Black-Tailed Jackrabbit") # 21 vs 21 (all stacked)


# Read in random points shapefile to gdf
random_gdf <- st_read()

# Bind random excel to random gdf
merged_random_gdf <- inner_join(random_gdf, random_excel, by = "CID") %>%
  select(!c("LATITUDE", "LONGITUDE", "Sheet")) %>%
  rename(MedianNotes = Notes,
         FID = CID
  )





# # Write merged gdf to shp
# out_path <- "D:\\Median Barriers\\D9 Shapefiles\\Combined Dataset\\D9_CROS_Medians.shp"
# st_write(merged_gdf, out_path)
#   


# # Write random merged gdf to shp  
# out_path <- "D:\\Median Barriers\\D9 Shapefiles\\Random Points\\D9_Random_Medians.shp"
# st_write(merged_random_gdf, out_path)  

