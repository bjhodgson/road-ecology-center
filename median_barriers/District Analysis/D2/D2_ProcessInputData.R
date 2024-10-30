library(readxl)
library(sf)
library(dplyr)

# Set paths to data directories
file_dir <- "C:\\Users\\HP\\Downloads" # Set pathway to folder containing input Excel data
shp_dir <- "D:\\Median Barriers\\District Spatial Data\\D2 Spatial Data" # Set pathway to folder containing input shapefile data
script_dir <- "C:\\Users\\HP\\Documents\\GitHub\\road-ecology-center\\median_barriers\\District Analysis" # Set path to folder containing RScripts

# PROCESS EXCEL FILES

# Define list of Excel file names and corresponding output names
setwd(file_dir) # Set working directory to folder containing Excel files
files <- list(
  "D2 Western Gray Squirrel CROS Medians.xlsx" = "squirrel_excel",
  #"Coyote CROS Medians (2).xlsx" = "coyote_excel",
  #"Jackrabbit CROS Medians (2).xlsx" = "jackrabbit_excel",
  "D2 Random Point Medians (3).xlsx" = "random_excel"
  
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

# Filter by sheet to ensure random point distribution
# random_excel <- random_excel %>%
#   filter(Sheet == 2)

# PROCESS SHAPEFILES



gdf <- st_read("D:\\Median Barriers\\District Spatial Data\\D2 Spatial Data\\MuleDeer\\D2_MuleDeer.shp")
gdf <- gdf %>%
  filter(condition %in% c("Injured", "Dead") |
           chips_An_1 %in% c("Fatality, result of collision", "Fatality, result of dispatch", "Injury"))
          
group <- gdf %>%
  group_by(latitude, longitude) %>%
 #slice(-1)
  #summarize(count = n())
  filter(n() < 2) 

#count(unique(group_by(gdf$latitude, gdf$longitude)))

#write_sf(group, "D:\\Median Barriers\\District Spatial Data\\D2 Spatial Data\\MuleDeer\\D2_MuleDeer_NoStack.shp")

# Define the shapefiles and their corresponding names
setwd(shp_dir) # Set working directory to folder containing shapefiles
shapefiles <- list(
  #"Mule Deer\\D9_deer_hwys.shp" = "deer_gdf",
  #"Coyote\\D9_coyote_1_145.shp" = "coyote_gdf",
  #"Jackrabbit\\D9_jackrabbit_1_138.shp" = "jackrabbit_gdf",
  "RandomPoints\\D2_Random_AADT.shp" = "random_gdf",
  "WesternGraySquirrel\\D2_WesternGraySquirrel_AADT.shp" = "squirrel_gdf"
)

# Loop through the shapefiles and process each one
setwd(shp_dir)  # Set working directory to folder containing shapefiles
for (shp_path in names(shapefiles)[2:2]) {
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
random_gdf <- st_read(names(shapefiles[1])) %>%
  rename(cid = CID)


# WRANGLE AND MERGE DATAFRAMES

# Bind dfs
df <- bind_rows(squirrel_excel) %>%
  select(!Sheet) %>%
  select(!...9)
  
# Bind gdfs
gdf <- bind_rows(squirrel_gdf) 
  
# Merge attributes by nid
merged_gdf <- inner_join(gdf, df, by = "nid")

# Filter merged_gdf for observation dates after Jan 1, 2015
merged_gdf$observatio <- as.POSIXct(merged_gdf$observatio, format = "%Y/%m/%d %H:%M:%S") # Convert data type to date
merged_gdf <- merged_gdf %>%
  filter(observatio >= as.POSIXct("2015-01-01 00:00:00"))

# Filter by WVC observations
WVC_gdf <- merged_gdf %>%
  filter(condition %in% c("Injured", "Dead") |
    chips_An_1 %in% c("Fatality, result of collision", "Fatality, result of dispatch", "Injury"))

# Bind random excel to random gdf
merged_random_gdf <- inner_join(random_gdf, random_excel, by = "cid") %>%
  select(!c("Sheet"))

unique(merged_random_gdf$StreetImageryDate)


# Convert mm/yy and mm/yyyy formats to a consistent Date format (year-month-day defaulting to 1st day)
merged_random_gdf <- merged_random_gdf %>%
  mutate(StreetImageryDate = parse_date_time(StreetImageryDate, orders = "my", tz = "America/Los_Angeles"))


# Add lat/long fields to gdf
coordinates <- st_coordinates(merged_random_gdf)
merged_random_gdf$Latitude <- coordinates[, "Y"]
merged_random_gdf$Longitude <- coordinates[, "X"]


# Create df object to export to csv
merged_random_df <- as.data.frame(merged_random_gdf) %>%
  select(-c("geometry", "Join_Count", "TARGET_FID"))

# Write to csv
write.csv(merged_random_df, file.path(file_dir, "D2_random_sites.csv"), row.names = FALSE)



# Filter WVCs by invalid conditions
WVC_gdf <- WVC_gdf %>%
  filter(rowSums(across(everything(), ~ grepl("overpass", ., ignore.case = TRUE))) == 0) %>% # Remove overpasses
  filter(!grepl("bridge", MedianNotes, ignore.case = TRUE)) # Remove bridges

unique(WVC_gdf$StreetImageryDate)

# Convert m/yyyy and mm/yyyy formats to a consistent Date format (year-month-day defaulting to 1st day)
WVC_gdf <- WVC_gdf %>%
  mutate(StreetImageryDate = parse_date_time(StreetImageryDate, orders = "my", tz = "America/Los_Angeles"))

# Create df object to export to csv
WVC_df <- as.data.frame(WVC_gdf) %>%
  select(-c("geometry", "Join_Count", "TARGET_FID"))

# Write to csv
#write.csv(WVC_df, file.path(file_dir, "D2_squirrel_hits.csv"), row.names = FALSE)

#_____ intersection test

int_wvc <- WVC_gdf %>%
  filter(grepl("intersection", SecondaryAttribute, ignore.case = TRUE))

nrow(int_wvc)/nrow(WVC_gdf)*100

int_random <- merged_random_gdf %>%
  filter(grepl("intersection", SecondaryAttribute, ignore.case = TRUE))

nrow(int_random)/nrow(merged_random_gdf)*100

chips_wvc <- WVC_gdf %>%
  filter(grepl("x", chips_Mark, ignore.case=TRUE))

nrow(chips_wvc)/nrow(WVC_gdf)*100

off_chips <- chips_wvc %>%
  filter(grepl("pull", SecondaryAttribute, ignore.case = TRUE) | 
           grepl("intersection", SecondaryAttribute, ignore.case = TRUE))

nrow(off_chips)/nrow(chips_wvc)*100

nrow(off_chips)/nrow(WVC_gdf)*100


off_wvc <- WVC_gdf %>%
  filter(grepl("pull", SecondaryAttribute, ignore.case = TRUE) | 
           grepl("intersection", SecondaryAttribute, ignore.case = TRUE))

nrow(off_wvc)/nrow(WVC_gdf)*100

off_random <- merged_random_gdf %>%
  filter(grepl("pull", SecondaryAttribute, ignore.case = TRUE) | 
           grepl("intersection", SecondaryAttribute, ignore.case = TRUE))

nrow(off_random)/nrow(merged_random_gdf)*100
