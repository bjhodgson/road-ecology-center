library(readxl)
library(sf)
library(dplyr)

# Set paths to data directories
file_dir <- "C:\\Users\\HP\\Downloads" # Set pathway to folder containing input Excel data
shp_dir <- "D:\\Median Barriers\\District Spatial Data\\D9 Spatial Data" # Set pathway to folder containing input shapefile data
script_dir <- "C://Users//HP//Documents//GitHub//road-ecology-center//median_barriers//District Analysis" # Set path to folder containing RScripts

# PROCESS EXCEL FILES

# Define list of Excel file names and corresponding output names
setwd(file_dir) # Set working directory to folder containing Excel files
files <- list(
  "D9 Deer CROS Medians (7).xlsx" = "deer_excel",
  "Coyote CROS Medians (2).xlsx" = "coyote_excel",
  "Jackrabbit CROS Medians (2).xlsx" = "jackrabbit_excel",
  "D9 Untreated Points Medians (7).xlsx" = "random_excel"
  
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
  "MuleDeer\\D9_MuleDeer_AADT.shp" = "deer_gdf",
  "Coyote\\D9_Coyote_AADT.shp" = "coyote_gdf",
  "BlacktailedJackrabbit\\D9_Jackrabbit_AADT.shp" = "jackrabbit_gdf",
  "RandomPoints\\D9_Random_AADT.shp" = "random_gdf"
)

# Loop through the shapefiles and process each one
setwd(shp_dir)  # Set working directory to folder containing shapefiles
removed_records <- list()  # To store removed duplicates

for (shp_path in names(shapefiles)[1:1]) {
  # Read the shapefile
  gdf <- st_read(shp_path)
  
  # Find duplicate rows based on latitude, longitude, and observation date
  duplicates <- gdf %>%
    group_by(latitude, longitude, observatio) %>%
    filter(n() > 1)  # Keep only duplicates
  
  # Store duplicates into the list
  removed_records[[shp_path]] <- duplicates
  
  # Clean the data by removing stacked points (keeping only one per group)
  cleaned_gdf <- gdf %>%
    group_by(latitude, longitude, observatio) %>%
    slice(1) %>%
    ungroup()
  
  # Assign the cleaned dataframe to a variable
  assign(shapefiles[[shp_path]], cleaned_gdf)
}

# Combine all removed records into one dataframe
all_removed_records <- bind_rows(removed_records)

# View or save the removed records
all_removed_records


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
gdf <- bind_rows(coyote_gdf, jackrabbit_gdf, deer_gdf) 
  
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

unique(merged_random_gdf$StreetImageryDate)

# Convert mm/yy and mm/yyyy formats to a consistent Date format (year-month-day defaulting to 1st day)
merged_random_gdf <- merged_random_gdf %>%
  mutate(StreetImageryDate = parse_date_time(StreetImageryDate, orders = "my", tz = "America/Los_Angeles"))

# Create df object to export to csv
merged_random_df <- as.data.frame(merged_random_gdf) %>%
  select(-c("geometry", "Join_Count", "TARGET_FID"))

# Write to csv
write.csv(merged_random_df, file.path(file_dir, "D9_random_sites.csv"), row.names = FALSE)


# Filter WVCs by mule deer & by invalid conditions
WVC_gdf <- WVC_gdf %>%
  filter(animal == "Mule (or Black tailed) Deer") %>% # Filter by animal
  filter(rowSums(across(everything(), ~ grepl("overpass", ., ignore.case = TRUE))) == 0) %>% # Remove overpasses
  filter(!grepl("bridge", MedianNotes, ignore.case = TRUE)) # Remove bridges

# Reformat dates for consistency
WVC_gdf <- WVC_gdf %>%
  mutate(StreetImageryDate = ifelse(StreetImageryDate == "02/08", "2/2008", StreetImageryDate))

# Convert m/yyyy and mm/yyyy formats to a consistent Date format (year-month-day defaulting to 1st day)
WVC_gdf <- WVC_gdf %>%
  mutate(StreetImageryDate = parse_date_time(StreetImageryDate, orders = "my", tz = "America/Los_Angeles"))

# Create df object to export to csv
WVC_df <- as.data.frame(WVC_gdf) %>%
  select(-c("geometry", "Join_Count", "TARGET_FID"))

# Write to csv
#write.csv(WVC_df, file.path(file_dir, "D9_deer_hits.csv"), row.names = FALSE)

#unique(WVC_gdf$StreetImageryDate)
#---- intersection check
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

