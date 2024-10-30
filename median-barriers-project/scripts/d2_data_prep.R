
# ----------------------------------------
# Data Load-in
# ----------------------------------------

# Load config
suppressMessages(source("config.R"))

# Create list of raw data files by type
path <- "data/raw"
files_list <- list(
  "xlsx_files" <- list.files(path = path, pattern = "\\.xlsx$", full.names = TRUE),
  "shp_files" <- list.files(path = path, pattern = "\\.shp.zip$", full.names = TRUE)
)
#print(files_list)

# Create empty list to store new dataframes
df_list <- list() 

# Loop through xlsx files and create dataframes for each file 
for (xlsx in xlsx_files) {
  # Process xlsx files and rename dataframes
  output_name <- process_and_name_xlsx(xlsx, "df") # Add "df" suffix string
  
  # Append the newly created dataframe to the list
  df_list[[output_name]] <- get(output_name)  # Use get to retrieve the dataframe by its name

}
names(df_list) # Print dfs in list

# Create empty list to store new geodataframes
gdf_list <- list()

# Loop through shapefiles and create geodataframes for each file
for (shp in shp_files) {
  # Create object name from file name
  name <- create_name(shp, "gdf") # Add "gdf" suffix string
  
  # Read in the shapefile
  shp_data <- suppressMessages(st_read(shp))
  
  # Dynamically assign the read data to the created name
  assign(name, shp_data)
  
  # Append the newly created geodataframe to the list
  gdf_list[[name]] <- shp_data
  
  # Optionally print the name of the created object
  print(paste("Created object:", name))
  
}
names(gdf_list) # Print gdfs in list

# ----------------------------------------
# Raw Data Cleanup
# ----------------------------------------

# Pre-process dataframes

gdf_list[["deer_gdf"]]$observatio
# just do in list

# Loop through each dataframe in df_list to standardize date format
for (df_name in names(df_list)) {
  # Access the dataframe from df_list
  df_list[[df_name]] <- standardize_dates(df_list[[df_name]]$StreetImageryDate)
  
  # Print message for updated dataframe in the list
  print(paste("Updated in df_list:", df_name))
  
  # Check if a dataframe with the same name exists in the global environment
  if (exists(df_name, envir = .GlobalEnv)) {
    # Access the dataframe from the global environment
    df <- get(df_name, envir = .GlobalEnv)
    
    # Standardize the dates in the StreetImageryDate column
    if ("StreetImageryDate" %in% colnames(df)) {
      df$StreetImageryDate <- standardize_dates(df$StreetImageryDate)
      
      # Assign the updated dataframe back to the global environment
      assign(df_name, df, envir = .GlobalEnv)
      
      # Print message for updated dataframe in the global environment
      print(paste("Updated in global environment:", df_name))
    } else {
      print(paste("Column 'StreetImageryDate' not found in:", df_name))
    }
  } else {
    print(paste("Dataframe not found in global environment:", df_name))
  }
} # MAY BE MORE COMPLEX THAN NECESSARY


# gdf_list[["deer_gdf"]][["StreetImageryDate"]]

# DO I KEEP IN LIST OR NOT
# 
# # PROCESS SHAPEFILES
# gdf <- gdf %>%
#   filter(condition %in% c("Injured", "Dead") |
#            chips_An_1 %in% c("Fatality, result of collision", "Fatality, result of dispatch", "Injury"))
# 
# 
# # Loop through the shapefiles and process each one
# setwd(shp_dir)  # Set working directory to folder containing shapefiles
# for (shp_path in names(shapefiles)[2:2]) {
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
# 
# # Process random points shapefile
# random_gdf <- st_read(names(shapefiles[1])) %>%
#   rename(cid = CID)
# 
# 
# # WRANGLE AND MERGE DATAFRAMES
# 
# # Bind dfs
# df <- bind_rows(squirrel_excel) %>%
#   select(!Sheet) %>%
#   select(!...9)
# 
# # Bind gdfs
# gdf <- bind_rows(squirrel_gdf) 
# 
# # Merge attributes by nid
# merged_gdf <- inner_join(gdf, df, by = "nid")
# 
# # Filter merged_gdf for observation dates after Jan 1, 2015
# merged_gdf$observatio <- as.POSIXct(merged_gdf$observatio, format = "%Y/%m/%d %H:%M:%S") # Convert data type to date
# merged_gdf <- merged_gdf %>%
#   filter(observatio >= as.POSIXct("2015-01-01 00:00:00"))
# 
# # Filter by WVC observations
# WVC_gdf <- merged_gdf %>%
#   filter(condition %in% c("Injured", "Dead") |
#            chips_An_1 %in% c("Fatality, result of collision", "Fatality, result of dispatch", "Injury"))
# 
# # Bind random excel to random gdf
# merged_random_gdf <- inner_join(random_gdf, random_excel, by = "cid") %>%
#   select(!c("Sheet"))
# 
# unique(merged_random_gdf$StreetImageryDate)
# 
# 
# # Convert mm/yy and mm/yyyy formats to a consistent Date format (year-month-day defaulting to 1st day)
# merged_random_gdf <- merged_random_gdf %>%
#   mutate(StreetImageryDate = parse_date_time(StreetImageryDate, orders = "my", tz = "America/Los_Angeles"))
# 
# 
# # Add lat/long fields to gdf
# coordinates <- st_coordinates(merged_random_gdf)
# merged_random_gdf$Latitude <- coordinates[, "Y"]
# merged_random_gdf$Longitude <- coordinates[, "X"]
# 
# 
# # Create df object to export to csv
# merged_random_df <- as.data.frame(merged_random_gdf) %>%
#   select(-c("geometry", "Join_Count", "TARGET_FID"))
# 
# # Write to csv
# write.csv(merged_random_df, file.path(file_dir, "D2_random_sites.csv"), row.names = FALSE)
# 
# 
# 
# # Filter WVCs by invalid conditions
# WVC_gdf <- WVC_gdf %>%
#   filter(rowSums(across(everything(), ~ grepl("overpass", ., ignore.case = TRUE))) == 0) %>% # Remove overpasses
#   filter(!grepl("bridge", MedianNotes, ignore.case = TRUE)) # Remove bridges
# 
# unique(WVC_gdf$StreetImageryDate)
# 
# # Convert m/yyyy and mm/yyyy formats to a consistent Date format (year-month-day defaulting to 1st day)
# WVC_gdf <- WVC_gdf %>%
#   mutate(StreetImageryDate = parse_date_time(StreetImageryDate, orders = "my", tz = "America/Los_Angeles"))
# 
# # Create df object to export to csv
# WVC_df <- as.data.frame(WVC_gdf) %>%
#   select(-c("geometry", "Join_Count", "TARGET_FID"))
# 
# # Write to csv
# #write.csv(WVC_df, file.path(file_dir, "D2_squirrel_hits.csv"), row.names = FALSE)
# 
# #_____ intersection test
# 
# int_wvc <- WVC_gdf %>%
#   filter(grepl("intersection", SecondaryAttribute, ignore.case = TRUE))
# 
# nrow(int_wvc)/nrow(WVC_gdf)*100
# 
# int_random <- merged_random_gdf %>%
#   filter(grepl("intersection", SecondaryAttribute, ignore.case = TRUE))
# 
# nrow(int_random)/nrow(merged_random_gdf)*100
# 
# chips_wvc <- WVC_gdf %>%
#   filter(grepl("x", chips_Mark, ignore.case=TRUE))
# 
# nrow(chips_wvc)/nrow(WVC_gdf)*100
# 
# off_chips <- chips_wvc %>%
#   filter(grepl("pull", SecondaryAttribute, ignore.case = TRUE) | 
#            grepl("intersection", SecondaryAttribute, ignore.case = TRUE))
# 
# nrow(off_chips)/nrow(chips_wvc)*100
# 
# nrow(off_chips)/nrow(WVC_gdf)*100
# 
# 
# off_wvc <- WVC_gdf %>%
#   filter(grepl("pull", SecondaryAttribute, ignore.case = TRUE) | 
#            grepl("intersection", SecondaryAttribute, ignore.case = TRUE))
# 
# nrow(off_wvc)/nrow(WVC_gdf)*100
# 
# off_random <- merged_random_gdf %>%
#   filter(grepl("pull", SecondaryAttribute, ignore.case = TRUE) | 
#            grepl("intersection", SecondaryAttribute, ignore.case = TRUE))
# 
# nrow(off_random)/nrow(merged_random_gdf)*100
