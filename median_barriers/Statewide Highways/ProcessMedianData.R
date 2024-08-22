library(readxl)
library(dplyr)

# Define the data directory
data_dir <- "D:/Median Barriers/Statewide Highways/Data Outputs"

# List all .xlsx files in the directory and subdirectories
xlsx_files <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

# Filter the list to include only files that end with "CROS.xlsx"
cros_files <- xlsx_files[grep("CROS\\.xlsx$", xlsx_files)]

# Function to read each Excel file
read_xlsx_file <- function(file) {
  tryCatch({
    # Read the file
    data <- read_excel(file)
    return(data)
  }, error = function(e) {
    cat("Error reading file:", file, "\nError message:", e$message, "\n")
    return(NULL)
  })
}

# Read all the CROS.xlsx files and store them in a list
cros_list <- lapply(cros_files, read_xlsx_file)

###  Determine lost Pair_IDs
# # Loop through the cros_list to print unique Pair_IDs for each dataframe
# for (i in seq_along(cros_list)) {
#   # Check if the dataframe is not NULL and contains the Pair_ID column
#   if (!is.null(cros_list[[i]]) && "Pair_ID" %in% colnames(cros_list[[i]])) {
#     cat("Unique Pair_IDs in dataframe", i, ":\n")
#     print(length(unique(cros_list[[i]]$Pair_ID)))
#     cat("\n")
#   } else {
#     cat("Pair_ID column not found or dataframe is NULL in dataframe", i, "\n\n")
#   }
# }

# Initialize an empty vector to store all unique Pair_IDs
# all_pair_ids <- character()
# 
# # Loop through the cros_list to extract unique Pair_IDs
# for (i in seq_along(cros_list)) {
#   if (!is.null(cros_list[[i]]) && "Pair_ID" %in% colnames(cros_list[[i]])) {
#     # Extract unique Pair_IDs and append to the all_pair_ids vector
#     all_pair_ids <- c(all_pair_ids, unique(cros_list[[i]]$Pair_ID))
#   }
# }
# 
# # Count the occurrences of each Pair_ID
# pair_id_counts <- table(all_pair_ids)
# 
# # Convert to a dataframe for easier viewing
# pair_id_summary <- as.data.frame(pair_id_counts)
# colnames(pair_id_summary) <- c("Pair_ID", "Count")
# 
# # Print the summary
# print(pair_id_summary)
###

# Remove any NULL entries (in case some files failed to read)
cros_list <- Filter(Negate(is.null), cros_list)

# Combine all data frames into one
cros_df <- bind_rows(cros_list)

# List of columns to remove
columns_to_remove <- c(
  "OBJECTID", "Join_Count",	"TARGET_FID",
  "TARGET_FID_1", "Valid_Pair", "Highway_SR", "New_ID", "Field15", "FID_1", 
  "Join_Count_1", "TARGET_F_1", "Id", "ORIG_FID", "ORIG_SEQ",
  "Field15_1", "NEAR_FID", "NEAR_DIST", "NEAR_X", "NEAR_Y", "DDLat", "DDLon", "ORIG_OID",
  "Field15_12", "Field15_13", "Valid_Pa_2", "Valid_Pa_3", "Highway__2", "Highway__3",
  "TARGET_F_2", "New_ID_12", "New_ID__13", "TARGET_F_3", "OBJECTID_1", "FID_", "Latitude_1",
  "Longitud_1", "ORIG_FID_1", "ORIG_SEQ_1", "Pair_Nam_1", "Pair_ID_1",  "Transect_1", "Pair_Typ_1",
  "Primary__1", "Median_w_1", "Secondary1", "Notes_1", "Initials_1", "Field14_1",
  "NEAR_FID_1", "NEAR_DIS_1", "NEAR_X_1", "NEAR_Y_1", "DDLat_1", "DDLon_1", "ORIG_OID_1",
  "Field14", "FID_12", "Join_Cou_1", "Id_1"
)

# Remove unneeded fields
cros_df <- cros_df %>% 
  select(-one_of(columns_to_remove))

# Rename fields
cros_df <- cros_df %>%
  rename(
    "Valid" = "Valid_Pa_1",
    "Highway" = "Highway__1",
    #"Pair_ID" = 
    "Transect_ID" = "Transect_I",
    #"Pair_Type" = 
    "MedianType" = "Primary_Me", 
    "MedianWidth" = "Median_wid",
    "SecondaryAttribute" = "Secondary_",
    "MedianNotes" = "Notes"
    #"Initials"  
  )

# Standardize values
cros_df <- cros_df %>%
  mutate(MedianType = case_when( # Standardize values
    MedianType == "conc" ~ "concrete",
    MedianType == "con" ~ "concrete",
    MedianType == "veg" ~ "vegetative",
    MedianType == "dirt" ~ "gravel",
    TRUE ~ MedianType  # Keep all other values unchanged
  )) %>%
  mutate(Pair_Type = case_when( # Standardize values
    Pair_Type %in% c("conc/veg", "con/veg", "concrete/veg", "veg/conc", "veg/con", "veg/concrete") ~ "concrete/vegetative",
    #Pair_Type %in% c("veg/conc", "veg/con") ~ "concrete/vegetative",
    Pair_Type == "veg/thrie" ~ "thrie/vegetative",
    Pair_Type == "cable/veg" ~ "cable/vegetative",
    Pair_Type == "thrie/cable" ~ "thrie/cable",
    Pair_Type == "thrie/concrete" ~ "concrete/thrie",
    Pair_Type == "veg/veg" ~ "vegetative/vegetative",
    #Pair_Type == "veg/concrete" ~ "concrete/vegetative",
    Pair_Type == "dirt/double concrete" ~ "concrete/gravel",
    Pair_Type == "veg/none" ~ "vegetative/none",
    Pair_Type %in% c("concrete/none", "conc/none") ~ "concrete/none",
    TRUE ~ Pair_Type  # Keep all other values unchanged
  ))


# Filter records 
cros_df <- cros_df %>%
  filter(observatio >= as.POSIXct("2015-01-01 00:00:00")) %>% # Filter by date: (Jan 1, 2015 - April 2024)
  filter(chips_An_1 %in% c("Fatality, result of collision", "Fatality, result of dispatch", "Injury") |
           condition %in% c("Dead", "Injured") ) %>% # Filter by WVCs
  filter(animal == "Mule (or Black tailed) Deer") # Filter by species
#   #filter(animal == "Black Bear")
#   #group_by(animal) %>%
  #count(animal)


# Read in highway segments Excel files

# Filter the list to include only files that do NOT end with "CROS.xlsx"
segments_list <- xlsx_files[!grepl("CROS\\.xlsx$", xlsx_files)]

# Read all the non-CROS.xlsx files and store them in a list
segments_list <- lapply(segments_list, read_xlsx_file)

# Remove any NULL entries (in case some files failed to read)
segments_list <- Filter(Negate(is.null), segments_list)

# Ensure consistent data types for specific columns after reading all data
segments_list <- lapply(segments_list, function(df) {
  if ("Highway_SR" %in% names(df)) {
    df$Highway_SR <- as.character(df$Highway_SR)
  }
  # Add other column conversions if needed
  return(df)
})

# Combine all data frames into one df
segments_df <- bind_rows(segments_list) 

# Standardize values & clean up columns
segments_df <- segments_df %>%
  filter(!is.na(Highway_SR)) %>% # Remove NA median data segments
  select(Pair_Name, Valid_Pair, Highway_SR, Pair_ID, Transect_I, Pair_Type, # Remove unnecessary columns
         Primary_Me, Median_wid, Secondary_, Latitude, Longitude, Notes, Initials) %>%
  mutate(Primary_Me = case_when( # Standardize values
    Primary_Me == "conc" ~ "concrete",
    Primary_Me == "con" ~ "concrete",
    Primary_Me == "veg" ~ "vegetative",
    Primary_Me == "dirt" ~ "gravel",
    TRUE ~ Primary_Me  # Keep all other values unchanged
  )) %>%
  mutate(Pair_Type = case_when( # Standardize values
    Pair_Type %in% c("conc/veg", "con/veg", "concrete/veg", "veg/conc", "veg/con", "veg/concrete") ~ "concrete/vegetative",
    #Pair_Type %in% c("veg/conc", "veg/con") ~ "concrete/vegetative",
    Pair_Type == "veg/thrie" ~ "thrie/vegetative",
    Pair_Type == "cable/veg" ~ "cable/vegetative",
    Pair_Type == "thrie/cable" ~ "thrie/cable",
    Pair_Type == "thrie/concrete" ~ "concrete/thrie",
    Pair_Type == "veg/veg" ~ "vegetative/vegetative",
    #Pair_Type == "veg/concrete" ~ "concrete/vegetative",
    Pair_Type == "dirt/double concrete" ~ "concrete/gravel",
    Pair_Type == "veg/none" ~ "vegetative/none",
    Pair_Type %in% c("concrete/none", "conc/none") ~ "concrete/none",
    TRUE ~ Pair_Type  # Keep all other values unchanged
  ))

# Edit values based on notes to ensure accuracy
segments_df <- segments_df %>%
  mutate(Primary_Me = case_when(
    Pair_ID == "4" ~ "gravel", # Veg w/ dirt falls under "gravel" type
    TRUE ~ Primary_Me 
  )) %>%
  filter(!Pair_ID %in% c("106")) # Median type not consistent across entire transect


# Count segment distance by unique record
segment_count <- segments_df %>%
  group_by(Pair_Name, Pair_Type, Primary_Me) %>%
  count(Pair_Type) %>%
  rename("Distance100m" = n)

# Aggregate roadkill data in df grouped by unique transect record
grouped_cros_df <- cros_df %>%
  group_by(Pair_Name, Pair_Type, MedianType) %>%
  count(MedianType) %>% 
  filter(!MedianType %in% c("transition"))
#        !n <= 5)
 

# distance_df <- segments_df %>% 
#   group_by(Pair_ID, Pair_Name, Highway_SR) %>%
#   count(Primary_Me)
# 
# distance_df <- segments_df %>% 
#   group_by(Pair_ID) %>%
#   count(Primary_Me)
# 
# type_count <- cros_df %>%
#   group_by(Pair_Type) %>%
#   count(Pair_Type)

#type_diff <- segment_count$Pair_Type[!segment_count$Pair_Type %in% type_count$Pair_Type]




