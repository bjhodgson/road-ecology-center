# Install and load necessary packages
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Read in transect lengths
segment_df <- read.csv("D:\\median_barriers\\sheets\\medians_cleaned.csv") %>%
    filter(Complete != "Deleted") # Filter out deleted transects

# Function to list xlsx and xls files in a given directory
list_excel_files <- function(directory) {
  xls_files <- list.files(path = directory, pattern = "*.xls", full.names = TRUE)
  c(xls_files)
}

# Specify the path to your folder containing Excel files
directory_path <- "C:\\Users\\HP\\Documents\\GitHub\\road-ecology-center\\median_barriers\\CROS_medians_dataset"
# Get a list of Excel files from the directory
file_list <- list_excel_files(directory_path)
# Read each Excel file into a list of dataframes
df_list <- lapply(file_list, read_excel)
# Combine the dataframes into one dataframe
combined_df <- bind_rows(df_list)

# View the combined dataframe
print(combined_df)

# List of columns to remove
columns_to_remove <- c(
  "OBJECTID", "Join_Count",	"TARGET_FID",
  "TARGET_FID_1", "Valid_Pair", "Highway_SR", "New_ID", "Field15", "FID_1", 
  "Join_Count_1", "TARGET_F_1", "Id", "ORIG_FID", "ORIG_SEQ",
  "Field15_1", "NEAR_FID", "NEAR_DIST", "NEAR_X", "NEAR_Y", "DDLat", "DDLon", "ORIG_OID",
  "Field15_12", "Field15_13", "Valid_Pa_2", "Valid_Pa_3", "Highway__2", "Highway__3",
  "TARGET_F_2", "New_ID_12", "New_ID__13", "TARGET_F_3", "OBJECTID_1", "FID_", "Latitude_1",
  "Longitud_1"
)

# Remove the specified columns and filter by hits
cleaned_df <- combined_df %>% 
  select(-one_of(columns_to_remove)) %>%
  filter(chips_An_1 %in% c("Fatality, result of collision", "Fatality, result of dispatch", "Injury") |
           condition %in% c("Dead", "Injured") ) # Filter by hits

# Group hits by unique pair ID and median type
grouped_df <- cleaned_df[!is.na(cleaned_df$New_ID_1), ] %>%
  rename(New_ID = New_ID_1) %>% #Rename New_ID column to act as join key
  group_by(Pair_ID, Pair_Name, New_ID) %>%
  summarise(count = n()) %>%
  merge(segment_df, by=c("Pair_Name","New_ID")) %>% #Join df's by key
  mutate(hits_per_100m = count / Segment_Count)
  
# Summary statistics by grouped median type
summary_stats_df <- grouped_df %>%
  group_by(New_ID) %>%
  summarise(
    mean_hitrate = mean(hits_per_100m), # Average across all transects
    median_hitrate = median(hits_per_100m),
    sd_hitrate = sd(hits_per_100m),
    min_hitrate = min(hits_per_100m),
    max_hitrate = max(hits_per_100m)
  )

