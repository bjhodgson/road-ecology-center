library(readxl)
library(dplyr)

data_dir <- "D:\\Median Barriers\\HPMS\\HPMS Spatial Data"


# Define list of Excel file names and corresponding output names
setwd(data_dir) # Set working directory to folder containing Excel files
files <- list(
  "D9_HPMS.xlsx" = "D9_HPMS_df",
  "D9_HPMS_CROS.xlsx" = "D9_CROS_df"
)

# Loop through the Excel files and process each one
for (file_path in names(files)) {
  # Read the dataframe
  df <- read_xlsx(file_path) 
  
  # Assign the dataframe to a variable
  assign(files[[file_path]], df)
}

# Cleanup CROS dataframe
cros_df <- D9_CROS_df %>%
  select(!OBJECTID) %>% # Remove unnecessary columns
  filter(!is.na(MEDIAN_TYPE)) # Remove NA median type values

# Cleanup HPMS dataframe
roads_df <- D9_HPMS_df %>%
  select(!OBJECTID) %>% # Remove unnecessary columns
  filter(!is.na(MEDIAN_TYPE)) # Remove NA median type values

plot(density(cros_df$MEDIAN_TYPE))

counts <- cros_df %>%
  group_by(MEDIAN_TYPE) %>%
  count()


test <- cros_df %>%
  select(nid,
         condition,
         animal,
         MEDIAN_TYPE,
         MEDIAN_WIDTH,
         AADT, 
        
  )
