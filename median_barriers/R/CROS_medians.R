# Install and load necessary packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(ggplot2)

# Specify the path to your main directory
main_directory <- "H:\\median_barriers\\CROS_medians_dataset"

# For separated folders file structure
# # List all subdirectories in the main directory
# subdirectories <- list.dirs(path = main_directory, recursive = FALSE)
# 
# # Function to list Excel files in a given directory
# list_excel_files <- function(directory) {
#   list.files(path = directory, pattern = "*.xlsx", full.names = TRUE)
# }
# 
# # Get a list of Excel files from each subdirectory
# file_list <- unlist(lapply(subdirectories, list_excel_files))
# 
# # Filter the list to only include files with "CROS_medians" in the name
# filtered_file_list <- grep("CROS_medians.*_TableToExcel\\.xlsx$", file_list, value = TRUE)
# 
# # Read each Excel file into a list of dataframes
# df_list <- lapply(filtered_file_list, read_excel)


# For one data folder files structure

# Function to list xlsx files in a given directory
list_excel_files <- function(directory) {
   list.files(path = directory, pattern = "*.xlsx", full.names = TRUE)
}

# Function to list Excel files in a given directory
list_xls_files <- function(directory) {
  list.files(path = directory, pattern = "*.xls", full.names = TRUE)
}

# Get a list of Excel files from each subdirectory
file_list <- unlist(lapply(main_directory, list_excel_files))

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
  "TARGET_F_2", "New_ID_12", "New_ID__13", "TARGET_F_3"
)

# Remove the specified columns
cleaned_df <- combined_df %>% select(-one_of(columns_to_remove))

# Visualize histogram of hits per median type
cleaned_df %>%
  group_by(New_ID_1) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = New_ID_1, y = count)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Frequency of 'New_ID_1' Values", x = "Value", y = "Count") +
  theme_minimal()
