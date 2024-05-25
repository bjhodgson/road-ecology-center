# Install and load necessary packages
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
library(readxl)
library(dplyr)
library(ggplot2)

# Function to list xlsx and xls files in a given directory
list_excel_files <- function(directory) {
  xls_files <- list.files(path = directory, pattern = "*.xls", full.names = TRUE)
  c(xls_files)
}

# Specify the path to your directory containing Excel files
directory_path <- "D:\\Documents\\road-ecology-center\\median_barriers\\CROS_medians_dataset"

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

# Remove the specified columns
cleaned_df <- combined_df %>% select(-one_of(columns_to_remove))

# Filter out NA values from the cleaned dataframe
cleaned_df_filtered <- cleaned_df[!is.na(cleaned_df$New_ID_1), ]

# Visualize histogram of hits per median type
cleaned_df_filtered %>%
  group_by(Pair_Type) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Pair_Type, y = count)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Frequency of 'New_ID_1' Values", x = "Value", y = "Count") +
  theme_minimal()

# Add concrete median binary variable
cleaned_df_filtered$concrete_median <- as.factor(ifelse(cleaned_df_filtered$New_ID_1 == "con", 1, 0))

# Visualize histogram of hits per concrete vs non-concrete median type
cleaned_df_filtered %>%
  group_by(concrete_median) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = concrete_median, y = count)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Frequency of Hits by Concrete vs Non-concrete Medians", x = "Concrete Median", y = "Count") +
  theme_minimal()


