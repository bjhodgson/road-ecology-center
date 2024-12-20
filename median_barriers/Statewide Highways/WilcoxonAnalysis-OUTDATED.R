# install.packages("readxl")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

#---------------------------------------------------------------------------
# Read and Preprocess CROS Data
#---------------------------------------------------------------------------

# Read the CROS data from the provided Excel file
cros <- read_xlsx("D:\\Downloads\\cros-2024-12-50m.xlsx")

# Filter out stacked observations (where latitude and longitude are repeated)
cros_nostack <- cros %>%
  group_by(latitude, longitude) %>%
  mutate(n = n()) %>%    # Use mutate instead of count() to keep original rows and add the count column
  filter(n < 2) %>%      # Filter where n is less than 2
  ungroup() %>%
  select(-n) 

# Filter CROS data for only "Dead" and "Injured" conditions and observations after January 1, 2015
cros_nostack <- cros %>%
  filter(condition %in% c("Dead", "Injured")) %>%
  filter(observation_date >= "2015/01/01 00:00:00.000")

# Define the list of Transect_IDs to remove from analysis
remove_transects_list <- list(41,
                              #49,
                              57,
                              #67,
                              #92,
                              #106,
                              #110,
                              #8,
                              #58,
                              22,
                              #10,
                              108,
                              4,
                              62,
                              52,
                              63,
                              #14,
                              94,
                              71,
                              72)

# Filter out the specified Transect_IDs from the CROS data
cros_nostack_filtered <- cros_nostack %>%
  filter(!Transect_ID %in% remove_transects_list)

#---------------------------------------------------------------------------
# Calculate Transect Hit Rates and Restructure Data for Analysis 
#---------------------------------------------------------------------------

# Group the filtered CROS data by Transect_ID and Median_Type, and calculate the count (Hits) per group
grouped_cros <- cros_nostack_filtered %>%
  group_by(Transect_ID, Median_Type) %>%
  summarize(Hits = n(), .groups = "drop")

# Calculate the average transect length (in meters) for each Transect_ID and Median_Type combination
cros_selected <- cros %>%
  group_by(Transect_ID, Median_Type) %>%
  summarize(Average_Transect_Length = mean(Average_Transect_Length, na.rm = TRUE), .groups = "drop") # Take avg length across N/S or E/W transects

# Join the Average_Transect_Length to grouped_cros
grouped_cros <- grouped_cros %>%
  left_join(cros_selected, by = c("Transect_ID", "Median_Type"))

# Create hit rate column
grouped_cros <- grouped_cros %>%
  mutate(HitRate_1km = Hits / (Average_Transect_Length / 1000))

# Create dataframe of unique transect rows
transects <- grouped_cros %>%
  select(Transect_ID, Median_Type, HitRate_1km)

# Pivot the transect data so that each Transect_ID is one row with separate columns for each Median_Type's HitRate_1km
pivoted_transects <- transects %>%
  pivot_wider(names_from = Median_Type, values_from = HitRate_1km)

# View the result
print(pivoted_transects)

# Create the pair_type column by combining the names of non-NA columns for each row
pivoted_transects <- pivoted_transects %>%
  mutate(pair_type = pmap_chr(select(., concrete, veg, thrie, cable, none), ~ {
    # Create a character vector of non-NA column names
    non_na_columns <- c("concrete", "veg", "thrie", "cable", "none")[!is.na(c(...))]
    # Combine non-NA column names into a string
    if (length(non_na_columns) > 1) {
      return(paste(non_na_columns, collapse = " vs "))
    } else if (length(non_na_columns) == 1) {
      return(non_na_columns)
    } else {
      return(NA_character_)
    }
  })) %>%
  select(Transect_ID, pair_type, everything()) # Move the pair_type column to the second position

# Filter out rows where pair_type does not contain "vs"
pivoted_transects_filtered <- pivoted_transects %>%
  filter(!is.na(pair_type)) %>%
  filter(str_detect(pair_type, "vs"))

# Split the data by pair_type and assign each subset to a new dataframe
pair_dataframes <- pivoted_transects %>%
  group_by(pair_type) %>%
  group_split()

# Create an empty list to store the dataframes
pair_dataframes_list <- list()

# Iterate over each dataframe and store it in the list with its pair_type
for (df in pair_dataframes) {
  # Get the pair_type (this will be used as the key for the list entry)
  pair_name <- unique(df$pair_type)
  
  # Add the pair_name as a new column in the dataframe (optional, if not already included)
  df <- df %>% mutate(pair_type = pair_name)
  
  # Drop all columns with NA values for this dataframe
  df <- df %>% select_if(~ !all(is.na(.)))
  
  # Add the dataframe to the list, using pair_name as the list item name
  pair_dataframes_list[[pair_name]] <- df
}

# Check the result for one of the pair dataframes
print(pair_dataframes_list[[2]])

#---------------------------------------------------------------------------
# Run Paired Wilcoxon Test
#---------------------------------------------------------------------------

# Create an empty dataframe to store the results
wilcoxon_results <- data.frame(
  Pair = character(),
  V = numeric(),
  p.value = numeric(),
  stringsAsFactors = FALSE
)

# Iterate over the treatment comparison dataframes in the list
for (pair_name in names(pair_dataframes_list)) {
  # Extract the dataframe for the current pair
  df <- pair_dataframes_list[[pair_name]]
  
  # Make sure the pair dataframe has the necessary columns
  # Assuming the columns are the ones representing the two Median Types
  # e.g., 'concrete' and 'veg' for "concrete vs veg"
  
  # Perform the Wilcoxon signed-rank test on the two columns (adjust if necessary)
  test_result <- wilcox.test(df[[3]], df[[4]], paired = TRUE)
  
  # Store the results (pair name, V statistic, and p-value) in the results dataframe
  wilcoxon_results <- rbind(wilcoxon_results, data.frame(
    Pair = pair_name,
    V = test_result$statistic,
    p.value = test_result$p.value
  ))
}

# View the results
print(wilcoxon_results)





#---------------------------------------------------------------------------
# Calculate Transect Hit Rate Summary Statistics
#---------------------------------------------------------------------------

# 
# # Iterate over each dataframe in the pair_dataframes_list
# for (pair_name in names(pair_dataframes_list)) {
#   # Extract the dataframe for the current pair
#   df <- pair_dataframes_list[[pair_name]]
# 
#   # Calculate the mean and median for each column (excluding NA values)
#   for (col in names(df)) {
#     if (col != "Transect_ID" && col != "pair_type") {  # Skip non-relevant columns
#       # Calculate mean and median for each column
#       df[paste(col, "mean", sep = "_")] <- mean(df[[col]], na.rm = TRUE)
#       df[paste(col, "median", sep = "_")] <- median(df[[col]], na.rm = TRUE)
#     }
#   }
# 
#   # Add the updated dataframe back to the list
#   pair_dataframes_list[[pair_name]] <- df
# }
# 
# # Optionally, check the result for one of the pair dataframes
# print(pair_dataframes_list[[3]])





