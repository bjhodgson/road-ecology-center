# install.packages("readxl")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")
# install.packages("purrr")


library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

#---------------------------------------------------------------------------
# Read and Preprocess CROS Data
#---------------------------------------------------------------------------

# Paths to input data
cros_path <- "D:\\Downloads\\cros50m-no overlaps.xlsx"
transects_path <- "D:\\Downloads\\transects-length-12-24-final.xlsx"

# Read the CROS data from the provided Excel file
cros <- read_xlsx(cros_path)

# Filter out stacked observations (where latitude and longitude are repeated)
cros_nostack <- cros %>%
  group_by(latitude, longitude) %>%
  mutate(n = n()) %>%
  filter(n < 2) %>%
  ungroup() %>%
  select(-n)


# Filter CROS data for only "Dead" and "Injured" conditions and observations after January 1, 2015
cros_nostack <- cros_nostack %>%
  filter(condition %in% c("Dead", "Injured")) %>%
  filter(observation_date >= "2015/01/01 00:00:00.000")

# Define the list of Transect_IDs to remove from analysis
remove_transects_list <- list(
                              41,
                              49,
                              57,
                              # 67,
                              92,
                              106,
                              110,
                              8,
                              # 58,
                              22,
                              10,
                              108,
                              4,
                              62,
                              52,
                              63,
                              14,
                              # 94,
                              71,
                              72
)

# Filter out the specified Transect_IDs from the CROS data
cros_nostack_filtered <- cros_nostack %>%
  filter(!Transect_ID %in% remove_transects_list)

#---------------------------------------------------------------------------
# Calculate Transect Hit Rates and Restructure Data for Analysis 
#---------------------------------------------------------------------------

# Calculate Transect Hit Rates and Restructure Data for Analysis
grouped_cros <- cros_nostack_filtered %>%
  group_by(Transect_ID, Median_Type) %>%
  summarize(Hits = n(), .groups = "drop")

# Read the transect data
transect_length <- read_xlsx(transects_path)

# Create pairs based on shared Transect_ID
transect_length <- transect_length %>%
  group_by(Transect_ID) %>%
  filter(n_distinct(Median_Type) == 2) %>%  # Ensure each Transect_ID has exactly 2 distinct Median_Types
  mutate(Pair_Type = paste(sort(Median_Type), collapse = " vs ")) %>%
  ungroup()

# Join transect lengths to CROS data
grouped_cros <- grouped_cros %>%
  left_join(transect_length, by = c("Transect_ID", "Median_Type"))

# Calculate hit rate
grouped_cros <- grouped_cros %>%
  mutate(HitRate_1km = Hits / (Average_Transect_Length / 1000))

# Clean up remove unnecessary columns
transects <- grouped_cros %>%
  select(Transect_ID, Median_Type, HitRate_1km, Pair_Type)

# Pivot transects df to wider format
pivoted_transects <- transects %>%
  pivot_wider(names_from = Median_Type, values_from = HitRate_1km)


# 
# # Create the pair_type column by combining the names of non-NA columns for each row
# pivoted_transects <- pivoted_transects %>%
#   mutate(pair_type = pmap_chr(select(., concrete, veg, thrie, cable, none), ~ {
#     non_na_columns <- c("concrete", "veg", "thrie", "cable", "none")[!is.na(c(...))]
#     if (length(non_na_columns) > 1) {
#       return(paste(non_na_columns, collapse = " vs "))
#     } else if (length(non_na_columns) == 1) {
#       return(non_na_columns)
#     } else {
#       return(NA_character_)
#     }
#   })) %>%
#   select(Transect_ID, pair_type, everything())

# # Filter out rows where pair_type does not contain "vs"
# pivoted_transects_filtered <- pivoted_transects %>%
#   filter(!is.na(Pair_Type)) %>%
#   filter(str_detect(Pair_Type, "vs"))

# # Split the data by pair_type and assign each subset to a new dataframe
# pair_dataframes <- pivoted_transects_filtered %>%
#   group_by(Pair_Type) %>%
#   group_split()

# Replace NA with 0 only for columns that are part of the pair (e.g., 'concrete' for "concrete vs thrie")
pivoted_transects <- pivoted_transects %>%
  mutate(
    concrete = if_else(str_detect(Pair_Type, "concrete") & is.na(concrete), 0, concrete),
    thrie = if_else(str_detect(Pair_Type, "thrie") & is.na(thrie), 0, thrie),
    veg = if_else(str_detect(Pair_Type, "veg") & is.na(veg), 0, veg),
    cable = if_else(str_detect(Pair_Type, "cable") & is.na(cable), 0, cable),
    none = if_else(str_detect(Pair_Type, "none") & is.na(none), 0, none)
  )


# Split the data by pair_type and assign each subset to a new dataframe
pair_dataframes <- pivoted_transects %>%
  group_by(Pair_Type) %>%
  group_split()

# Create an empty list to store the dataframes
pair_dataframes_list <- list()

# Iterate over each dataframe and store it in the list with its pair_type
for (df in pair_dataframes) {
  pair_name <- unique(df$Pair_Type)
  df <- df %>%
    mutate(Pair_Type = pair_name) %>%
    select_if(~ !all(is.na(.)))
  
  pair_dataframes_list[[make.names(pair_name)]] <- df
}

# Check the result for one of the pair dataframes
print(pair_dataframes_list[[2]])

#---------------------------------------------------------------------------
# Run Paired Wilcoxon Test
#---------------------------------------------------------------------------

# Run Paired Wilcoxon Test
wilcoxon_results <- data.frame(
  Pair = character(),
  V = numeric(),
  p.value = numeric(),
  stringsAsFactors = FALSE
)

for (pair_name in names(pair_dataframes_list)) {
  df <- pair_dataframes_list[[pair_name]]
  
  test_result <- wilcox.test(df[[names(df)[3]]], df[[names(df)[4]]], paired = TRUE)
  
  if (!is.null(test_result)) {
    wilcoxon_results <- rbind(wilcoxon_results, data.frame(
      Pair = pair_name,
      V = test_result$statistic,
      p.value = test_result$p.value
    ))
  }
}

# View the results
print(wilcoxon_results)


#---------------------------------------------------------------------------
# Calculate Transect Hit Rate Summary Statistics
#---------------------------------------------------------------------------

# Iterate over each dataframe in the pair_dataframes_list
for (pair_name in names(pair_dataframes_list)) {
  # Extract the dataframe for the current pair
  df <- pair_dataframes_list[[pair_name]]

  # Calculate the mean and median for each column (excluding NA values)
  for (col in names(df)) {
    if (col != "Transect_ID" && col != "Pair_Type") {  # Skip non-relevant columns
      # Calculate mean and median for each column
      df[paste(col, "mean", sep = "_")] <- mean(df[[col]], na.rm = TRUE)
      df[paste(col, "median", sep = "_")] <- median(df[[col]], na.rm = TRUE)
    }
  }

  # Add the updated dataframe back to the list
  pair_dataframes_list[[pair_name]] <- df
}

print(pair_dataframes_list[[5]])
