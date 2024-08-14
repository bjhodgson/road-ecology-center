library(dplyr)
library(sf)
library(readxl)
library(lubridate)
library(ggplot2)
library(tidyr)

# Set paths to data directories
script_dir <- "C://Users//HP//Documents//GitHub//road-ecology-center//median_barriers//District Analysis" # Set path to folder containing RScripts
# Pull objects from data processing script
source(file.path(script_dir, "ProcessInputData.R"))

# DATA PREP FOR ANALYSIS

# Summarize WVC counts by median type
WVC_counts <- WVC_gdf %>%
  as.data.frame() %>%
  filter(animal %in% c( # Filter by animal
    "Mule (or Black tailed) Deer" # Deer only
    # "Coyote",
    # "Black-Tailed Jackrabbit"
    )) %>%  
  count(MedianType) %>%
  rename(Count = n)  # Rename the count column to "Count"
print(sum(roadkill_counts$Count))

# Summarize random point counts by median type
random_counts <- merged_random_gdf %>%
  slice_sample(n = 1100) %>%
  as.data.frame() %>% 
  # filter(!MedianType %in% c( # METHOD 1: Filter out non-standard values
  #   "temp concrete",
  #   "gravel, vegetative",
  #   "thrie beam, vegetative",
  #   "vegetative, gravel",
  #   "vegetative, thrie beam"
  # )) %>%
  mutate(MedianType = case_when( # METHOD 2: Standardize values
    MedianType == "vegetative, thrie beam" ~ "thrie beam",
    MedianType == "thrie beam, vegetative" ~ "thrie beam",
    MedianType == "gravel, vegetative" ~ "vegetative",
    MedianType == "vegetative, gravel" ~ "vegetative",
    MedianType == "temp concrete" ~ "concrete",
    TRUE ~ MedianType  # Retain original value if no condition is met
  )) %>%
  count(MedianType) %>%
  rename(Count = n)  # Rename the count column to "Count"
print(sum(random_counts$Count))

# Create contingency table for chi-square test
contingency_table <- matrix(c(WVC_counts$Count, random_counts$Count), 
                            ncol = 2, 
                            byrow = FALSE,
                            dimnames = list(WVC_counts$MedianType, c("Roadkill", "Random"))
)

# Perform chi-square test
chisq.test(contingency_table)









# COUNT MEDIAN TYPES

df <- WVC_gdf
median_type <- df$MedianType

# Count median types
median_counts <- df %>%
  # Split the median types into separate components
  mutate(median_type = strsplit(as.character(median_type), ", ")) %>%
  unnest(median_type) %>%
  # Count occurrences of each individual median type
  group_by(median_type) %>%
  summarise(count = n(), .groups = 'drop')

# MATCH SAMPLE SIZE
df <- merged_random_gdf
median_type <- df$MedianType


# Randomly select 1700 rows from the dataframe
sampled_df <- df %>%
  slice_sample(n = 1700)

# Process the sampled rows
processed_sample <- sampled_df %>%
  # Split the median types into separate components
  mutate(median_type = strsplit(as.character(median_type), ", ")) %>%
  unnest(median_type) %>%
  # Count occurrences of each individual median type
  group_by(median_type) %>%
  summarise(count = n(), .groups = 'drop')

#-------

cleaned_random_gdf <- merged_random_gdf %>%
  # Standardize values
  mutate(MedianType = case_when(
    MedianType == "vegetative, thrie beam" ~ "thrie beam, vegetative",
    MedianType == "gravel, vegetative" ~ "vegetative, gravel",
    MedianType == "temp concrete" ~ "concrete",
    TRUE ~ MedianType  # Retain original value if no condition is met
  ))

random_counts <- cleaned_random_gdf %>%
  slice_sample(n = 1100) %>%
  as.data.frame() %>% 
  filter(!MedianType %in% c( # Filter out non-standard values
    # "temp concrete",
    # "gravel, vegetative",
    # "thrie beam, vegetative",
    # "vegetative, gravel",
    # "vegetative, thrie beam"
  )) %>%
  mutate
  count(MedianType) %>%
  rename(Count = n)  # Rename the count column to "Count"
print(sum(random_counts$Count))



# STANDARDIZE ORDER OF AND COUNT MULTI TYPE MEDIANS

# Function to normalize the entries
normalize_entry <- function(entry) {
  # Split the entry into components
  components <- unlist(strsplit(entry, ", "))
  # Sort the components and reassemble them
  sorted_components <- paste(sort(components), collapse = ", ")
  return(sorted_components)
}

# Apply normalization
df_normalized <- df %>%
  mutate(normalized_median_type = sapply(median_type, normalize_entry)) %>%
  group_by(normalized_median_type) %>%
  summarise(count = n(), .groups = 'drop')

# Print the results
print(df_normalized)


# Chi square
contingency_table <- matrix(c(median_counts$count, median_counts1$count), 
                            ncol = 2, 
                            byrow = FALSE,
                            dimnames = list(median_counts$median_type, c("Random", "Roadkill"))
                            )

chi_squared_test <- chisq.test(contingency_table)

chi_squared_test # need take 







# Combine both counts into a data frame for alignment
combined_counts <- merge(roadkill_counts, random_counts, 
                         by.x = "MedianType", by.y = "MedianType", all = TRUE)

# Replace NA with 0 (in case some median types are not present in one of the datasets)
combined_counts[is.na(combined_counts)] <- 0

# Filter out non-shared median types
combined_counts %>% 
  filter(!Var1 %in% c("overpass (open air)", "temp concrete", "thrie beam and veg"))

# Step 2: Create the contingency table
contingency_table <- matrix(c(combined_counts$Freq.x, combined_counts$Freq.y), 
                            ncol = 2, 
                            byrow = FALSE,
                            dimnames = list(combined_counts$Var1, c("Roadkill", "Random")))

# Step 3: Perform the Chi-Squared Test
chi_squared_test <- chisq.test(contingency_table)

# Print the results of the chi-squared test
print(chi_squared_test)


# Chi-Squared Test on No Median / Veg

combined_counts <- combined_counts %>%
  filter(Var1 %in% c("no median", "vegetative"))
# Step 2: Create the contingency table
contingency_table <- matrix(c(combined_counts$Freq.x, combined_counts$Freq.y), 
                            ncol = 2, 
                            byrow = FALSE,
                            dimnames = list(combined_counts$Var1, c("Roadkill", "Random")))
# Step 3: Perform the Chi-Squared Test
chi_squared_test <- chisq.test(contingency_table)
# Print the results of the chi-squared test
print(chi_squared_test)
