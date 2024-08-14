library(dplyr)
library(sf)
library(readxl)
library(lubridate)
library(ggplot2)

# Set pathway to folder containing input data
setwd("C:\\Users\\HP\\Downloads")
# Pull processed data from RScript
source("C:\\Users\\HP\\Documents\\GitHub\\road-ecology-center\\median_barriers\\District Analysis\\ProcessInputData.R")

# Summarize counts by median type
roadkill_counts <- merged_gdf %>%
  as.data.frame() %>%
  filter(condition %in% c("Dead", "Injured")) %>%  # Filter by animal condition
  count(MedianType) %>%
  rename(Count = n)  # Rename the count column to "Count"
#print(sum(roadkill_counts$Count))




# Assuming 'MednTyp' in 'cleaned_gdf' contains random points data
random_counts <- as.data.frame(table(merged_random_gdf$MedianType))

random_counts <- merged_random_gdf %>%
  as.data.frame() %>%
  count(MedianType) %>%
  rename(Count = n)  # Rename the count column to "Count"
#print(sum(random_counts$Count))

# COUNT MEDIAN TYPES

df <- merged_random_gdf
median_type <- df$MedianType

# Separate each entry into individual components
df_separated <- df %>%
  mutate(median_type = strsplit(as.character(median_type), ", ")) %>%
  unnest(median_type)

# Count occurrences of each median type
median_counts <- df_separated %>%
  group_by(median_type) %>%
  summarise(count = n())

# Print the results
print(median_counts)


#-------

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
