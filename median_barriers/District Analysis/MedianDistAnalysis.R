library(dplyr)
library(lubridate)
library(tidyr)

# Set paths to data directories
script_dir <- "C://Users//HP//Documents//GitHub//road-ecology-center//median_barriers//District Analysis" # Set path to folder containing RScripts
# Pull objects from data processing script
source(file.path(script_dir, "ProcessInputData.R"))

# Compare distributions of median type

# DATA PREP FOR ANALYSIS

# Categorize vegetative medians by vegetation type
WVC_gdf <- WVC_gdf %>% 
  filter(animal == "Mule (or Black tailed) Deer") %>% # Filter by animal
  mutate(MedianType = case_when(
    MedianType == "vegetative" & grepl("grass", SecondaryAttribute) ~ paste0(MedianType, " (grass)"),
    MedianType == "vegetative" & grepl("sparse shrubs/trees", SecondaryAttribute) ~ paste0(MedianType, " (sparse shrubs/trees)"),
    MedianType == "vegetative" & grepl("dense trees", SecondaryAttribute) ~ paste0(MedianType, " (dense shrubs/trees)"),
    TRUE ~ MedianType
  ))

# Summarize WVC counts by median type
WVC_counts <- WVC_gdf %>%
  as.data.frame() %>%
  mutate(MedianType = case_when( # Standardize values
    MedianType == "vegetative, thrie beam" ~ "thrie beam", # Change to thrie beam, otherwise count too low
    MedianType == "thrie beam, vegetative" ~ "thrie beam", # Change to thrie beam, otherwise count too low
    MedianType == "gravel, vegetative" ~ "vegetative",
    MedianType == "vegetative, gravel" ~ "vegetative",
    MedianType == "temp concrete" ~ "concrete",
    TRUE ~ MedianType  # Retain original value if no condition is met
  )) %>%
  count(MedianType) %>%
  rename(Count = n)  # Rename the count column to "Count"
print(sum(WVC_counts$Count))

# Categorize vegetative medians by vegetation type
merged_random_gdf <- merged_random_gdf %>% 
  mutate(MedianType = case_when(
    MedianType == "vegetative" & grepl("grass", SecondaryAttribute) ~ paste0(MedianType, " (grass)"),
    MedianType == "vegetative" & grepl("sparse shrubs/trees", SecondaryAttribute) ~ paste0(MedianType, " (sparse shrubs/trees)"),
    MedianType == "vegetative" & grepl("dense trees", SecondaryAttribute) ~ paste0(MedianType, " (dense shrubs/trees)"),
    MedianType == "vegetative" & grepl("dense shrubs/trees", SecondaryAttribute) ~ paste0(MedianType, " (dense shrubs/trees)"),
    TRUE ~ MedianType
  ))

# # Check if any "vegetative" remains uncategorized
# uncategorized_vegetative <- merged_random_gdf %>%
#   filter(MedianType == "vegetative")
# 
# nid_list <- merged_random_gdf %>%
#   filter(MedianType == "vegetative" & is.na(SecondaryAttribute)) %>%
#   pull(FID)


# Summarize random point counts by median type
set.seed(123)  # Set a seed for reproducibility
random_counts <- merged_random_gdf %>%
  slice_sample(n = 1100) %>%
  as.data.frame() %>%
  mutate(MedianType = case_when( # Standardize values
    MedianType == "vegetative, thrie beam" ~ "thrie beam", # Change to thrie beam, otherwise count too low
    MedianType == "thrie beam, vegetative" ~ "thrie beam", # Change to thrie beam, otherwise count too low
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
                            dimnames = list(WVC_counts$MedianType, c("Roadkill", "Random")))


# ANALYSIS

# Set up Chi-Square Goodness of Fit Test

# Calculate total counts
total_observed <- sum(WVC_counts$Count)
total_expected <- sum(random_counts$Count)

# Calculate proportions in the Random group
expected_proportions <- random_counts$Count / total_expected

# Perform Chi-Square Goodness of Fit Test
chisq_test <- chisq.test(WVC_counts$Count, p = expected_proportions)
print(chisq_test)




# Perform chi-square test for independence
chisq_test <- chisq.test(contingency_table)
print(chisq.test(contingency_table))


# Perform Fisher Exact Test with simulated p-values
fisher_result <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 1e5) # Simulate to reduce computational requirements
print(fisher_result)

# Analyze residuals

# Calculate percentages and merge counts into a single data frame
combined_counts <- merge(
  WVC_counts %>%
    mutate(Percentage = (Count / sum(Count)) * 100),
  random_counts %>%
    mutate(Percentage = (Count / sum(Count)) * 100),
  by = "MedianType", all = TRUE
) %>%
  rename(
    "Observed_Count" = Count.x,
    "Expected_Count" = Count.y,
    "Observed_Percentage" = Percentage.x,
    "Expected_Percentage" = Percentage.y
  )

# Replace NA values with 0
combined_counts[is.na(combined_counts)] <- 0

# Calculate Pearson residuals and standardized residuals
combined_counts <- combined_counts %>%
  mutate(
    Residual = Observed_Count - Expected_Count,
    Standardized_Residual = Residual / sqrt(Expected_Count)
  )
print(combined_counts[, c(1, 7)])

# # Compare distributions of median width
# 
# WVC_width <- WVC_gdf %>%
#   as.data.frame() %>% 
#   filter(!is.na(MedianWidth)) %>%  # Remove rows where MedianWidth is NA
#   count(MedianWidth) %>%
#   rename(Count = n)  # Rename the count column to "Count"
# sum(WVC_width$Count)
# 
# set.seed(123)  # Set a seed for reproducibility
# random_width <- merged_random_gdf %>%
#   slice_sample(n = 310) %>%
#   as.data.frame() %>% 
#   filter(!is.na(MedianWidth)) %>%  # Remove rows where MedianWidth is NA
#   count(MedianWidth) %>%
#   rename(Count = n)  # Rename the count column to "Count"
# 
# # Create contingency table for chi-square test
# contingency_table1 <- matrix(c(WVC_width$Count, random_width$Count), 
#                             ncol = 2, 
#                             byrow = FALSE,
#                             dimnames = list(WVC_width$MedianType, c("Roadkill", "Random"))
# )
# 
# # Perform chi-square test
# chisq.test(contingency_table1) # NOT APPROPRIATE, MAY REFLECT GIS METHOD NOT PICKING UP DEER HITS IN WIDE MEDIANS



# # COUNT MEDIAN TYPES
# 
# df <- WVC_gdf
# median_type <- df$MedianType
# 
# # Count median types
# median_counts <- df %>%
#   # Split the median types into separate components
#   mutate(median_type = strsplit(as.character(median_type), ", ")) %>%
#   unnest(median_type) %>%
#   # Count occurrences of each individual median type
#   group_by(median_type) %>%
#   summarise(count = n(), .groups = 'drop')
# 
# # MATCH SAMPLE SIZE
# df <- merged_random_gdf
# median_type <- df$MedianType
# 
# 
# # Randomly select 1700 rows from the dataframe
# sampled_df <- df %>%
#   slice_sample(n = 1700)
# 
# # Process the sampled rows
# processed_sample <- sampled_df %>%
#   # Split the median types into separate components
#   mutate(median_type = strsplit(as.character(median_type), ", ")) %>%
#   unnest(median_type) %>%
#   # Count occurrences of each individual median type
#   group_by(median_type) %>%
#   summarise(count = n(), .groups = 'drop')
# 
# #-------
# 
# cleaned_random_gdf <- merged_random_gdf %>%
#   # Standardize values
#   mutate(MedianType = case_when(
#     MedianType == "vegetative, thrie beam" ~ "thrie beam, vegetative",
#     MedianType == "gravel, vegetative" ~ "vegetative, gravel",
#     MedianType == "temp concrete" ~ "concrete",
#     TRUE ~ MedianType  # Retain original value if no condition is met
#   ))
# 
# random_counts <- cleaned_random_gdf %>%
#   slice_sample(n = 1100) %>%
#   as.data.frame() %>% 
#   filter(!MedianType %in% c( # Filter out non-standard values
#     # "temp concrete",
#     # "gravel, vegetative",
#     # "thrie beam, vegetative",
#     # "vegetative, gravel",
#     # "vegetative, thrie beam"
#   )) %>%
#   mutate
#   count(MedianType) %>%
#   rename(Count = n)  # Rename the count column to "Count"
# print(sum(random_counts$Count))
# 
# 
# 
# # STANDARDIZE ORDER OF AND COUNT MULTI TYPE MEDIANS
# 
# # Function to normalize the entries
# normalize_entry <- function(entry) {
#   # Split the entry into components
#   components <- unlist(strsplit(entry, ", "))
#   # Sort the components and reassemble them
#   sorted_components <- paste(sort(components), collapse = ", ")
#   return(sorted_components)
# }
# 
# # Apply normalization
# df_normalized <- df %>%
#   mutate(normalized_median_type = sapply(median_type, normalize_entry)) %>%
#   group_by(normalized_median_type) %>%
#   summarise(count = n(), .groups = 'drop')
# 
# # Print the results
# print(df_normalized)
# 
# 
# # Chi square
# contingency_table <- matrix(c(median_counts$count, median_counts1$count), 
#                             ncol = 2, 
#                             byrow = FALSE,
#                             dimnames = list(median_counts$median_type, c("Random", "Roadkill"))
#                             )
# 
# chi_squared_test <- chisq.test(contingency_table)
# 
# chi_squared_test # need take 
# 
# 
# 
# 
# 
# 
# 
# # Combine both counts into a data frame for alignment
# combined_counts <- merge(roadkill_counts, random_counts, 
#                          by.x = "MedianType", by.y = "MedianType", all = TRUE)
# 
# # Replace NA with 0 (in case some median types are not present in one of the datasets)
# combined_counts[is.na(combined_counts)] <- 0
# 
# # Filter out non-shared median types
# combined_counts %>% 
#   filter(!Var1 %in% c("overpass (open air)", "temp concrete", "thrie beam and veg"))
# 
# # Step 2: Create the contingency table
# contingency_table <- matrix(c(combined_counts$Freq.x, combined_counts$Freq.y), 
#                             ncol = 2, 
#                             byrow = FALSE,
#                             dimnames = list(combined_counts$Var1, c("Roadkill", "Random")))
# 
# # Step 3: Perform the Chi-Squared Test
# chi_squared_test <- chisq.test(contingency_table)
# 
# # Print the results of the chi-squared test
# print(chi_squared_test)
# 
# 
# # Chi-Squared Test on No Median / Veg
# 
# combined_counts <- combined_counts %>%
#   filter(Var1 %in% c("no median", "vegetative"))
# # Step 2: Create the contingency table
# contingency_table <- matrix(c(combined_counts$Freq.x, combined_counts$Freq.y), 
#                             ncol = 2, 
#                             byrow = FALSE,
#                             dimnames = list(combined_counts$Var1, c("Roadkill", "Random")))
# # Step 3: Perform the Chi-Squared Test
# chi_squared_test <- chisq.test(contingency_table)
# # Print the results of the chi-squared test
# print(chi_squared_test)




# 
# # Pie chart for WVC counts
# WVC_counts %>%
#   ggplot(aes(x = "", y = Count, fill = MedianType)) +
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar(theta = "y") +
#   labs(title = "Distribution of WVC Counts by Median Type") +
#   theme_void() +
#   theme(legend.position = "right")
# 
# # Save the WVC pie chart
# #ggsave("WVC_pie_chart.png")
# 
# 
# # Pie chart for Random Distribution counts
# random_counts %>%
#   ggplot(aes(x = "", y = Count, fill = MedianType)) +
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar(theta = "y") +
#   labs(title = "Distribution of Random Distribution by Median Type") +
#   theme_void() +
#   theme(legend.position = "right")
# 
# # Save the Random Distribution pie chart
# #ggsave("Random_Distribution_pie_chart.png")