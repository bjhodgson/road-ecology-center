library(dplyr)
library(tidyr)

script_dir <- "C:\\Users\\HP\\Documents\\GitHub\\road-ecology-center\\median_barriers\\Statewide Highways"
source(file.path(script_dir, "process2.R"))

# Perform type comparison on each unique pair

# STEP 1: Reshape df to unique rows by transect 

# Count segment length by unique median transect 
grouped_segments_df <- segments_df %>%
  rename("MedianType" = Primary_Me) %>%
  group_by(Pair_Name, Pair_Type, MedianType) %>%
  count(MedianType) %>%
  mutate("Distance1km" = n*100/1000) # Normalize to 1km distance
  
# Create a pairing column
paired_df <- grouped_segments_df %>%
  mutate(Pair_Column = paste(MedianType, collapse = "_")) %>%
  pivot_wider(
    names_from = Pair_Name,
    values_from = c(n, Distance1km),
    names_prefix = "pair_"
  ) %>%
  separate(Pair_Column, into = c("Left_MedianType", "Right_MedianType"), sep = "_")

# Create a dataframe with left and right pairs
pivoted_df <- grouped_segments_df %>%
  arrange(Pair_Name, Pair_Type, MedianType) %>%
  group_by(Pair_Name, Pair_Type) %>%
  mutate(position = row_number()) %>%
  pivot_wider(
    names_from = c(MedianType, position),
    values_from = c(n, Distance1km),
    names_sep = "_",
    names_glue = "{MedianType}_{.value}_{position}"
  ) %>%
  # Rename columns for clarity
  rename(
    MedianType_Left = `concrete_n_1`,
    MedianType_Right = `gravel_n_2`,
    Roadkill_Left = `concrete_n_1`,
    Roadkill_Right = `gravel_n_2`,
    Segment_Length_Left = `concrete_Distance1km_1`,
    Segment_Length_Right = `gravel_Distance1km_2`
  )

# Prepare segment data for merge
merge_segment_df <- segment_count %>%
  filter(!Primary_Me == "transition") %>% # Remove "transition" records
  group_by(Pair_Name, Pair_Type) %>%
  summarise(
    MedianType_A = first(Primary_Me),
    MedianType_B = last(Primary_Me),
    Distance1km_A = sum(Distance100m[Primary_Me == MedianType_A]*100/1000),
    Distance1km_B = sum(Distance100m[Primary_Me == MedianType_B]*100/1000),
  )

# Prepare CROS data for merge
merge_cros_df <- grouped_cros_df %>%
  filter(MedianType != "transition") %>% # Remove "transition" records
  group_by(Pair_Name, Pair_Type) %>%
  summarise(
    MedianType_A = strsplit(Pair_Type, "/")[[1]][1],
    MedianType_B = strsplit(Pair_Type, "/")[[1]][2],
    Hits_A = sum(n[MedianType == strsplit(Pair_Type, "/")[[1]][1]], na.rm = TRUE),
    Hits_B = sum(n[MedianType == strsplit(Pair_Type, "/")[[1]][2]], na.rm = TRUE),
    .groups = 'drop'  # Ungroup after summarizing
  ) %>%
  mutate(
    Hits_B = if_else(is.na(Hits_B), 0, Hits_B)  # Ensure Hits_B is 0 when no full pair
  )

# Merge segment and CROS data
merged_df <- merge(merge_cros_df, merge_segment_df, 
                   by=c("Pair_Name", "Pair_Type")) # Should be ~77 pairs

# Clean up dataframe
merged_df <- merged_df %>% 
  select(!c("MedianType_A.y", "MedianType_B.y")) %>% # Remove duplicate columns
  rename( # Rename columns
    MedianType_A = MedianType_A.x,
    MedianType_B = MedianType_B.x
    
  )
  
# Filter unwanted pairs
merged_df <- merged_df %>%
  filter(!Pair_Name %in% c(
    "Willows", #veg/veg, remove for now
    "Orland", #veg/veg, remove for now
    "I80; Forebay", #median too wide
    "I80; Lake Spalding" #median too wide
    )) %>% 
  mutate(
    HitsPer1km_A = Hits_A / Distance1km_A,
    HitsPer1km_B = Hits_B / Distance1km_B,
    HitRateDiff = HitsPer1km_A - HitsPer1km_B
  ) 

# Create wilcoxon test df by pair type
wilcox_df <- merged_df %>% # maybe do barrier/non barrier for all
  filter(Pair_Type == "thrie/vegetative")

# STEP 2: Compare WVC counts within each pair

# Perform paired t-test (parametric) or Wilcoxon signed-rank test (non-parametric)
# t_test_result <- t.test(merged_df$HitsPer1km_A, merged_df$HitsPer1km_B, paired = TRUE)
# print(t_test_result)

wilcox_test <- wilcox.test(wilcox_df$HitsPer1km_A, wilcox_df$HitsPer1km_B, paired = TRUE, conf.int = TRUE)
print(wilcox_test)

unique(merged_df$Pair_Type)

# Initialize an empty dataframe to store the results
wilcox_results_df <- data.frame(
  Pair_Type = character(),
  Test_Statistic = numeric(),
  P_Value = numeric(),
  Conf_Int_Lower = numeric(),
  Conf_Int_Upper = numeric(),
  stringsAsFactors = FALSE
)

# Get unique pair types from the dataframe
pair_types <- unique(merged_df$Pair_Type)

# Loop through each pair type and perform the Wilcoxon signed-rank test
for (pair in pair_types) {
  # Filter the dataframe for the current pair type
  wilcox_df <- merged_df %>% filter(Pair_Type == pair)
  
  # Perform the Wilcoxon signed-rank test
  wilcox_test <- wilcox.test(wilcox_df$HitsPer1km_A, wilcox_df$HitsPer1km_B, paired = TRUE, conf.int = TRUE)
  
  # Append the results to the dataframe
  wilcox_results_df <- rbind(wilcox_results_df, data.frame(
    Pair_Type = pair,
    Test_Statistic = wilcox_test$statistic,
    P_Value = wilcox_test$p.value,
    Conf_Int_Lower = wilcox_test$conf.int[1],
    Conf_Int_Upper = wilcox_test$conf.int[2]
  ))
}

# Print the results dataframe
print(wilcox_results_df)


# Calculate means or medians
mean_hits <- merged_df %>%
  group_by(Pair_Type) %>%
  summarise(
    Mean_HitsPer1km_A = mean(HitsPer1km_A, na.rm = TRUE),
    Mean_HitsPer1km_B = mean(HitsPer1km_B, na.rm = TRUE),
    Median_HitsPer1km_A = median(HitsPer1km_A, na.rm = TRUE),
    Median_HitsPer1km_B = median(HitsPer1km_B, na.rm = TRUE)
  )

# Print the results
print(mean_hits)



test <- merged_df %>%
  filter(grepl("concrete", Pair_Type)) %>%
  filter(!Pair_Type == "concrete/thrie")

wilcox.test(test$HitsPer1km_A, test$HitsPer1km_B, paired = TRUE, conf.int = TRUE)

test <- merged_df %>%
  filter(grepl("/vegetative", Pair_Type)) %>%
  filter(!grepl("concrete", Pair_Type))

wilcox.test(test$HitsPer1km_A, test$HitsPer1km_B, paired = TRUE, conf.int = TRUE)

test <- merged_df %>%
  filter(grepl("/vegetative", Pair_Type))

wilcox.test(test$HitsPer1km_A, test$HitsPer1km_B, paired = TRUE, conf.int = TRUE)


# # STEP 3: Compare aggregate counts across all pairs by median type
# 
# # Reshape data for independent testing
# aggregated_df <- merged_df %>%
#   select(MedianType_A, HitsPer1km_A, MedianType_B, HitsPer1km_B) %>%
#   pivot_longer(cols = starts_with("HitsPer1km"), 
#                names_to = "MedianType_Side", 
#                values_to = "HitsPer1km") %>%
#   mutate(MedianType = ifelse(grepl("_A$", MedianType_Side), MedianType_A, MedianType_B)) %>%
#   group_by(MedianType) %>%
#   summarise(Aggregated_HitsPer1km = sum(HitsPer1km, na.rm = TRUE))
# 
# # Perform Kruskal-Wallis test (non-parametric)
# kruskal_test_result <- kruskal.test(Aggregated_HitsPer1km ~ MedianType, data = aggregated_df)
# print(kruskal_test_result)


