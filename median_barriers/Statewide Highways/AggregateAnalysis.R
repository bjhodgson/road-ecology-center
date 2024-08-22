# Install and load necessary packages
library(dplyr)
library(lubridate)
library(tidyr)

script_dir <- "C:\\Users\\HP\\Documents\\GitHub\\road-ecology-center\\median_barriers\\Statewide Highways"
source(file.path(script_dir, "ProcessMedianData.R"))

# Compare aggregate hits by pair type

# # Filter by hits (DONE IN PROCSESING STEP)
# cros_df <- cros_df %>%
#   filter(chips_An_1 %in% c("Fatality, result of collision", "Fatality, result of dispatch", "Injury") |
#            condition %in% c("Dead", "Injured") ) # Filter by hits

# Count WVCs by transect type 
cros_df <- cros_df %>%
  group_by(Pair_Name, Pair_Type, MedianType) %>%
  filter(!MedianType %in% c("transition")) %>% # Filter out transition hits
  count()

# Sum the WVCs for each transect type
summarized_cros_df <- cros_df %>%
  group_by(Pair_Type, MedianType) %>%
  summarize(TotalHits = sum(n), .groups = 'drop') 
  #filter(!TotalHits <= 10)

# Sum the segment length for each transect type
summarized_segments_df <- segments_df %>%
  rename("MedianType" = Primary_Me) %>%
  group_by(Pair_Type, MedianType) %>%
  count() %>%
  rename("Distance100m" = n) %>%
  filter(!MedianType == "transition") # Remove any remaining transition records

# Merge summarized dfs to create hits with transect distance
summarized_df <- merge(summarized_cros_df, summarized_segments_df, by = c("Pair_Type", "MedianType"))
  
# Adjust WVC count by transect distance  
summarized_df <- summarized_df %>%
  mutate(HitsPer100m = TotalHits / Distance100m) %>%
  mutate(Distance1km = Distance100m*100 / 1000) %>%
  mutate(HitsPer1km = TotalHits / Distance1km)

#print(summarized_df[,c(1,2,7)])

# Perform Kruskal-Wallis Test (non-parametric)
kruskal_result <- kruskal.test(HitsPer100m ~ MedianType, data = summarized_df)
print(kruskal_result)

