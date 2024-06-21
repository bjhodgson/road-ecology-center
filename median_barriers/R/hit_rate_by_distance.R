# Install and load necessary packages
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Read in segment lengths
segment_df <- read.csv("D:\\median_barriers\\sheets\\medians_cleaned.csv") %>%
    filter(Complete != "Deleted") # Filter out deleted transects

# Function to list xlsx and xls files in a given directory
list_excel_files <- function(directory) {
  xls_files <- list.files(path = directory, pattern = "*.xls", full.names = TRUE)
  c(xls_files)
}

# Specify the path to your folder containing Excel files
directory_path <- "C:\\Users\\HP\\Documents\\GitHub\\road-ecology-center\\median_barriers\\CROS_medians_dataset"
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

# Remove the specified columns and filter by hits
cleaned_df <- combined_df %>% 
  select(-one_of(columns_to_remove)) %>%
  filter(chips_An_1 %in% c("Fatality, result of collision", "Fatality, result of dispatch", "Injury") |
           condition %in% c("Dead", "Injured") ) # Filter by hits

# Group hits by unique pair ID and median type
grouped_df <- cleaned_df[!is.na(cleaned_df$New_ID_1), ] %>%
  rename(New_ID = New_ID_1) %>% #Rename New_ID column to act as join key
  group_by(Pair_ID, Pair_Name, New_ID) %>%
  summarise(count = n()) %>%
  merge(segment_df, by=c("Pair_Name","New_ID")) %>% #Join df's by key
  mutate(hits_per_100m = count / Segment_Count) %>%
  filter(!New_ID == "tran") # Exclude transition segments
  
# Summary statistics by grouped median type
summary_stats_df <- grouped_df %>%
  group_by(New_ID) %>%
  summarise(
    mean_hitrate = mean(hits_per_100m), # Average across all transects
    median_hitrate = median(hits_per_100m),
    sd_hitrate = sd(hits_per_100m),
    min_hitrate = min(hits_per_100m),
    max_hitrate = max(hits_per_100m)
  )


#________________



# Stacked bar plot of hits per transect by median type
ggplot(grouped_df, aes(x = factor(Pair_ID), y = hits_per_100m, fill = New_ID)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Hits per 100 meters",
       x = "Transect Pair ID",
       y = "Hit Rate",
       fill = "Median Type") +
  theme_minimal() #+
scale_fill_brewer(palette = "Set1")

# Difference in rates (concrete - nonconcrete)
difference_df <- grouped_df %>%
  filter(New_ID %in% c("con", "veg", "thrie", "cab", "none")) %>% # Filter out transition transects
  mutate(concrete_var = ifelse(New_ID == "con", "concrete", "nonconcrete")) %>% # Create concrete binary variable
  pivot_wider(names_from = concrete_var, values_from = hits_per_100m, values_fill = list(hits_per_100m = 0)) %>% # Pivot to wide format
  group_by(Pair_ID) %>%
  summarise( # Summarize to get total rates for 'concrete' and 'nonconcrete' per Pair_ID
    concrete = sum(concrete, na.rm = TRUE), # Sum function selects only concrete rates
    nonconcrete = sum(nonconcrete, na.rm = TRUE), # Sum function selects only nonconcrete rates
    difference = concrete - nonconcrete
  ) %>%
  select(Pair_ID, concrete, nonconcrete, difference) # Select desired columns for dataframe

# View the data frame
print(difference_df)


# Exploratory visualizations of difference in counts 

# Visualize distribution in boxplot
boxplot(difference_df$difference)

# Visualize distribution in density
ggplot(data = difference_df, aes(x = difference)) +
  geom_density() +
  ggtitle("Difference in Hit Rates (concrete - nonconcrete)") +
  geom_vline(aes(xintercept = mean(difference)), color = "red", linetype = "dashed", size = 1.5) + # Add line for mean difference
  theme_bw()

# Summary statistics
summary(difference_df$difference)
sd(difference_df$difference) # Std Deviation

#-----------------------

# Group hits by year and median type

# Create grouped dataframe of hits by year
year_df <- cleaned_df %>%
  rename(New_ID = New_ID_1) %>%
  filter(!is.na(New_ID)) %>%
  merge(segment_df, by=c("Pair_Name", "New_ID")) %>%
  mutate(obs_year = as.factor(year(ymd_hms(observatio)))) #%>%
  group_by(obs_year, Pair_ID, New_ID) %>%
  summarise(count = n(), .groups = 'drop')

date_range = c(2015:2023)

# Summary statistics of hits by grouped year, median type
summarized_df <- year_df %>% 
  filter(!New_ID == "tran") %>% # Exclude transition segments
  group_by(New_ID, obs_year) %>% 
  #summarise(count = n()) %>%
  #mutate(hits_per_100m = n() / Segment_Count) %>%
  summarise(
    hit_rate = n() / Segment_Count,
    #total_count = sum(hits_per_100m), # Total count
    # mean_rate = mean(hits_per_100m), # Average count across number of transects
    # median_rate = median(hits_per_100m),
    # sd_rate = sd(hits_per_100m),
    # min_rate = min(hits_per_100m),
    # max_rate = max(hits_per_100m),
    # n_transects = sum(length(unique(Pair_ID))), # Number of transects
    #weighted_avg_count = sum(hits_per_100m * length(unique(Pair_ID.x))) / sum(length(unique(Pair_ID.x))) # Weighted average across number of transects
  )

# Stacked bar plot of hit rate by median type over time
# summarized_df %>%
#   filter(obs_year %in% date_range) %>%
#   #group_by(New_ID, obs_year) %>%
#   ggplot(aes(x=obs_year, y=hit_rate, fill=New_ID)) +
#   geom_bar(stat = "identity", position="stack") + 
#   labs(x="", y="Hit Rate", fill="Median Type") +
#   theme_bw()

# Bar plot of hit rate by median type over time
summarized_df %>%
  filter(obs_year %in% date_range) %>%
  ggplot(aes(x=New_ID, y=hit_rate)) +
  geom_bar(stat = "identity", position="stack", aes(fill=New_ID)) #+
  facet_wrap(vars(obs_year)) + 
  labs(title="Hit Rate", x="", y="Hits per 100m", fill="Median Type") +
  theme_bw()


  
  #________
  
  summarized_df <- year_df %>% 
    filter(New_ID != "tran") %>%  # Exclude "tran"
    group_by(New_ID, obs_year) %>%       # Group by New_ID and obs_year
    summarise(
      hit_rate = sum(New_ID == "con") / first(Segment_Count)  # Calculate hit_rate as number of "con" hits / Segment_Count for the first occurrence in each group
    )

