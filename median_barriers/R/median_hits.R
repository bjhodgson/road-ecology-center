# Install and load necessary packages
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Function to list xlsx and xls files in a given directory
list_excel_files <- function(directory) {
  xls_files <- list.files(path = directory, pattern = "*.xls", full.names = TRUE)
  c(xls_files)
}

# Specify the path to your folder containing Excel files
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

# Remove the specified columns and filter by hits
cleaned_df <- combined_df %>% 
  select(-one_of(columns_to_remove)) %>%
  filter(chips_An_1 %in% c("Fatality, result of collision", "Fatality, result of dispatch", "Injury") |
          condition %in% c("Dead", "Injured") ) # Filter by hits

# Histogram of total hits per median type
cleaned_df %>%
  group_by(New_ID_1) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = New_ID_1, y = count, fill = New_ID_1)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Hits per Median Type", x = "", y = "Total Hits", fill = "Median Type") +
  theme_minimal()

# Create list of excluded median types
exclude_values <- c("tran")

# Filter out excluded median types
cleaned_df_filtered <- cleaned_df %>%
  filter(!New_ID_1 %in% exclude_values) # Filter out median transitions

# Histogram of number of median pair types
cleaned_df_filtered %>%
  group_by(Pair_Type) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Pair_Type, y = count)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Type of Median Pair", y = "Number of hits") +
  theme_minimal()

# Add concrete median binary variable
cleaned_df_filtered$concrete_median <- as.factor(ifelse(cleaned_df_filtered$New_ID_1 == "con", 1, 0))

# Calculate total hit counts by concrete/non-concrete medians
total_counts_df <- cleaned_df_filtered %>%
  group_by(concrete_median) %>%
  summarise(count = n()) %>%
  mutate(difference = sum(count[concrete_median == 1]) - sum(count[concrete_median == 0]))


# Histogram of hits per concrete vs non-concrete median type
total_counts_df %>%
  ggplot(aes(x = concrete_median, y = count)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Hits by Concrete vs Non-concrete Medians", x = "Concrete Median", y = "Total Hits") +
  theme_minimal()




# Group hits by unique pair ID and median type
grouped_df <- cleaned_df[!is.na(cleaned_df$New_ID_1), ] %>%
  group_by(Pair_ID, New_ID_1) %>%
  summarise(count = n())

# Summary statistics by grouped median type
summary_stats_df <- grouped_df %>%
  group_by(New_ID_1) %>%
  summarise(
    mean_count = mean(count), # Average across all transects
    median_count = median(count),
    sd_count = sd(count),
    min_count = min(count),
    max_count = max(count)
  )

# Print the results
print(summary_stats_df)

# Stacked bar plot of hits per transect by median type
ggplot(grouped_df, aes(x = factor(Pair_ID), y = count, fill = New_ID_1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Number of Hits per Road Transect by Median Type",
       x = "Median Pair ID",
       y = "Total Hits",
       fill = "Median Type") +
  theme_minimal() #+
  scale_fill_brewer(palette = "Set1")

# Difference in counts (concrete - nonconcrete)
difference_df <- grouped_df %>%
  filter(New_ID_1 %in% c("con", "veg", "thrie", "cab", "none")) %>% # Filter out transition transects
  mutate(concrete_var = ifelse(New_ID_1 == "con", "concrete", "nonconcrete")) %>% # Create concrete binary variable
  pivot_wider(names_from = concrete_var, values_from = count, values_fill = list(count = 0)) %>% # Pivot to wide format
  group_by(Pair_ID) %>%
  summarise( # Summarize to get total counts for 'concrete' and 'nonconcrete' per Pair_ID
    concrete = sum(concrete, na.rm = TRUE),
    nonconcrete = sum(nonconcrete, na.rm = TRUE),
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
  ggtitle("Difference in Hit Counts (concrete - nonconcrete)") +
  geom_vline(aes(xintercept = mean(difference)), color = "red", linetype = "dashed", size = 1.5) + # Add line for mean difference
  theme_bw()

# Summary statistics
summary(difference_df$difference)
sd(difference_df$difference) # Std Deviation


# Group hits by year and median type

# Create grouped dataframe of hits by year
year_df <- cleaned_df %>%
  filter(!is.na(New_ID_1)) %>%
  mutate(obs_year = as.factor(year(ymd_hms(observatio)))) %>%
  group_by(obs_year, Pair_ID, New_ID_1) %>%
  summarise(count = n(), .groups = 'drop')

date_range = c(2015:2023)

# Summary statistics of hits by grouped year, median type
summarized_df <- year_df %>% 
  group_by(New_ID_1, obs_year) %>%
  summarise(
    total_count = sum(count), # Total count
    mean_count = mean(count), # Average count across number of transects
    median_count = median(count),
    sd_count = sd(count),
    min_count = min(count),
    max_count = max(count),
    n_transects = sum(length(unique(Pair_ID))), # Number of transects
    weighted_avg_count = sum(count * length(unique(Pair_ID))) / sum(length(unique(Pair_ID))) # Weighted average across number of transects
  )

# Stacked bar plot of TOTAL hits by median type over time
summarized_df %>%
  filter(obs_year %in% date_range) %>%
  group_by(New_ID_1, obs_year) %>%
  ggplot(aes(x=obs_year, y=total_count, fill=New_ID_1)) +
  geom_bar(stat = "identity", position="stack") + 
  labs(x="", y="Total Hits", fill="Median Type") +
  theme_bw()

# Bar plot of TOTAL hits by median type over time
summarized_df %>%
  filter(obs_year %in% date_range) %>%
  ggplot(aes(x=New_ID_1, y=total_count)) +
  geom_bar(stat = "identity", position="stack", aes(fill=New_ID_1)) +
  facet_wrap(vars(obs_year)) + 
  labs(title="Total hits", x="", y="Total hits per Mile", fill="Median Type") +
  theme_bw()

# Bar plot of AVERAGE hits by median type over time WITH total transect count
summarized_df %>%
  filter(obs_year %in% date_range) %>%
  ggplot(aes(x=New_ID_1, y=mean_count)) +
  geom_bar(stat = "identity", position="stack", aes(fill=New_ID_1)) +
  geom_text(aes(label = n_transects), position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(vars(obs_year)) + 
  labs(title = "Average Hits with Number of Transects", x="", y="Average hits per Mile", fill="Median Type") +
  theme_bw()

# Bar plot of WEIGHTED AVERAGE kills by median type over time (weighting by # of transects, n)
summarized_df %>%
  filter(obs_year %in% date_range) %>%
  ggplot( aes(x = New_ID_1, y = weighted_avg_count, fill = New_ID_1)) +
  geom_bar(stat = "identity") +
  facet_wrap(vars(obs_year)) +
  labs(title = "Weighted Average Hits", x = "Category", y = "Weighted Average Hits per Mile", fill = "Median Type") +
  theme_bw()

# Bar plot of AVERAGE hits by concrete/non-concrete median type over time
summarized_df %>%
  filter(obs_year %in% date_range) %>%
  mutate(concrete_var = ifelse(New_ID_1 == "con", "concrete", "nonconcrete")) %>% # Create concrete binary variable
  ggplot(aes(x = concrete_var, y = mean_count, fill = concrete_var)) +
  geom_bar(stat = "identity") +
  facet_wrap(vars(obs_year)) +
  labs(title = "Average Hits by Concrete/Non-concrete Medians", x = "Median Type", y = "Average Hits per Mile", fill = "Median Type") +
  theme_bw()


# Scatterplot of total hits by median type over time
summarized_df %>%
 # filter(New_ID_1 == "veg") %>%
  mutate(obs_year = as.numeric(as.character(obs_year))) %>%
  ggplot(aes(x = obs_year, y = total_count, color = New_ID_1)) +
  geom_point() + 
  labs(x = "", y = "Total hits per Mile", color = "Median Type") +
  theme_bw()
