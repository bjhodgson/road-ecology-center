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

# Remove the specified columns
cleaned_df <- combined_df %>% select(-one_of(columns_to_remove))

# Filter out NA values from the cleaned dataframe
cleaned_df_filtered <- cleaned_df[!is.na(cleaned_df$New_ID_1), ]

# Visualize histogram of hits per median pair type
cleaned_df_filtered %>%
  group_by(Pair_Type) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Pair_Type, y = count)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Frequency of 'New_ID_1' Values", x = "Value", y = "Count") +
  theme_minimal()

# Create list of excluded median types
exclude_values <- c("tran")

# Filter out excluded median types
cleaned_df_filtered <- cleaned_df_filtered %>%
  filter(!New_ID_1 %in% exclude_values)

# Add concrete median binary variable
cleaned_df_filtered$concrete_median <- as.factor(ifelse(cleaned_df_filtered$New_ID_1 == "con", 1, 0))

# Visualize histogram of hits per concrete vs non-concrete median type
cleaned_df_filtered %>%
  group_by(concrete_median) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = concrete_median, y = count)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Number of Hits by Concrete vs Non-concrete Medians", x = "Concrete Median", y = "Count") +
  theme_minimal()

# Visualize histogram of deer hits per concrete vs non-concrete median type
cleaned_df_filtered %>%
  filter(animal == "Mule (or Black tailed) Deer") %>%
  group_by(concrete_median) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = concrete_median, y = count)) +
  geom_bar(stat = "identity", fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Number of Deer Hits by Concrete vs Non-concrete Medians", x = "Concrete Median", y = "Count") +
  theme_minimal()

# Visualize histogram of coyote hits per concrete vs non-concrete median type
cleaned_df_filtered %>%
  filter(animal == "Coyote") %>%
  group_by(concrete_median) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = concrete_median, y = count)) +
  geom_bar(stat = "identity", fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Number of Coyote Hits by Concrete vs Non-concrete Medians", x = "Concrete Median", y = "Count") +
  theme_minimal()

# Group hits by unique pair ID and median type
grouped_df <- cleaned_df[!is.na(cleaned_df$New_ID_1), ] %>%
  group_by(Pair_ID, New_ID_1) %>%
  summarise(count = n())

# Visualize histogram of hits per median type
ggplot(grouped_df, aes(x = New_ID_1)) +
  geom_bar() +
  labs(title = "Number of Hits by Median Type",
       x = "Median Type",
       y = "Count") +
  theme_minimal()

# Visualize histogram of hits per median pair
ggplot(grouped_df, aes(x = Pair_ID)) +
  geom_bar() +
  labs(title = "Number of Hits by Median Type",
       x = "Median Type",
       y = "Count") +
  theme_minimal()

# Visualize stacked bar plot of hits per transect by median type
ggplot(grouped_df, aes(x = factor(Pair_ID), y = count, fill = New_ID_1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Number of Hits per Road Transect by Median Type",
       x = "Pair_ID",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Summary statistics by grouped type
summary_stats_medtype <- grouped_df %>%
  group_by(New_ID_1) %>%
  summarise(
    mean_count = mean(count),
    median_count = median(count),
    sd_count = sd(count),
    min_count = min(count),
    max_count = max(count)
  )

# Print the results
print(summary_stats_medtype)

# Perform chi-squared test to determine association

# Create a contingency table
contingency_table <- table(cleaned_df_filtered$concrete_median)

# Perform the chi-squared test
chi_squared_test <- chisq.test(contingency_table)

# Print the results of the chi-squared test
print(chi_squared_test)




# Group hits by year and median type

# Create grouped dataframe of hits by year
year_df <- cleaned_df %>%
  filter(!is.na(New_ID_1)) %>%
  mutate(obs_year = as.factor(year(ymd_hms(observatio)))) %>%
  group_by(obs_year, Pair_ID, New_ID_1) %>%
  summarise(count = n(), .groups = 'drop')

date_range = c(2015:2023)

# Summary statistics of hits by grouped year, type
summary_stats_medtype_by_year <- year_df %>% 
  filter(obs_year %in% date_range) %>%
  group_by(New_ID_1, obs_year) %>%
  summarise(
    mean_count = mean(count),
    median_count = median(count),
    sd_count = sd(count),
    min_count = min(count),
    max_count = max(count)
  )

print(summary_stats_medtype_by_year)

# Visualize bar plot of hits by median type over time
ggplot(summary_stats_medtype_by_year, aes(x=New_ID_1, y=mean_count)) +
  geom_bar(stat = "identity", position="stack", aes(fill=New_ID_1)) +
  facet_wrap(vars(obs_year)) + 
  labs(x="", y="Average Hits per Mile", fill="Median Type") +
  theme_bw()

# Visualize stacked bar plot of hits by median type over time
year_df %>% 
  filter(obs_year %in% date_range) %>%
  group_by(New_ID_1, obs_year) %>%
  ggplot(aes(x=obs_year, y=count, fill=New_ID_1)) +
  geom_bar(stat = "identity", position="stack") + 
  labs(x="", y="Total Hits", fill="Median Type") +
  theme_bw()


# Comparison of Means

# Filter for 'con' and 'veg' counts, spread to wide format, then calculate the difference
# Add the new column 'concrete_median' with three categories using nested ifelse statements
grouped_df <- grouped_df %>%
  mutate(concrete_var = ifelse(New_ID_1 == "con", "concrete",
                                  ifelse(New_ID_1 == "tran", "transition", "nonconcrete")))

new_df <- grouped_df %>%
  pivot_wider(names_from = concrete_var, values_from = count) %>%
  mutate(difference = concrete - nonconcrete) %>%
  select(Pair_ID, difference)


new_df <- grouped_df %>%
  filter(New_ID_1 %in% c("con", "veg")) %>%
  pivot_wider(names_from = New_ID_1, values_from = count) %>%
  mutate(difference = con - veg) %>%
  select(Pair_ID, difference)

# Pivot to wide format and calculate the difference
new_df <- grouped_df %>%
  pivot_wider(names_from = concrete_var, values_from = count, values_fill = list(count = 0)) %>%
  mutate(difference = concrete - nonconcrete) %>%
  select(Pair_ID, difference)

# Print the new data frame
print(new_df)


#----------

# ggplot(summary_stats_medtype_by_year, aes(x=obs_year, y=mean_count, fill=New_ID_1)) +
#   geom_point() 
# 
# ggplot(summary_stats_medtype_by_year, aes(x=New_ID_1, y=mean_count, fill=obs_year)) +
#   geom_bar(stat = "identity", position="stack") + 
#   theme_bw()

# 
# year_df %>% 
#   filter(obs_year %in% date_range) %>%
#   group_by(New_ID_1, obs_year) %>% 
#   ggplot(aes(x=New_ID_1, y=count)) +
#   geom_bar(stat = "identity", position="stack", aes(fill=New_ID_1)) +
#   facet_wrap(vars(obs_year)) + 
#   labs(x="", y="Count", fill="Median Type") +
#   theme_bw()
