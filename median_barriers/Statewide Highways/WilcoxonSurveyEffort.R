# for survey effort calc


survey_transects <- read.csv("D:\\Downloads\\cros-survey-2024-12-18 (6).csv")
survey_observations <- read.csv("D:\\Downloads\\cros-survey-observations-2024-12-18 (4).csv")
transect_lengths <- read_xlsx("D:\\Downloads\\pairwise-data-Wilcoxon\\median-transects-survey-5mile-UPDATED.xlsx")
  

survey_transects <- survey_transects %>%
  mutate(Lane_Direction = tolower(survey_transect)) %>%
  filter(!str_detect(Lane_Direction, "transition|test"))  # Exclude rows with specified strings

survey_transects <- survey_transects %>%
  group_by(Lane_Direction) %>%
  count()

transect_lengths <- transect_lengths %>%
  mutate(Lane_Direction = tolower(Lane_Direction))

joined <- left_join(survey_transects, transect_lengths, by = "Lane_Direction")  %>%
  rename(Survey_Effort = n) 

# Define the list of Transect_IDs to remove from analysis
remove_transects_list <- list(
                              # 41,
                              # 49,
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

# Filter out Transect_IDs in the removal list
joined <- joined %>%
  filter(!Transect_ID %in% remove_transects_list)


# joined <- joined %>%
#   filter(unique(Lane_Direction))

# joined_group <- joined %>%
#   group_by(Lane_Direction) %>%
#   count()


survey_observations <- survey_observations %>%
  mutate(Lane_Direction = tolower(survey_transect))


survey_observations <- survey_observations %>%
  mutate(Lane_Direction = tolower(survey_transect)) %>%
  group_by(Lane_Direction) %>%
  count() %>%
  rename(Observation_Count = n)

# joined <- joined %>%
#   select(Lane_Direction:Length_Meters)

joined_observations <- left_join(joined, survey_observations, by = "Lane_Direction")



joined_observations <- joined_observations %>%
  mutate(Observation_Count = replace_na(Observation_Count, 0))

# joined_observations <- joined_observations %>%
#   mutate(Hit_Rate = Observation_Count / Survey_Effort / (Shape_Length/1000)) # Observation count / (Survey effort * Transect length in km)

joined_observations <- joined_observations %>%
group_by(Transect_ID) %>%
  filter(n_distinct(Median_Type) == 2) %>%  # Ensure each Transect_ID has exactly 2 distinct Median_Types
  mutate(Pair_Type = paste(sort(unique(Median_Type)), collapse = " vs ")) %>%
  ungroup()

# 
# lane_direction_counts <- joined_observations %>%
#   count(Lane_Direction)
# 
# joined_observations <- merge(joined_observations, lane_direction_counts, by = "Lane_Direction") %>%
#   rename(Observation_Count = n)


aggregated_observations <- joined_observations %>%
  group_by(Transect_ID, Median_Type) %>%
  summarize(
    Total_Survey_Effort = sum(Survey_Effort, na.rm = TRUE),
    Total_Observation_Count = sum(Observation_Count, na.rm = TRUE),
    Average_Shape_Length = mean(Shape_Length, na.rm = TRUE),
    .groups = "drop" # Ungroup after summarizing
  )


aggregated_observations <- aggregated_observations %>%
  mutate(Hit_Rate = Total_Observation_Count / Total_Survey_Effort / (Average_Shape_Length/1000)) # Observation count / (Survey effort * Transect length in km)

aggregated_observations <- aggregated_observations %>%
  group_by(Transect_ID) %>%
  filter(n_distinct(Median_Type) == 2) %>%  # Ensure each Transect_ID has exactly 2 distinct Median_Types
  mutate(Pair_Type = paste(sort(unique(Median_Type)), collapse = " vs ")) %>%
  ungroup()

filtered_observations <- aggregated_observations %>%
  select(Transect_ID, Median_Type, Pair_Type, Hit_Rate)


pivoted_observations <- filtered_observations %>%
  pivot_wider(
    id_cols = Transect_ID, 
    names_from = Median_Type, 
    values_from = c(Hit_Rate)
  )

head(pivoted_observations)

transect_pair_types <- filtered_observations %>%
  select(Transect_ID, Pair_Type)

pair_observations <- merge(pivoted_observations, transect_pair_types, by = "Transect_ID")

# # Replace NA with 0 only for columns that are part of the pair (e.g., 'concrete' for "concrete vs thrie")
# pair_observations <- pair_observations %>%
#   mutate(
#     concrete = if_else(str_detect(Pair_Type, "concrete") & is.na(concrete), 0, concrete),
#     thrie = if_else(str_detect(Pair_Type, "thrie") & is.na(thrie), 0, thrie),
#     veg = if_else(str_detect(Pair_Type, "veg") & is.na(veg), 0, veg),
#     cable = if_else(str_detect(Pair_Type, "cable") & is.na(cable), 0, cable),
#     none = if_else(str_detect(Pair_Type, "none") & is.na(none), 0, none)
#   )


pair_observations <- pair_observations %>%
  distinct()

head(pair_observations)


# Split the data by pair_type and assign each subset to a new dataframe
pair_dataframes <- pair_observations %>%
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
  
  test_result <- wilcox.test(df[[names(df)[2]]], df[[names(df)[3]]], paired = TRUE)
  
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

# Optionally, check the result for one of the pair dataframes
print(pair_dataframes_list[[7]])
