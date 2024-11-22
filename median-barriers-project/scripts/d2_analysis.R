source("scripts/d2_data_prep.R")

# ----------------------------------------
# Prepare & Analyze Deer Data
# ----------------------------------------

# Join roadkill data to median attribute data
deer_merge <- right_join(
  deer_gdf, deer_df,
  by = "nid" # Join key
) %>%
  select("nid", "condition", "MedianType") # Select relevant columns

# Recategorize median types to dominant median type
deer_merge <- deer_merge %>%
  as.data.frame() %>%
  mutate(MedianType = case_when(
    str_detect(MedianType, "thrie beam") ~ "thrie beam", # If "thrie beam" is anywhere in the string, change to "thrie beam"
    str_detect(MedianType, "concrete") ~ "concrete", 
    str_detect(MedianType, "cable") ~ "cable",
    str_detect(MedianType, "gravel") & str_detect(MedianType, "vegetative") ~ "vegetative", # If both "gravel" and "vegetative" are present, change to "vegetative"
    TRUE ~ MedianType # Keep other values as they are
  ))

unique(deer_merge$MedianType)


# Sum roadkill observations by median type
deer_sum <- deer_merge %>%
  #filter(!MedianType == "cable") %>%
  group_by(MedianType) %>%
  count() %>%
  rename(observed = n)

unique(deer_sum$MedianType)


# Rename unique id for merge
random_gdf$cid <- random_gdf$CID

# Join random data to median attribute data
random_merge <- right_join(
  random_gdf, random_df,
  by = "cid"
) %>%
  select("cid", "MedianType")

unique(random_merge$MedianType)

# Recategorize median types to dominant median type
random_merge <- random_merge %>%
  as.data.frame() %>%
  mutate(MedianType = case_when(
    str_detect(MedianType, "thrie beam") ~ "thrie beam", # If "thrie beam" is anywhere in the string, change to "thrie beam"
    str_detect(MedianType, "concrete") ~ "concrete", 
    str_detect(MedianType, "cable") ~ "cable",
    str_detect(MedianType, "gravel") & str_detect(MedianType, "vegetative") ~ "vegetative",
    TRUE ~ MedianType # Keep other values as they are
  ))

unique(random_merge$MedianType)

# Sum random observations by median type
random_sum <- random_merge %>%
  #filter(!MedianType == "cable") %>%
  group_by(MedianType) %>%
  count() %>%
  rename(expected = n)

unique(random_sum$MedianType)

# Join roadkill and random counts by MedianType
deer_comparison <- left_join(deer_sum, random_sum, by = "MedianType")

# Calculate frequencies
deer_comparison$observed.pct <- deer_comparison$observed/sum(deer_comparison$observed)
deer_comparison$expected.pct <- deer_comparison$expected/sum(deer_comparison$expected)

# Reshape data from wide to long format
deer_comparison_long <- deer_comparison %>%
  pivot_longer(cols = c("observed.pct", "expected.pct"), 
               names_to = "Type", 
               values_to = "Percentage")

# Create bar chart
ggplot(deer_comparison_long, aes(x = MedianType, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Observed vs Expected Percentages by Median Type", x = "Median Type", y = "Percentage") +
  scale_fill_manual(values = c("skyblue", "orange"))

  
# Create contingency table with only the counts
contingency_table <- as.matrix(deer_comparison[, c("observed", "expected")])
# Perform Chi-square test
chi_test <- chisq.test(contingency_table)
# View the results
chi_test
chi_test$expected

# deer_comparison <- deer_comparison %>%
#   filter(!MedianType == "cable")

# Observed frequencies for the deer counts
observed_counts <- deer_comparison$observed 
# Expected frequencies using the random counts
expected_counts <- deer_comparison$expected 
# Perform the Chi-square goodness-of-fit test
goodness_of_fit_test <- chisq.test(observed_counts, p = expected_counts / sum(expected_counts), simulate.p.value = FALSE)
# View the results
goodness_of_fit_test


# ----------------------------------------
# Prepare & Analyze Squirrel Data
# ----------------------------------------

# Join roadkill data to median attribute data
squirrel_merge <- right_join(
  squirrel_gdf, squirrel_df,
  by = "nid"
) %>%
  select("nid", "condition", "MedianType")

unique(squirrel_merge$MedianType)

# Recategorize median types to dominant median type
squirrel_merge <- squirrel_merge %>%
  as.data.frame() %>%
  mutate(MedianType = case_when(
    str_detect(MedianType, "thrie beam") ~ "thrie beam", # If "thrie beam" is anywhere in the string, change to "thrie beam"
    str_detect(MedianType, "concrete") ~ "concrete", 
    str_detect(MedianType, "cable") ~ "cable",
    str_detect(MedianType, "gravel") & str_detect(MedianType, "vegetative") ~ "vegetative",
    TRUE ~ MedianType # Keep other values as they are
  ))

unique(squirrel_merge$MedianType)

# Sum roadkill observations by median type
squirrel_sum <- squirrel_merge %>%
  #filter(!MedianType == "cable") %>%
  group_by(MedianType) %>%
  count() %>%
  rename(observed = n)

unique(squirrel_sum$MedianType)

# Join roadkill and random counts by MedianType
squirrel_comparison <- left_join(squirrel_sum, random_sum, by = "MedianType")

# Create contingency table with only the counts
contingency_table <- as.matrix(squirrel_comparison[, c("observed", "expected")])
# Perform Chi-square test
chi_test <- chisq.test(contingency_table)
# View the results
chi_test
chi_test$expected 

# squirrel_comparison <- squirrel_comparison %>%
#   filter(!MedianType == "cable")

# Observed frequencies for the deer counts
observed_counts <- squirrel_comparison$observed 
# Expected frequencies using the random counts
expected_counts <- squirrel_comparison$expected 
# Perform the Chi-square goodness-of-fit test
goodness_of_fit_test <- chisq.test(observed_counts, p = expected_counts / sum(expected_counts), simulate.p.value = FALSE)
# View the results# View the results FALSE
goodness_of_fit_test


