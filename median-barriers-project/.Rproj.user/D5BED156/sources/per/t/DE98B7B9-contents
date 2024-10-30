source("scripts/d2_data_prep.R")

# DEER

deer_comb <- right_join(
  deer_gdf, deer_df,
  by = "nid"
) %>%
  select("nid", "condition", "MedianType")

unique(deer_comb$MedianType)


deer_comb <- deer_comb %>%
  as.data.frame() %>%
  mutate(MedianType = case_when(
    str_detect(MedianType, "thrie beam") ~ "thrie beam", # If "thrie beam" is anywhere in the string, change to "thrie beam"
    str_detect(MedianType, "concrete") ~ "concrete", 
    str_detect(MedianType, "cable") ~ "cable",
    str_detect(MedianType, "gravel") & str_detect(MedianType, "vegetative") ~ "vegetative",
    TRUE ~ MedianType # Keep other values as they are
  ))

unique(deer_comb$MedianType)


deer_sum <- deer_comb %>%
  #filter(!MedianType == "cable") %>%
  group_by(MedianType) %>%
  count() %>%
  rename(observed = n)

unique(deer_sum$MedianType)



random_gdf$cid <- random_gdf$CID

random_comb <- right_join(
  random_gdf, random_df,
  by = "cid"
) %>%
  select("cid", "MedianType")

unique(random_comb$MedianType)


random_comb <- random_comb %>%
  as.data.frame() %>%
  mutate(MedianType = case_when(
    str_detect(MedianType, "thrie beam") ~ "thrie beam", # If "thrie beam" is anywhere in the string, change to "thrie beam"
    str_detect(MedianType, "concrete") ~ "concrete", 
    str_detect(MedianType, "cable") ~ "cable",
    str_detect(MedianType, "gravel") & str_detect(MedianType, "vegetative") ~ "vegetative",
    TRUE ~ MedianType # Keep other values as they are
  ))

unique(random_comb$MedianType)


random_sum <- random_comb %>%
  #filter(!MedianType == "cable") %>%
  group_by(MedianType) %>%
  count() %>%
  rename(expected = n)

unique(random_sum$MedianType)

tab1 <- left_join(deer_sum, random_sum, by = "MedianType")
  
# Create contingency table with only the counts
contingency_table <- as.matrix(tab1[, c("observed", "expected")])
# Perform Chi-square test
chi_test <- chisq.test(contingency_table)
# View the results
chi_test
chi_test$expected  # View expected counts if needed

# tab1 <- tab1 %>%
#   filter(!MedianType == "cable")

# Observed frequencies for the deer counts
observed_counts <- tab1$observed 
# Expected frequencies using the random counts
expected_counts <- tab1$expected 
# Perform the Chi-square goodness-of-fit test
goodness_of_fit_test <- chisq.test(observed_counts, p = expected_counts / sum(expected_counts), simulate.p.value = FALSE)
# View the results
goodness_of_fit_test

# Calculate expected frequencies
expected_frequencies <- tab1$expected
# Calculate Pearson's residuals
observed_frequencies <- contingency_table
residuals <- (observed_frequencies - expected_frequencies) / sqrt(expected_frequencies)
# Print residuals
print("Pearson's Residuals:")
print(residuals)
# Identify significant categories
significant_categories <- abs(residuals) > 2
print("Significant Categories (True indicates significant):")
print(significant_categories)

fisher.test(contingency_table, simulate.p.value = TRUE)

colnames(contingency_table) <- c("Observed", "Expected")
rownames(contingency_table) <- c("Category 1", "Category 2", "Category 3", "Category 4", "Category 5", "Category 6")
summary(assoc(contingency_table))
mosaic(contingency_table, shade = TRUE, legend = TRUE)


# SQUIRRELS

squirrel_comb <- right_join(
  squirrel_gdf, squirrel_df,
  by = "nid"
) %>%
  select("nid", "condition", "MedianType")

unique(squirrel_comb$MedianType)


squirrel_comb <- squirrel_comb %>%
  as.data.frame() %>%
  mutate(MedianType = case_when(
    str_detect(MedianType, "thrie beam") ~ "thrie beam", # If "thrie beam" is anywhere in the string, change to "thrie beam"
    str_detect(MedianType, "concrete") ~ "concrete", 
    str_detect(MedianType, "cable") ~ "cable",
    str_detect(MedianType, "gravel") & str_detect(MedianType, "vegetative") ~ "vegetative",
    TRUE ~ MedianType # Keep other values as they are
  ))

unique(squirrel_comb$MedianType)


squirrel_sum <- squirrel_comb %>%
  #filter(!MedianType == "cable") %>%
  group_by(MedianType) %>%
  count() %>%
  rename(observed = n)

unique(squirrel_sum$MedianType)

tab2 <- left_join(squirrel_sum, random_sum, by = "MedianType")

# Create contingency table with only the counts
contingency_table <- as.matrix(tab2[, c("observed", "expected")])
# Perform Chi-square test
chi_test <- chisq.test(contingency_table)
# View the results
chi_test
chi_test$expected  # View expected counts if needed

# tab1 <- tab1 %>%
#   filter(!MedianType == "cable")

# Observed frequencies for the deer counts
observed_counts <- tab2$observed 
# Expected frequencies using the random counts
expected_counts <- tab2$expected 
# Perform the Chi-square goodness-of-fit test
goodness_of_fit_test <- chisq.test(observed_counts, p = expected_counts / sum(expected_counts), simulate.p.value = FALSE)
# View the results# View the resultsFALSE
goodness_of_fit_test

# Calculate expected frequencies
expected_frequencies <- tab1$expected
# Calculate Pearson's residuals
observed_frequencies <- contingency_table
residuals <- (observed_frequencies - expected_frequencies) / sqrt(expected_frequencies)
# Print residuals
print("Pearson's Residuals:")
print(residuals)
# Identify significant categories
significant_categories <- abs(residuals) > 2
print("Significant Categories (True indicates significant):")
print(significant_categories)

fisher.test(contingency_table, simulate.p.value = TRUE)

colnames(contingency_table) <- c("Observed", "Expected")
rownames(contingency_table) <- c("Category 1", "Category 2", "Category 3")
summary(assoc(contingency_table))
mosaic(contingency_table, shade = TRUE, legend = TRUE)
