
# Set paths to data directories
script_dir <- "C://Users//HP//Documents//GitHub//road-ecology-center//median_barriers//District Analysis" # Set path to folder containing RScripts
# Pull objects from data processing script
source(file.path(script_dir, "MedianDistAnalysis.R"))


# Count physical barriers from median types in WVC distribution
WVC_barriers <- WVC_counts %>%
  mutate(Barrier = ifelse(MedianType %in% # Define barriers
                            c("concrete") , "barrier", "permeable")) %>%
  group_by(Barrier) %>%
  summarise(total_count = sum(Count))

# Count physical barriers from median types in random distribution
random_barriers <- random_counts %>%
  mutate(Barrier = ifelse(MedianType %in% # Define barriers
                            c("concrete") , "barrier", "permeable")) %>%
  group_by(Barrier) %>%
  summarise(total_count = sum(Count))

contingency_table <- as.data.frame(matrix(c(WVC_barriers$total_count, random_barriers$total_count), 
                            ncol = 2, 
                            byrow = FALSE,
                            dimnames = list(WVC_barriers$Barrier, c("Roadkill", "Random"))
))

# Perform chi-square test for independence
chisq_test <- chisq.test(contingency_table)
print(chisq_test)

# Goodness of Fit

# Calculate total counts
total_observed <- sum(WVC_barriers$total_count)
total_expected <- sum(random_barriers$total_count)

# Calculate proportions in the Random group
expected_proportions <- random_barriers$total_count / total_expected

# Perform Chi-Square Goodness of Fit Test
chisq_test <- chisq.test(WVC_barriers$total_count, p = expected_proportions)
print(chisq_test)

# one tailed t test
