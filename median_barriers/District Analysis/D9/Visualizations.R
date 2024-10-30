# Set paths to data directories
script_dir <- "C://Users//HP//Documents//GitHub//road-ecology-center//median_barriers//District Analysis" # Set path to folder containing RScripts
# Pull objects from data processing script
source(file.path(script_dir, "ProcessInputData.R"))

WVC_df <- WVC_gdf %>%
  as.data.frame() %>%
  filter(animal == "Mule (or Black tailed) Deer") %>%
  mutate(MedianType = case_when( # Standardize values
    MedianType == "vegetative, thrie beam" ~ "thrie beam", # Change to thrie beam, otherwise count too low
    MedianType == "thrie beam, vegetative" ~ "thrie beam", # Change to thrie beam, otherwise count too low
    MedianType == "gravel, vegetative" ~ "vegetative",
    MedianType == "vegetative, gravel" ~ "vegetative",
    MedianType == "temp concrete" ~ "concrete",
    TRUE ~ MedianType  # Retain original value if no condition is met
  ))

WVC_grouped_df <- WVC_df %>%
  group_by(MedianType) %>%
  count() %>%
  rename(WVC_Count = n)

random_df <- merged_random_gdf %>%
  as.data.frame() %>%
  mutate(MedianType = case_when( # Standardize values
    MedianType == "vegetative, thrie beam" ~ "thrie beam", # Change to thrie beam, otherwise count too low
    MedianType == "thrie beam, vegetative" ~ "thrie beam", # Change to thrie beam, otherwise count too low
    MedianType == "gravel, vegetative" ~ "vegetative",
    MedianType == "vegetative, gravel" ~ "vegetative",
    MedianType == "temp concrete" ~ "concrete",
    TRUE ~ MedianType  # Retain original value if no condition is met
  ))

set.seed(123)
random_grouped_df <- random_df %>%
  slice_sample(n = 1100) %>%
  group_by(MedianType) %>%
  count() %>%
  rename(Random_Count = n)


median_df <- merge(WVC_grouped_df, random_grouped_df, by="MedianType") %>%
  mutate(MedianType = case_when(
    MedianType == "gravel" ~ "vegetative"
    ,
    TRUE ~ MedianType
  ))


# Reshape data from wide to long format
median_long_df <- median_df %>%
  pivot_longer(cols = c(WVC_Count, Random_Count), names_to = "Count_Type", values_to = "Count")


ggplot(median_long_df, aes(x = MedianType, y = Count, fill = Count_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Count_Type) +
  labs(y = "Count", fill = "Count Type") +
  theme_minimal()

