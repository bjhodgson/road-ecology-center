library(sf)
library(dplyr)
library(lubridate)

hwy <- st_read("D:\\Downloads\\CA_HPMSPR2016_TMC2017_MEDIAN-TYPE.gpkg") %>%
  as.data.frame()

sum <- hwy %>%
  group_by(MEDIAN_TYPE) %>%
  count()

hwy2020 <- st_read("H:\\General Data\\Road Features\\HPMS_CA_2020_MEDIAN-TYPE.gpkg") %>%
  as.data.frame()

sum2020 <- hwy2020 %>%
  group_by(MEDIAN_TYPE) %>%
  count()

all <- st_read("H:\\General Data\\Road Features\\HPMS_CA_2020.gdb.zip") %>%
  as.data.frame()

sumall <- all %>%
  group_by(MEDIAN_TYPE) %>%
  count()

sumall$pct <- sumall$n / sum(sumall$n) * 100

all1 <- all %>%
  filter(!is.na(MEDIAN_TYPE))

sumall1 <- all1 %>%
  group_by(MEDIAN_TYPE) %>%
  count() 

sumall1$pct <- sumall1$n / sum(sumall1$n) * 100


colnames(all)

unique(all$ROUTE_NUMBER)
unique(all$ROUTE_NAME)

length(unique(all$ROUTE_NUMBER))
length(unique(all$ROUTE_NAME))

hist(all$MEDIAN_TYPE)


hwys <- all %>%
  filter(!is.na(ROUTE_NAME))

hist(hwys$MEDIAN_TYPE)

sumhwys <- hwys %>%
  group_by(MEDIAN_TYPE) %>%
  count() 

sumhwys$pct <- sumhwys$n / sum(sumhwys$n) * 100


library(dplyr)

# Assuming 'all' is your original dataframe
ranked_routes <- all %>%
  filter(!is.na(ROUTE_NAME)) %>%  # Filter out rows with NA in ROUTE_NAME
  group_by(ROUTE_NAME) %>%
  summarise(
    total_count = n(),  # Total count of entries for each ROUTE_NAME
    na_count = sum(is.na(MEDIAN_TYPE)),  # Count of NA in MEDIAN_TYPE
    na_pct = (na_count / total_count) * 100  # Percentage of NA values
  ) %>%
  arrange(desc(total_count), na_pct)  # Rank by total_count desc, then na_pct asc

# Display the ranked dataframe
print(ranked_routes)



plot(ranked_routes$total_count, ranked_routes$na_pct)



# Find the highest total_count
max_count <- ranked_routes %>%
  summarise(max_total_count = max(total_count)) %>%
  pull(max_total_count)

# Filter for routes with the highest total_count
highest_routes <- ranked_routes %>%
  filter(total_count == max_count)

# Find the route with the lowest na_pct among those
result <- highest_routes %>%
  arrange(na_pct) %>%
  slice(1)  # Get the first one, which has the lowest na_pct

# Display the result
print(result)



# do spatial join
library(sf)
library(dplyr)
library(lubridate)
roadkill <- st_read("H:\\General Data\\Roadkill\\CROS-CHIPS-20240410-ND-All\\CROS-CHIPS-20240410-ND-All.shp")

roadkill2020 <- roadkill %>%
  mutate(observatio = ymd_hms(observatio)) %>%
  filter(year(observatio) == 2020)



highways <- st_read("D:\\Downloads\\CA_HPMS_2020_SHS.gpkg")

# plot(highways$geom)
# plot(roadkill2020$geometry)

length(unique(highways$ROUTE_NAME))
length(unique(highways$ROUTE_NUMBER))
length(unique(highways$Route_ID))

unique(highways$NHS)


# Transform both layers to EPSG 3310 (California Albers)
roadkill2020 <- st_transform(roadkill2020, crs = 3310) %>% st_zm()
highways <- st_transform(highways, crs = 3310) %>% st_zm()

st_crs(roadkill2020)
st_crs(highways)

# Create a 50-meter buffer around highways
highways_buffer <- st_buffer(highways, dist = 50)

# Clip roadkill points to the 50-meter buffer
clipped_roadkill <- st_intersection(roadkill2020, highways_buffer) 

clipped_roadkill <- clipped_roadkill %>%
  filter(!is.na(Route_ID))

# plot(clipped_roadkill$geometry)
# 
# hist(clipped_roadkill$MEDIAN_TYPE)
# 
# hist(highways$MEDIAN_TYPE)



roadkill_sum <- clipped_roadkill %>%
  as.data.frame() %>%
  filter(!is.na(MEDIAN_TYPE)) %>%
  group_by(MEDIAN_TYPE) %>%
  count() 

roadkill_sum$pct <- roadkill_sum$n / sum(roadkill_sum$n) *100
roadkill_sum

highways_sum <- highways %>%
  as.data.frame() %>%
  filter(!is.na(MEDIAN_TYPE)) %>%
  group_by(MEDIAN_TYPE) %>%
  count()

highways_sum$pct <- highways_sum$n / sum(highways_sum$n) *100
highways_sum


library(dplyr)
library(ggplot2)
library(tidyr)

# Combine the data frames
roadkill_sum <- clipped_roadkill %>%
  as.data.frame() %>%
  filter(!is.na(MEDIAN_TYPE)) %>%
  group_by(MEDIAN_TYPE) %>%
  count()
roadkill_sum$pct <- roadkill_sum$n / sum(roadkill_sum$n) *100
roadkill_sum$dataset <- "Roadkill"

highways_sum <- highways %>%
  as.data.frame() %>%
  filter(!is.na(MEDIAN_TYPE)) %>%
  group_by(MEDIAN_TYPE) %>%
  count() 
highways_sum$pct <- highways_sum$n / sum(highways_sum$n) *100
highways_sum$dataset <- "Highways"


# Combine the two dataframes
combined_sum <- rbind(roadkill_sum, highways_sum)

# Create the plot
ggplot(combined_sum, aes(x = MEDIAN_TYPE, y = pct, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of MEDIAN_TYPE by Dataset", 
       x = "MEDIAN_TYPE", 
       y = "Percentage (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Load necessary library
library(ggplot2)

# Set up a multi-panel plotting area
par(mfrow = c(1, 2))  # 1 row, 2 columns

# Plot histogram for clipped_roadkill
hist(clipped_roadkill$MEDIAN_TYPE, 
     main = "Clipped Roadkill MEDIAN_TYPE", 
     xlab = "MEDIAN_TYPE", 
     col = "lightblue", 
     border = "black")

# Plot histogram for highways
hist(highways$MEDIAN_TYPE, 
     main = "Highways MEDIAN_TYPE", 
     xlab = "MEDIAN_TYPE", 
     col = "lightgreen", 
     border = "black")

# Reset to default single-panel plotting
par(mfrow = c(1, 1))




# Create the contingency table
contingency_table <- combined_sum %>%
  select(MEDIAN_TYPE, n, dataset) %>%  # Select relevant columns
  pivot_wider(names_from = dataset, values_from = n, values_fill = 0)  # Reshape data

# Display the contingency table
print(contingency_table)

# Remove MT 4 (catchall / misc)
contingency_table <- contingency_table %>%
  filter(!MEDIAN_TYPE == 4)

# Create observed counts for Roadkill
observed_counts <- contingency_table$Roadkill  # Counts from Roadkill

# Create expected counts based on Highways
expected_counts <- contingency_table$Highways / sum(contingency_table$Highways) * sum(observed_counts)

# Combine observed and expected counts
data_for_test <- rbind(observed_counts, expected_counts)

# Perform the Chi-square test
chi_square_result <- chisq.test(data_for_test)

# Display the result
print(chi_square_result)


deer <- clipped_roadkill %>%
  filter(animal == "Mule (or Black tailed) Deer")

deer1 <- deer %>%
  filter(condition %in% c("Dead", "Injured"))

predictor_vars <- c("MEDIAN_TYPE", "AADT", "MEDIAN_WIDTH")

unique(deer$AADT)


deer <- deer %>%
  mutate(AADT_bin = cut(AADT, breaks = seq(0, max(AADT), by = 50000), include.lowest = TRUE))

deer <- deer %>%
  as.data.frame() %>%
  select(!geometry) %>%
  mutate(MEDIAN_TYPE = as.factor(MEDIAN_TYPE)) %>%
  filter(!MEDIAN_TYPE %in% c("3", "4", "5"))


deer$roadkill_count <- 1

glm_df <- deer %>%
  as.data.frame() %>%
  #select(!geometry) %>%
  group_by(MEDIAN_TYPE, AADT_bin) %>%
  summarize(roadkill_count = n())

# Fit a Poisson regression model
poisson_model <- glm(roadkill_count ~ MEDIAN_TYPE + AADT_bin, family = poisson(link = "log"), data = glm_df)
summary(poisson_model)


predictors <- c("AADT", 
                "MEDIAN_TYPE", 
                "MEDIAN_WIDTH", 
                "LANE_WIDTH", 
                "roadkill")

roadkill_df <- clipped_roadkill %>%
  as.data.frame() %>%
  mutate(roadkill = 1) %>%
  select(all_of(predictors))

random_df <- highways %>%
  as.data.frame() %>%
  mutate(roadkill = 0) %>%
  select(all_of(predictors))

glm_df <- rbind(roadkill_df, random_df) #%>%
#   filter(MEDIAN_TYPE != 3) %>%
#   filter(MEDIAN_TYPE != 4) %>%
#   filter(MEDIAN_TYPE != 5)



glm_summary <- glm_df %>%
  group_by(roadkill, MEDIAN_TYPE) %>%
  count()

# Fit a logistic regression model
logistic_model <- glm(roadkill ~ as.factor(MEDIAN_TYPE), family = binomial(link = "logit"), data = glm_df)
summary(logistic_model)
exp(coef(logistic_model))


# Fit a logistic regression model
logistic_model <- glm(roadkill ~ as.factor(MEDIAN_TYPE) + MEDIAN_WIDTH + AADT + LANE_WIDTH, family = binomial(link = "logit"), data = glm_df)
summary(logistic_model)
exp(coef(logistic_model))


# logistic regression more appropriate, need random points back

# Generate random points
random <- st_read("D:\\Downloads\\CA_HPMS_2020_SHS.gpkg")
head(random)

#st_crs(random, 3310)

# Clip roadkill points to the 50-meter buffer
clipped_random <- st_join(random, highways, join = st_is_within_distance, dist = 2)


head(clipped_random)


highways <- st_transform(highways, 3310) %>% st_zm()
st_crs(highways)

# Set the seed for reproducibility
set.seed(123)  # Replace 123 with any number you choose

# Dissolve the lines
dissolved_line <- highways %>% 
  st_union() %>%          # Union all geometries
  st_sf()                 # Convert back to an sf object

# Convert to LINESTRING if it's a MULTILINESTRING
dissolved_line <- st_cast(dissolved_line, "LINESTRING")

# Specify the number of random points you want
n_points <- 100

# Generate random points along the dissolved line
random_points <- st_line_sample(dissolved_line, sample = runif(n_points, 0, 1))

# Convert the sample points to an sf object
random_points_sf <- st_sf(geometry = random_points)

# Plot the results
plot(st_geometry(dissolved_line), col = 'blue', main = "Random Points Along Dissolved Line")
plot(random_points_sf, col = 'red', add = TRUE)


