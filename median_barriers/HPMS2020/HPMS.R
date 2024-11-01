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

deer <- roadkill2020 %>%
  filter(animal == "Mule (or Black tailed) Deer")



highways <- st_read("D:\\Downloads\\HPMS_CA_2020_Highways.gpkg")

# plot(highways$geom)
# plot(roadkill2020$geometry)


# Transform both layers to EPSG 3310 (California Albers)
roadkill2020 <- st_transform(roadkill2020, crs = 3310) %>% st_zm()
highways <- st_transform(highways, crs = 3310) %>% st_zm()

st_crs(roadkill2020)
st_crs(highways)

# Create a 50-meter buffer around highways
highways_buffer <- st_buffer(highways, dist = 50)

# Clip roadkill points to the 50-meter buffer
clipped_roadkill <- st_intersection(roadkill2020, highways_buffer)

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


library(ggplot2)
library(gridExtra)

# Create individual plots
plot1 <- ggplot(clipped_roadkill, aes(x = MEDIAN_TYPE)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Clipped Roadkill MEDIAN_TYPE", x = "MEDIAN_TYPE", y = "Count")

plot2 <- ggplot(highways, aes(x = MEDIAN_TYPE)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 30) +
  labs(title = "Highways MEDIAN_TYPE", x = "MEDIAN_TYPE", y = "Count")

# Arrange plots side by side
grid.arrange(plot1, plot2, ncol = 2)




# Set up a multi-panel plotting area
par(mfrow = c(1, 2))  # 1 row, 2 columns

# Calculate percentages for clipped_roadkill
hist(clipped_roadkill$MEDIAN_TYPE, 
     main = "Clipped Roadkill MEDIAN_TYPE", 
     xlab = "MEDIAN_TYPE", 
     col = "lightblue", 
     border = "black", 
     freq = FALSE)  # Set freq = FALSE to plot density
# Add percentage scale
rug(clipped_roadkill$MEDIAN_TYPE)
y_vals <- seq(0, max(density(clipped_roadkill$MEDIAN_TYPE)$y), length.out = 100)
lines(density(clipped_roadkill$MEDIAN_TYPE), col = "blue", lwd = 2)
y_pct <- y_vals * 100 / sum(hist(clipped_roadkill$MEDIAN_TYPE, plot = FALSE)$counts)
lines(density(clipped_roadkill$MEDIAN_TYPE) * diff(hist(clipped_roadkill$MEDIAN_TYPE, plot = FALSE)$breaks)[1] * 100, col = "blue")

# Calculate percentages for highways
hist(highways$MEDIAN_TYPE, 
     main = "Highways MEDIAN_TYPE", 
     xlab = "MEDIAN_TYPE", 
     col = "lightgreen", 
     border = "black", 
     freq = FALSE)  # Set freq = FALSE to plot density
# Add percentage scale
rug(highways$MEDIAN_TYPE)
y_vals <- seq(0, max(density(highways$MEDIAN_TYPE)$y), length.out = 100)
lines(density(highways$MEDIAN_TYPE), col = "green", lwd = 2)
y_pct <- y_vals * 100 / sum(hist(highways$MEDIAN_TYPE, plot = FALSE)$counts)
lines(density(highways$MEDIAN_TYPE) * diff(hist(highways$MEDIAN_TYPE, plot = FALSE)$breaks)[1] * 100, col = "green")

# Reset to default single-panel plotting
par(mfrow = c(1, 1))

