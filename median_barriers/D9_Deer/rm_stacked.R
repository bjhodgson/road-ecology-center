library(dplyr)
library(sf)
library(readxl)
library(lubridate)
library(ggplot2)

# Set paths
shp_file <- "D:\\Median Barriers\\CROS Search\\Output Data\\D9_deer_AADT\\d9_deer_AADT.shp"
excel_file <- "C:\\Users\\HP\\Downloads\\Deer CROS Medians (3).xlsx"

# Read in Excel sheets
sheets <- excel_sheets(excel_file)
# Filter out the sheets you want to exclude
selected_sheets <- sheets[!sheets %in% c("MedianTypes")]

# Read the selected sheets into a list of data frames
data_frames <- lapply(selected_sheets, function(sheet) {
  read_excel(excel_file, sheet = sheet)
}) %>%
  lapply(function(df) {
    df %>% # Convert type to character for binding
      mutate(StreetImageryDate = as.character(StreetImageryDate)) %>%
      mutate(StreetImageryDate = parse_date_time(StreetImageryDate, orders = "my")) %>%
      mutate(StreetImageryDate = as.character(StreetImageryDate))
    #mutate(StreetImageryDate = format(as.Date(StreetImageryDate, format = "%Y-%m-%d"), "%m/%Y"))
  })


# Combine all data frames into one using bind_rows from dplyr, adding a column for sheet name
medians_df <- bind_rows(data_frames, 
  .id = "Sheet") %>%
  #select(-'Sheet') %>%
  rename(nid = NID)

# Read shp into gdf
deer_gdf <- st_read(shp_file)

# Join medians df to deer gdf
joined_gdf <- merge(deer_gdf, medians_df, by = "nid", all = FALSE)

# Count occurrences of each latitude and longitude pair
grouped_gdf <- joined_gdf %>%
  group_by(latitude, longitude) %>%
  mutate(group_number = cur_group_id()) %>%
  ungroup()  

# Slice first record in each group
sliced_gdf <- grouped_gdf %>%
  group_by(latitude, longitude) %>%
  slice(1) %>%
  ungroup()

# Frequency table of median types
freq_table <- table(sliced_gdf$MedianType)
print(freq_table)
