library(dplyr)
library(sf)
library(readxl)

# Set paths
shp_file <- "D:\\Median Barriers\\CROS Search\\Output Data\\D9_deer\\d9_deer.shp"
excel_file <- "C:\\Users\\HP\\Downloads\\Deer CROS Medians.xlsx"

# Read shp into gdf
deer_gdf <- st_read(shp_file)

# Read in Excel sheets
sheets <- excel_sheets(excel_file)
# Filter out the sheets you want to exclude
selected_sheets <- sheets[sheets %in% c("1_823", "2472_3295", "3296_4119", "4944_5767", "5768_6591")]

# Read the selected sheets into a list of data frames
data_frames <- lapply(selected_sheets, function(sheet) read_excel(excel_file, sheet = sheet))


# Combine all data frames into one using bind_rows from dplyr, adding a column for sheet name
medians_df <- bind_rows(data_frames, 
                        .id = "Sheet") %>%
  select(-'Sheet') %>%
  rename(nid = NID)

# Join medians df to deer gdf
joined_gdf <- merge(deer_gdf, medians_df, by = "nid", all = FALSE)

# Create frequency table
freq_table <- table(joined_gdf$MedianType)

# Print the frequency table
print(freq_table)

# Perform chi-squared test
chi_square_test <- chisq.test(freq_table)

# Print the chi-squared test results
print(chi_square_test)
