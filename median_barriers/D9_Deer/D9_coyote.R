library(dplyr)
library(sf)
library(readxl)
library(lubridate)
library(ggplot2)

setwd("D:\\Median Barriers\\CROS Search\\District 9 Post-Processing Shapefiles\\D9_Coyote")

# Set paths
excel_file <- "C:\\Users\\HP\\Downloads\\Untreated Points Medians (3).xlsx"

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
  filter(!is.na(MedianType)) # Filter out NA's
  
# Frequency table of median types
medians_freq <- table(medians_df$MedianType)
print(medians_freq)


# Set paths
excel_file <- "C:\\Users\\HP\\Downloads\\Coyote CROS Medians.xlsx"

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
coyote_df <- bind_rows(data_frames, 
                        .id = "Sheet") %>%
  #select(-'Sheet') %>%
  filter(!is.na(MedianType)) # Filter out NA's

# Frequency table of median types
coyote_freq <- table(coyote_df$MedianType)
print(coyote_freq)



# Assuming 'MedianType' in 'medians_df' contains roadkill data
roadkill_counts <- table(coyote_df$MedianType)

# Assuming 'MednTyp' in 'cleaned_gdf' contains random points data
random_counts <- table(medians_df$MedianType)

# Combine both counts into a data frame for alignment
combined_counts <- merge(as.data.frame(roadkill_counts), as.data.frame(random_counts), 
                         by.x = "Var1", by.y = "Var1", all = TRUE)

# Replace NA with 0 (in case some median types are not present in one of the datasets)
combined_counts[is.na(combined_counts)] <- 0

# Filter out non-shared median types
combined_counts %>% 
  filter(!Var1 %in% c("overpass (open air)", "temp concrete", "thrie beam and veg"))

# Step 2: Create the contingency table
coyote_contingency_table <- matrix(c(combined_counts$Freq.x, combined_counts$Freq.y), 
                            ncol = 2, 
                            byrow = FALSE,
                            dimnames = list(combined_counts$Var1, c("Coyote", "Random")))

# Step 3: Perform the Chi-Squared Test
chi_squared_test <- chisq.test(coyote_contingency_table)

# Print the results of the chi-squared test
print(chi_squared_test)


# # Chi-Squared Test on No Median / Veg
# 
# combined_counts <- combined_counts %>%
#   filter(Var1 %in% c("no median", "vegetative"))
# # Step 2: Create the contingency table
# contingency_table <- matrix(c(combined_counts$Freq.x, combined_counts$Freq.y), 
#                             ncol = 2, 
#                             byrow = FALSE,
#                             dimnames = list(combined_counts$Var1, c("Roadkill", "Random")))
# # Step 3: Perform the Chi-Squared Test
# chi_squared_test <- chisq.test(contingency_table)
# # Print the results of the chi-squared test
# print(chi_squared_test)


# Merge shapefile and median data through common NID

shp_path <- "D://Median Barriers//CROS Search//D9 Pre-Processing Shapefiles//D9_coyote_1_145.shp"
shp_in <- st_read(shp_path)

# Rename common field for merge
coyote_df <- coyote_df %>%
  rename(nid = NID)

# Merge based on common field (nid)
coyote_gdf <- merge(shp_in, coyote_df, by="nid") #%>%
  mutate(as.integer(CHP))

shp_out <- coyote_gdf %>%
  select(!c("Sheet", "ORIG_FID")) %>%
  rename(
    # StreetImageDate = StrtImD,
    # MedianType = MednTyp,
    # SecondaryAttribute = ScndryA,
    # MedianWidth = MdnWdth,
    # RoadsideBarrier = RdsdBrr,
    MedianNotes = Notes
  ) #%>%
  mutate(as.integer(coyote_gdf$chps_Cd))

st_write(shp_out, "D9_Coyote.shp", layer_options = "FIELD_WIDTH=15")
