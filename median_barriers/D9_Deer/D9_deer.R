library(dplyr)
library(sf)
library(readxl)
library(writexl)
library(lubridate)
library(ggplot2)

setwd("C:/Users/HP/Downloads/")

# Set paths
excel_file <- "C:\\Users\\HP\\Downloads\\Untreated Points Medians (4).xlsx"

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



# Read in deer shapefile
shp_file <- "C:\\Users\\HP\\Documents\\ArcGIS\\Projects\\District 9 Medians\\D9_deer\\D9_hwy_deer.shp"
deer_df <- st_read(shp_file)

# Frequency table of median types
deer_freq <- table(deer_df$MednTyp)
print(deer_freq)



# Assuming 'MedianType' in 'medians_df' contains roadkill data
roadkill_counts <- table(deer_df$MednTyp)

# Assuming 'MedianType' in 'cleaned_gdf' contains random points data
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
deer_contingency_table <- matrix(c(combined_counts$Freq.x, combined_counts$Freq.y), 
                            ncol = 2, 
                            byrow = FALSE,
                            dimnames = list(combined_counts$Var1, c("Deer", "Random")))

# Step 3: Perform the Chi-Squared Test
chi_squared_test <- chisq.test(deer_contingency_table)

# Print the results of the chi-squared test
print(chi_squared_test)

# 
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

deer_shp <- deer_df %>%
  select(!c("Sheet", "grp_nmb")) %>%
  rename(
    StreetImageDate = StrtImD,
    MedianType = MednTyp,
    SecondaryAttribute = ScndryA,
    MedianWidth = MdnWdth,
    RoadsideBarrier = RdsdBrr,
  ) %>%
  convert()

st_write(deer_shp, "D9_Deer.shp")
