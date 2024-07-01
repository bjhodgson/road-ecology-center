library(dplyr)
library(sf)
library(readxl)
library(lubridate)

# Set paths
shp_file <- "D:\\Median Barriers\\CROS Search\\Output Data\\D9_deer_AADT\\d9_deer_AADT.shp"
excel_file <- "C:\\Users\\HP\\Downloads\\Deer CROS Medians (1).xlsx"

# Read shp into gdf
deer_gdf <- st_read(shp_file)

# Read in Excel sheets
sheets <- excel_sheets(excel_file)
# Filter out the sheets you want to exclude
selected_sheets <- sheets[!sheets %in% c("MedianTypes", "1648_2471")]

# Read the selected sheets into a list of data frames
data_frames <- lapply(selected_sheets, function(sheet) {
  read_excel(excel_file, sheet = sheet)
}) %>%
  lapply(function(df) {
    df %>% # Convert type to character for binding
      mutate(StreetImageryDate = as.character(StreetImageryDate)) %>%
      mutate(StreetImageryDate = parse_date_time(StreetImageryDate, orders = "my"))
    #mutate(StreetImageryDate = format(as.Date(StreetImageryDate, format = "%Y-%m-%d"), "%m/%Y"))
  })


# Combine all data frames into one using bind_rows from dplyr, adding a column for sheet name
medians_df <- bind_rows(data_frames, 
                        .id = "Sheet") %>%
  select(-'Sheet') %>%
  rename(nid = NID) #%>%
  mutate(StreetImageryDate = format(as.Date(StreetImageryDate, format = "%Y-%m-%d"), "%m/%Y"))
  
joined_gdf <- medians_df  

# Join medians df to deer gdf
joined_gdf <- merge(deer_gdf, medians_df, by = "nid", all = FALSE)

# Create frequency table
freq_table <- table(joined_gdf$MedianType)
# Print the frequency table
print(freq_table)

# Create a bar plot
barplot(freq_table,
        main = "Frequency of Median Types in Roadkill Observations",
        #xlab = "Median Types",
        ylab = "Frequency",
        ) 

# Perform chi-squared test
chi_square_test <- chisq.test(freq_table)
# Print the chi-squared test results
print(chi_square_test)





plot(density(joined_gdf$AADT))

modelmodelmodel <- glm(roadkill_freq ~ continuous_var + covariate1 + covariate2, family = poisson, data = your_data)

reg1 <- lm()
