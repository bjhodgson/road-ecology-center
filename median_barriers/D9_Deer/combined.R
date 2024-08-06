library(readxl)
library(sf)
library(dplyr)

# Read in Excel files
path <- "C:\\Users\\HP\\Downloads\\Deer CROS Medians (6).xlsx"
process_excel_sheets(path, "deer_excel")
path <- "C:\\Users\\HP\\Downloads\\Coyote CROS Medians (2).xlsx"
process_excel_sheets(path, "coyote_excel")
path <- "C:\\Users\\HP\\Downloads\\Jackrabbit CROS Medians (2).xlsx"
process_excel_sheets(path, "jackrabbit_excel")
path <- "C:\\Users\\HP\\Downloads\\Untreated Points Medians (6).xlsx"
process_excel_sheets(path, "random_excel")

# Read in shapefiles
shp_path <- "D:\\Median Barriers\\D9 Shapefiles\\Mule Deer\\D9_deer_hwys.shp"
deer_gdf <- st_read(shp_path)
shp_path <- "D:\\Median Barriers\\D9 Shapefiles\\Coyote\\D9_coyote_1_145.shp"
coyote_gdf <- st_read(shp_path)
shp_path <- "D:\\Median Barriers\\D9 Shapefiles\\Jackrabbit\\D9_jackrabbit_1_138.shp"
jackrabbit_gdf <- st_read(shp_path)
shp_path <- "D:\\Median Barriers\\D9 Shapefiles\\Random Points\\random_points.shp"
random_gdf <- st_read(shp_path)

# Remove stacked points from deer gdf
# Count occurrences of each latitude and longitude pair
grouped_deer_gdf <- deer_gdf %>%
  group_by(latitude, longitude) %>%
  mutate(group_number = cur_group_id()) %>%
  ungroup()  

# Slice first record in each group
cleaned_deer_gdf <- grouped_deer_gdf %>%
  group_by(latitude, longitude) %>%
  slice(1) %>%
  ungroup() %>%
  select(!"group_number")

# Bind dfs
df <- bind_rows(coyote_excel, jackrabbit_excel, deer_excel) %>%
  select(!Sheet) %>%
  rename(MedianNotes = Notes,
         nid = NID
  )

# Bind gdfs
gdf <- bind_rows(coyote_gdf, jackrabbit_gdf, cleaned_deer_gdf) #%>%
  select(!ORIG_FID)
  
# Merge attributes
merged_gdf <- inner_join(gdf, df, by = "nid") %>%
  select(!"ORIG_FID")

# Write merged gdf to shp
out_path <- "D:\\Median Barriers\\D9 Shapefiles\\Combined Dataset\\D9_CROS_Medians.shp"
st_write(merged_gdf, out_path)
  
# Bind random excel to random gdf
merged_random_gdf <- inner_join(random_gdf, random_excel, by = "CID") %>%
  select(!c("LATITUDE", "LONGITUDE", "Sheet")) %>%
  rename(MedianNotes = Notes,
         FID = CID
  )
  
# Write random merged gdf to shp  
out_path <- "D:\\Median Barriers\\D9 Shapefiles\\Random Points\\D9_Random_Medians.shp"
st_write(merged_random_gdf, out_path)  
