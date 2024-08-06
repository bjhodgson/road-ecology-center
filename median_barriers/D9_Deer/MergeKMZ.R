library(writexl)
library(sf)
library(dplyr)

excel_file = "C:\\Users\\HP\\Downloads\\Deer CROS Medians (5).xlsx"
output_df_name = "deer_df_raw"
process_excel_sheets(excel_file, output_df_name)

shp_file <- "C:\\Users\\HP\\Documents\\ArcGIS\\Projects\\District 9 Medians\\D9_deer\\D9_hwy_deer.shp"
deer_gdf <- st_read(shp_file)
deer_df <- deer_df_raw %>%
  filter(deer_df_raw$NID %in% deer_gdf$nid)

df <- bind_rows(coyote_df, jackrabbit_df, deer_df) %>%
  select(!Sheet) %>%
  rename(MedianNotes = Notes,
         nid = NID
         )



#-----------


excel_path <- "C:\\Users\\HP\\Downloads\\D9_CROS_Medians.xlsx"
write_xlsx(df, excel_path)


# Read another shapefile to merge with
CROS_shp_file <- "C:\\Users\\HP\\Downloads\\CROS-CHIPS-20240410-ND-All (2)\\CROS-CHIPS-20240410-ND-All.shp"
CROS_gdf <- st_read(CROS_shp_file)

# Merge deer_df with other_gdf based on NID attribute
merged_gdf <- inner_join(CROS_gdf, df, by = c("nid" = "nid"))

species_df <- merged_gdf %>%
  select(nid, 
         animal, 
         StreetImageryDate, 
         MedianType, 
         SecondaryAttribute,
         MedianWidth,
         RoadsideBarrier,
         MedianNotes) %>%
  st_drop_geometry()

# Write to excel
excel_output <- "C:\\Users\\HP\\Downloads\\CROS_Medians\\CROS_Medians.xlsx"
write_xlsx(species_df, excel_output)

# Optionally, save the merged shapefile
CROS_shp_file <- "C:\\Users\\HP\\Downloads\\CROS_Medians\\CROS_Medians.shp"
st_write(merged_gdf, CROS_shp_file)



# ---------------------

excel_file <- "C:\\Users\\HP\\Downloads\\Untreated Points Medians (5).xlsx"
output_df_name <- "random_df"
process_excel_sheets(excel_file, output_df_name) %>%
  select(-"Sheet")

# Write to excel
excel_output <- "C:\\Users\\HP\\Downloads\\CROS_Medians\\Untreated_Medians_07-31-24.xlsx"
random_df %>% 
  rename(cid = CID) %>%
  select(-c("Sheet", "...8")) %>%
  write_xlsx(excel_output)
#write_xlsx(random_df, excel_output)
