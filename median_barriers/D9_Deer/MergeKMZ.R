library(writexl)
library(sf)
library(dplyr)

shp_file <- "C:\\Users\\HP\\Documents\\ArcGIS\\Projects\\District 9 Medians\\D9_deer\\D9_hwy_deer.shp"
deer_gdf <- st_read(shp_file)
deer_list <- deer_df %>%
  filter(NID %in% deer_gdf$nid)

df <- bind_rows(coyote_df, jackrabbit_df, deer_list) %>%
  select(!Sheet) %>%
  rename(MedianNotes = Notes,
         nid = NID)

excel_path <- "C:\\Users\\HP\\Downloads\\D9_CROS_Medians.xlsx"

write_xlsx(df, excel_path)
