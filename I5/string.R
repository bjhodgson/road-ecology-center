library(dplyr)

csv_path <- "C:\\Users\\HP\\Downloads\\EXPECTED SPECIES _Master_Lit_Search_Summary - Expected Species.csv"
  
csv <- read.csv(csv_path)

col_list <- c("Species", "Western.WA", "Northwestern.WA", "Southwestern.WA",  "Willamette.Valley", "Klamath.Siskiyou.Mountains",
                  "Sacramento.Valley", "Central.Valley", "Southern.CA")


# Use dplyr to select columns and filter rows with NA
rows_with_na <- csv %>%
  select(all_of(col_list)) %>%  # Select the columns specified in column_list
  filter(!if_any(everything(), is.na)) %>%  # Filter rows that have NA in any selected column
  filter(if_any(everything(), ~ grepl("\\(", .))) %>% # Filter rows that contain "(" in any selected column
  mutate(CommonName = Species) %>% 
  filter(if_all(Species, ~ !grepl("genus|species|Genus|Species", .))) %>%  # Filter out rows that contain "genus" or "species"
  filter(if_any(Species, ~ str_detect(., "\\("))) %>%  # Keep rows where any column contains '('
  mutate(across(Species, ~ str_extract(., "(?<=\\().+?(?=\\))")))  # Extract and remove parentheses


df <- rows_with_na[,c(10, 1, 2, 3, 4, 5, 6, 7, 8, 9)]
name_list <- as.data.frame(df[,c(1,2)])

out_path <- "D:\\I5\\SpeciesRasters\\SpeciesCodes.xlsx"
write_xlsx(name_list, path = out_path)
