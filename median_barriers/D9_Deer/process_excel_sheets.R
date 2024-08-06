library(readxl)
library(dplyr)
library(lubridate)

process_excel_sheets <- function(excel_file, output_df_name, exclude_sheets = c("MedianTypes")) {
  # Read in Excel sheets
  sheets <- excel_sheets(excel_file)
  
  # Filter out the sheets to exclude
  selected_sheets <- sheets[!sheets %in% exclude_sheets]
  
  # Read the selected sheets into a list of data frames
  data_frames <- lapply(selected_sheets, function(sheet) {
    read_excel(excel_file, sheet = sheet)
  }) %>%
    lapply(function(df) {
      df #%>% 
        # Convert type to character for binding
        #mutate(StreetImageryDate = as.character(StreetImageryDate)) %>%
        #mutate(StreetImageryDate = parse_date_time(StreetImageryDate, orders = "my")) %>%
        #mutate(StreetImageryDate = as.character(StreetImageryDate))
    }
    )
  
  # Combine all data frames into one using bind_rows from dplyr, adding a column for sheet name
  combined_df <- bind_rows(data_frames, 
                           .id = "Sheet") %>%
    filter(!is.na(MedianType)) # Filter out NA's
  
  # Assign the combined data frame to the specified variable name in the parent environment
  assign(output_df_name, combined_df, envir = .GlobalEnv)
}

# Example usage:
excel_file <- "C:\\Users\\HP\\Downloads\\Coyote CROS Medians.xlsx"
output_df_name <- "coyote_df"
process_excel_sheets(excel_file, output_df_name)
