# ==============================
# Function: process_xlsx
# ==============================
# Purpose:
#   Process and combine specified sheets from an Excel (XLSX) file into a 
#   single data frame while excluding certain sheets. The combined data frame 
#   is then assigned to a variable in the global environment.
#
# Parameters:
#   xlsx_file: A string indicating the path to the Excel file to be processed.
#   output_df_name: A string for the name of the output data frame to be created.
#   exclude_sheets: A character vector of sheet names to exclude from processing.
#                    Defaults to "MedianTypes".
#
# Returns:
#   None. The function assigns the combined data frame to the specified name in the global environment.
#
# Example Usage:
#   xlsx_file <- "C:\\Users\\HP\\Downloads\\Coyote CROS Medians.xlsx"
#   output_df_name <- "coyote_df"
#   process_xlsx_sheets(xlsx_file, output_df_name)
#
# ==============================

process_xlsx <- function(xlsx_file, output_df_name, exclude_sheets = c("MedianTypes")) {
  # Read in xlsx sheets
  sheets <- excel_sheets(xlsx_file)
  
  # Filter out the sheets to exclude
  selected_sheets <- sheets[!sheets %in% exclude_sheets]
  
  # Read the selected sheets into a list of data frames
  data_frames <- lapply(selected_sheets, function(sheet) {
    read_excel(xlsx_file, sheet = sheet)
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
# xlsx_file <- "C:\\Users\\HP\\Downloads\\Coyote CROS Medians.xlsx"
# output_df_name <- "coyote_df"
# process_xlsx_sheets(xlsx_file, output_df_name)
